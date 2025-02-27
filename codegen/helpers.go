package codegen

import (
	"cool-compiler/ast"
	"fmt"
	"strings"
	"unicode"
	"sort"	
	"github.com/llir/llvm/ir/enum"

	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/types"
	"github.com/llir/llvm/ir/value"
)

// Resolve the receiver object and type for a method call
func (g *CodeGenerator) resolveMethodReceiver(block *ir.Block, e *ast.MethodCall) (receiverObj value.Value, receiverType string, resultBlock *ir.Block, err error) {
	resultBlock = block
	
	if e.Object == nil {
		// Implicit "self" receiver
		receiverObj = g.locals["$self"]
		receiverType = g.currentClass
		return
	}
	
	// Generate code for the receiver expression
	receiverObj, resultBlock, err = g.generateExpression(resultBlock, e.Object)
	if err != nil {
		return
	}
	
	if e.Type != nil {
		receiverType = e.Type.Value
		// Cast receiver to static type's class
		if classType, exists := g.classTypes[receiverType]; exists {
			receiverObj = resultBlock.NewBitCast(receiverObj, types.NewPointer(classType))
		} else {
			err = fmt.Errorf("static type %s not found", receiverType)
			return
		}
	}

	// Determine receiver type based on expression
	switch expr := e.Object.(type) {
	case *ast.NewExpression:
		receiverType = expr.Type.Value
	case *ast.ObjectIdentifier:
		// First check if it's a class attribute
		if classInfo, exists := g.classTable[g.currentClass]; exists {
			for _, attr := range classInfo.Attributes {
				if attr.Name == expr.Value {
					receiverType = attr.Type
					break
				}
			}
		}

		// If not found as attribute, check local variables
		if receiverType == "" {
			var exists bool
			receiverType, exists = g.localsTypes[expr.Value]
			if !exists {
				err = fmt.Errorf("undefined variable: %s", expr.Value)
				return
			}
		}
	default:
		err = fmt.Errorf("unsupported receiver expression: %T", e.Object)
		return
	}
	
	return
}

// Handle special methods (type_name, copy)
func (g *CodeGenerator) handleSpecialMethods(block *ir.Block, e *ast.MethodCall, receiverObj value.Value, receiverType string) (result value.Value, resultBlock *ir.Block, handled bool, err error) {
	resultBlock = block
	
	// Handle type_name method
	if e.Method.Value == "type_name" {
		result, resultBlock, err = g.generateTypeNameMethod(block, e, receiverType)
		handled = true
		return
	}
	
	// Handle copy method
	if e.Method.Value == "copy" {
		methodName := fmt.Sprintf("%s_copy", receiverType)
		methodFunc := g.methods[methodName]
		if methodFunc == nil {
			err = fmt.Errorf("method %s not found", methodName)
			handled = true
			return
		}
		result = receiverObj
		handled = true
		return
	}
	
	handled = false
	return
}

// Generate code for the type_name method
func (g *CodeGenerator) generateTypeNameMethod(block *ir.Block, e *ast.MethodCall, receiverType string) (value.Value, *ir.Block, error) {
	// For primitive types (Bool, Int), use Object's type_name implementation
	methodName := "Object_type_name"
	methodFunc := g.methods[methodName]

	if methodFunc == nil {
		return nil, block, fmt.Errorf("method %s not found", methodName)
	}

	// Get the type name string
	typeNameStr := g.getOrCreateStringConstant(receiverType)

	// Create new String object
	stringType := g.classTypes["String"]
	stringObj := block.NewAlloca(stringType)

	// Set vtable for String object
	stringVtablePtr := block.NewGetElementPtr(stringType, stringObj,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0),
	)
	block.NewStore(
		block.NewBitCast(g.vtables["String"], i8Ptr),
		stringVtablePtr,
	)

	// Set string value (type name)
	valuePtr := block.NewGetElementPtr(stringType, stringObj,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 1),
	)

	var strPtr value.Value
	if arrType, ok := typeNameStr.Type().(*types.ArrayType); ok {
		strPtr = block.NewGetElementPtr(arrType, typeNameStr,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, 0),
		)
	} else {
		strPtr = block.NewBitCast(typeNameStr, i8Ptr)
	}
	block.NewStore(strPtr, valuePtr)


	// Return the String object
	return stringObj, block, nil
}


// Generate code for regular method calls
func (g *CodeGenerator) generateRegularMethodCall(block *ir.Block, e *ast.MethodCall, receiverObj value.Value, receiverType string) (value.Value, *ir.Block, error) {
	methodName := fmt.Sprintf("%s_%s", receiverType, e.Method.Value)
	methodFunc := g.methods[methodName]

	// If method not found in current class, look in parent class (IO)
	if methodFunc == nil && receiverType == "Main" {
		methodName = fmt.Sprintf("IO_%s", e.Method.Value)
		methodFunc = g.methods[methodName]
	}

	if methodFunc == nil {
		return nil, block, fmt.Errorf("method %s not found", methodName)
	}

	// Generate arguments
	args, newBlock, err := g.generateMethodCallArguments(block, e, receiverObj, receiverType)
	if err != nil {
		return nil, newBlock, err
	}
	block = newBlock
	
	// Call the method
	result := block.NewCall(methodFunc, args...)
	if result == nil {
		return nil, block, fmt.Errorf("failed to generate call to %s", methodName)
	}
	
	// Handle void returns
	if result.Type() == types.Void {
		// For void returns, add unreachable if needed
		if block.Term == nil {
			block.NewUnreachable()
		}
	}
	
	return result, block, nil
}

// Generate arguments for a method call
func (g *CodeGenerator) generateMethodCallArguments(block *ir.Block, e *ast.MethodCall, receiverObj value.Value, receiverType string) ([]value.Value, *ir.Block, error) {
	args := make([]value.Value, 0, len(e.Arguments)+1)
	
	// Add receiver as first argument
	if receiverType == "Int" || receiverType == "Bool" {
		args = append(args, receiverObj)
	} else {
		args = append(args, block.NewBitCast(receiverObj, types.I8Ptr))
	}
	
	// Generate code for each argument
	for _, arg := range e.Arguments {
		argVal, newBlock, err := g.generateExpression(block, arg)
		if err != nil {
			return nil, newBlock, err
		}
		block = newBlock
		
		// Cast if needed
		if ptrType, ok := argVal.Type().(*types.PointerType); ok {
			if _, isObject := g.classTypes[g.getClassNameFromType(ptrType)]; isObject {
				argVal = block.NewBitCast(argVal, types.I8Ptr)
			}
		}
		
		args = append(args, argVal)
	}
	
	return args, block, nil
}

// Helper function to box primitive types
func (g *CodeGenerator) boxPrimitiveIfNeeded(block *ir.Block, val value.Value) (value.Value, *ir.Block, error) {
	if val.Type() == types.I32 {
		return g.boxInt(block, val)
	} else if val.Type() == types.I1 {
		return g.boxBool(block, val)
	} else if ptrType, isPtrType := val.Type().(*types.PointerType); isPtrType {
		if elemType, isIntPtr := ptrType.ElemType.(*types.IntType); isIntPtr {
			// Load the value first
			loadedValue := block.NewLoad(elemType, val)
			
			if elemType.BitSize == 32 {
				return g.boxInt(block, loadedValue)
			} else if elemType.BitSize == 1 {
				return g.boxBool(block, loadedValue)
			}
		}
	}
	
	return val, block, nil
}

// Box an integer value
func (g *CodeGenerator) boxInt(block *ir.Block, intVal value.Value) (value.Value, *ir.Block, error) {
	intType := g.classTypes["Int"]
	if intType == nil {
		return nil, block, fmt.Errorf("int type not found in class types")
	}

	intObj := block.NewAlloca(intType)

	// Set vtable
	vtablePtr := block.NewGetElementPtr(intType, intObj,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0),
	)

	if g.vtables["Int"] == nil {
		return nil, block, fmt.Errorf("int vtable not found")
	}

	block.NewStore(
		block.NewBitCast(g.vtables["Int"], i8Ptr),
		vtablePtr,
	)

	// Set value field
	valuePtr := block.NewGetElementPtr(intType, intObj,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 1),
	)
	block.NewStore(intVal, valuePtr)

	// Return boxed object
	return block.NewBitCast(intObj, i8Ptr), block, nil
}

// Box a boolean value
func (g *CodeGenerator) boxBool(block *ir.Block, boolVal value.Value) (value.Value, *ir.Block, error) {
	boolType := g.classTypes["Bool"]
	if boolType == nil {
		return nil, block, fmt.Errorf("bool type not found in class types")
	}

	boolObj := block.NewAlloca(boolType)

	// Set vtable
	vtablePtr := block.NewGetElementPtr(boolType, boolObj,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0),
	)

	if g.vtables["Bool"] == nil {
		return nil, block, fmt.Errorf("bool vtable not found")
	}

	block.NewStore(
		block.NewBitCast(g.vtables["Bool"], i8Ptr),
		vtablePtr,
	)

	// Set value field
	valuePtr := block.NewGetElementPtr(boolType, boolObj,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 1),
	)
	
	fieldType := boolType.Fields[1]
	if fieldType.Equal(types.I32) {
		// If Bool.val is defined as i32, extend the i1 value
		extendedBool := block.NewZExt(boolVal, types.I32)
		block.NewStore(extendedBool, valuePtr)
	} else {
		// If Bool.val is defined as i1, store directly
		block.NewStore(boolVal, valuePtr)
	}

	// Return boxed object
	return block.NewBitCast(boolObj, i8Ptr), block, nil
}

// Setup null check for an object
func (g *CodeGenerator) setupNullCheck(block *ir.Block, val value.Value) (*ir.Block, *ir.Block, error) {
	nonNullBlock := block.Parent.NewBlock("case.non_null")
	abortBlock := block.Parent.NewBlock("case.abort")
	
	// Get abort function
	var abortFunc *ir.Func
	for _, f := range g.module.Funcs {
		if f.Name() == "abort" {
			abortFunc = f
			break
		}
	}
	if abortFunc == nil {
		abortFunc = g.module.NewFunc("abort", types.Void)
	}
	
	abortBlock.NewCall(abortFunc)
	abortBlock.NewUnreachable()

	// Generate null check branch
	if ptrType, ok := val.Type().(*types.PointerType); ok {
		nullPtr := constant.NewNull(ptrType)
		isNull := block.NewICmp(enum.IPredEQ, val, nullPtr)
		block.NewCondBr(isNull, abortBlock, nonNullBlock)
	} else {
		block.NewBr(nonNullBlock)
	}
	
	return nonNullBlock, abortBlock, nil
}

// Find an Object case in the list of cases
func (g *CodeGenerator) findObjectCase(cases []*ast.Case) *ast.Case {
	for _, c := range cases {
		if c.Type.Value == "Object" {
			return c
		}
	}
	return nil
}

// Filter out non-Object cases
func (g *CodeGenerator) filterNonObjectCases(cases []*ast.Case) []*ast.Case {
	var nonObjectCases []*ast.Case
	for _, c := range cases {
		if c.Type.Value != "Object" {
			nonObjectCases = append(nonObjectCases, c)
		}
	}
	return nonObjectCases
}

// Create test blocks for each case
func (g *CodeGenerator) createTestBlocks(block *ir.Block, count int) []*ir.Block {
	var testBlocks []*ir.Block
	for i := 0; i < count; i++ {
		testBlocks = append(testBlocks, block.Parent.NewBlock(fmt.Sprintf("case.test.%d", i)))
	}
	return testBlocks
}

// Handle primitive type cases
func (g *CodeGenerator) handlePrimitiveCases(cases []*ast.Case, testBlocks []*ir.Block, 
	objectCaseBlock *ir.Block, target value.Value, targetType string, 
	endBlock *ir.Block, incoming []*ir.Incoming) []*ir.Incoming {
	
	for i, caseItem := range cases {
		currentBlock := testBlocks[i]
		caseBlock := currentBlock.Parent.NewBlock(fmt.Sprintf("case.%d.body", i))

		// Next block is either the next test or the object case
		nextBlock := objectCaseBlock
		if i < len(cases)-1 {
			nextBlock = testBlocks[i+1]
		}

		// Compare type IDs directly
		typeID := g.typeID(caseItem.Type.Value)
		targetTypeID := g.typeID(targetType)
		cmp := currentBlock.NewICmp(
			enum.IPredEQ,
			constant.NewInt(types.I32, int64(targetTypeID)),
			constant.NewInt(types.I32, int64(typeID)),
		)

		currentBlock.NewCondBr(cmp, caseBlock, nextBlock)

		// Handle case body
		caseResult, bodyBlock, err := g.handleSingleCase(caseBlock, caseItem, target, caseItem.Type.Value)
		if err != nil {
			// Just skip this case if there's an error
			continue
		}

		bodyBlock.NewBr(endBlock)
		incoming = append(incoming, ir.NewIncoming(caseResult, bodyBlock))
	}
	
	return incoming
}

// Handle object type cases
func (g *CodeGenerator) handleObjectCases(cases []*ast.Case, testBlocks []*ir.Block, 
	objectCaseBlock *ir.Block, target value.Value, 
	endBlock *ir.Block, incoming []*ir.Incoming) []*ir.Incoming {
	
	// Get the vtable pointer from the object
	currentBlock := testBlocks[0]
	classType := types.NewStruct(types.NewPointer(types.I8)) // Generic object type with vtable ptr
	castedTarget := currentBlock.NewBitCast(target, types.NewPointer(classType))
	vtablePtr := currentBlock.NewGetElementPtr(classType, castedTarget,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0),
	)
	vtablePtrVal := currentBlock.NewLoad(types.NewPointer(types.I8), vtablePtr)

	// Sort non-object cases by inheritance depth (most specific first)
	sortedCases := make([]*ast.Case, len(cases))
	copy(sortedCases, cases)
	sort.Slice(sortedCases, func(i, j int) bool {
		return g.getTypeDepth(sortedCases[i].Type.Value) > g.getTypeDepth(sortedCases[j].Type.Value)
	})

	for i, caseItem := range sortedCases {
		currentBlock = testBlocks[i]
		
		// Setup blocks for this case
		loopBlock := currentBlock.Parent.NewBlock(fmt.Sprintf("case.%d.loop", i))
		matchBlock := currentBlock.Parent.NewBlock(fmt.Sprintf("case.%d.match", i))

		// Next block is either the next test or the object case
		nextBlock := objectCaseBlock
		if i < len(sortedCases)-1 {
			nextBlock = testBlocks[i+1]
		}

		// Create inheritance chain traversal
		incoming = g.setupInheritanceTraversal(
			currentBlock, loopBlock, matchBlock, nextBlock, 
			vtablePtrVal, caseItem, target, endBlock, i, incoming)
	}
	
	return incoming
}

// Setup inheritance chain traversal for object case matching
func (g *CodeGenerator) setupInheritanceTraversal(
	currentBlock, loopBlock, matchBlock, nextBlock *ir.Block,
	vtablePtrVal value.Value, caseItem *ast.Case, target value.Value,
	endBlock *ir.Block, caseIndex int, incoming []*ir.Incoming) []*ir.Incoming {
	
	// Start with current vtable and traverse the inheritance chain
	currentBlock.NewBr(loopBlock)

	// Loop block for inheritance traversal
	vtablePhi := loopBlock.NewPhi(ir.NewIncoming(vtablePtrVal, currentBlock))

	// Access vtable header to get class name
	vtableHeaderType := types.NewStruct(
		types.NewPointer(types.I8), // class name
		types.NewPointer(types.I8), // parent vtable
	)
	castedVtable := loopBlock.NewBitCast(vtablePhi, types.NewPointer(vtableHeaderType))

	// Get class name from vtable
	classNamePtrPtr := loopBlock.NewGetElementPtr(vtableHeaderType, castedVtable,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0),
	)
	classNamePtr := loopBlock.NewLoad(types.NewPointer(types.I8), classNamePtrPtr)

	// Compare with case type using strcmp
	typeStrConst := g.getOrCreateStringConstant(caseItem.Type.Value)
	typeStrPtr := loopBlock.NewGetElementPtr(
		types.NewArray(uint64(len(caseItem.Type.Value)+1), types.I8),
		typeStrConst,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0),
	)

	// Get or declare strcmp
	strcmpFunc := g.getOrCreateStrcmp()
	// Call strcmp
	cmpResult := loopBlock.NewCall(strcmpFunc, classNamePtr, typeStrPtr)
	isMatch := loopBlock.NewICmp(enum.IPredEQ, cmpResult, constant.NewInt(types.I32, 0))

	// Check if we have a match
	continueBlock := loopBlock.Parent.NewBlock(fmt.Sprintf("case.%d.continue", caseIndex))
	loopBlock.NewCondBr(isMatch, matchBlock, continueBlock)

	// If no match, check parent vtable
	parentVtablePtrPtr := loopBlock.NewGetElementPtr(vtableHeaderType, castedVtable,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 1),
	)
	parentVtablePtr := continueBlock.NewLoad(types.NewPointer(types.I8), parentVtablePtrPtr)

	// Check if we've reached Object or null (end of inheritance chain)
	isNull := continueBlock.NewICmp(enum.IPredEQ, parentVtablePtr,
		constant.NewNull(types.NewPointer(types.I8)))
	isObjectVtable := continueBlock.NewICmp(enum.IPredEQ, parentVtablePtr,
		continueBlock.NewBitCast(g.vtables["Object"], i8Ptr))
	isEnd := continueBlock.NewOr(isNull, isObjectVtable)

	// Branch based on whether we've reached the end
	continueBlock.NewCondBr(isEnd, nextBlock, loopBlock)

	// Update the vtable phi node with the parent vtable
	vtablePhi.Incs = append(vtablePhi.Incs, ir.NewIncoming(parentVtablePtr, continueBlock))

	// Handle the actual case body
	caseResult, bodyBlock, err := g.handleSingleCase(matchBlock, caseItem, target, caseItem.Type.Value)
	if err == nil {
		bodyBlock.NewBr(endBlock)
		incoming = append(incoming, ir.NewIncoming(caseResult, bodyBlock))
	}
	
	return incoming
}

// Handle a single case expression
func (g *CodeGenerator) handleSingleCase(block *ir.Block, caseItem *ast.Case, 
	target value.Value, typeName string) (value.Value, *ir.Block, error) {
	
	// Save old locals
	oldLocals := make(map[string]value.Value)
	oldTypes := make(map[string]string)
	localName := caseItem.Name.Value

	// Create a local variable for the case branch
	if existing, ok := g.locals[localName]; ok {
		oldLocals[localName] = existing
	}
	if existingType, ok := g.localsTypes[localName]; ok {
		oldTypes[localName] = existingType
	}

	// Set new local
	varAlloca := block.NewAlloca(target.Type())
	block.NewStore(target, varAlloca)
	g.locals[localName] = varAlloca
	g.localsTypes[localName] = typeName

	// Generate case body
	caseResult, bodyBlock, err := g.generateExpression(block, caseItem.Expression)
	if err != nil {
		return nil, bodyBlock, err
	}

	// Restore previous locals
	if oldVal, exists := oldLocals[localName]; exists {
		g.locals[localName] = oldVal
	} else {
		delete(g.locals, localName)
	}
	if oldType, exists := oldTypes[localName]; exists {
		g.localsTypes[localName] = oldType
	} else {
		delete(g.localsTypes, localName)
	}

	return caseResult, bodyBlock, nil
}

func  (g *CodeGenerator) getOrCreateStrlen() *ir.Func { 
	strlenName := "strlen"
	for _, f := range g.module.Funcs {
		if f.Name() == strlenName  {
			 return f
		}
	}
	return  g.module.NewFunc(strlenName, types.I64,
			ir.NewParam("str", i8Ptr))

}
func (g *CodeGenerator) getOrCreateStrcmp() *ir.Func { 
	strcmpName := "strcmp"
	for _, f := range g.module.Funcs { 
		if f.Name() == strcmpName { 
			return f 
			} 
		} 
	return g.module.NewFunc(strcmpName, types.I32,
	ir.NewParam("s1", i8Ptr),
	ir.NewParam("s2", i8Ptr),
)
}
func (g *CodeGenerator) getOrCreateStrchr() *ir.Func { 
	strchrName := "strchr"
	for _, f := range g.module.Funcs { 
		if f.Name() == strchrName { 
			return f 
			} 
		} 
	return g.module.NewFunc(strchrName, types.I8Ptr,
		ir.NewParam("s", types.I8Ptr),
		ir.NewParam("c", types.I32),
	)
} 

func (g *CodeGenerator) getOrCreatePrintf() *ir.Func { 
	printfName := "printf"
	for _, f := range g.module.Funcs { 
		if f.Name() == printfName { 
			return f 
			} 
		} 
	printf := g.module.NewFunc("printf", types.I32, ir.NewParam("format", i8Ptr))
	printf.Sig.Variadic = true
	return printf
} 

func (g *CodeGenerator) getOrCreateAbort() *ir.Func { 
	abortName := "abort"
	for _, f := range g.module.Funcs { 
		if f.Name() == abortName { 
			return f 
			} 
		} 
	return g.module.NewFunc(abortName, types.Void)
} 

func (g *CodeGenerator) getOrCreateMemcpy() *ir.Func { 
	memcpyName := "memcpy"
	for _, f := range g.module.Funcs { 
		if f.Name() == memcpyName { 
			return f 
			} 
		} 
	return g.module.NewFunc(memcpyName, types.I8Ptr,
		ir.NewParam("dest", types.I8Ptr),
		ir.NewParam("src", types.I8Ptr),
		ir.NewParam("len", types.I64))
} 


func (g *CodeGenerator) getOrCreateFgets(fileType *types.StructType ) *ir.Func { 
	fgetsName := "fgets"
	for _, f := range g.module.Funcs { 
		if f.Name() == fgetsName { 
			return f 
			} 
		} 
	return g.module.NewFunc(fgetsName, types.I8Ptr,
		ir.NewParam("s", types.I8Ptr),
		ir.NewParam("size", types.I32),
		ir.NewParam("stream", types.NewPointer(fileType)),
	)
} 


 

func (g *CodeGenerator) getOrCreateMemset() *ir.Func { 
	memsetName := "llvm.memset.p0i8.i64" 
	for _, f := range g.module.Funcs { 
		if f.Name() == memsetName { 
			return f 
			} 
		} 
	// Create the memset intrinsic if it wasn't found. 
	return  g.module.NewFunc(memsetName, types.Void,
	ir.NewParam("dst", i8Ptr),
	ir.NewParam("val", types.I8),
	ir.NewParam("len", types.I64),
	ir.NewParam("align", types.I32),
	ir.NewParam("isvolatile", types.I1))
}
func (g *CodeGenerator)isBuiltin(typ types.Type) bool {
	 // Depending on your LLVM type mapping for COOL: 
	 // For example, assume Int is I32 and Bool is I1. 
	 if typ.Equal(types.I32) || typ.Equal(types.I1) { 
		return true } 
		if typ.Equal(g.classTypes["String"]) { 
			return true 
			} 
			return false }

// getClassNameFromType returns the class name for a given LLVM type (if it's a class struct pointer)
func (g *CodeGenerator) getClassNameFromType(t types.Type) string {
	ptrType, ok := t.(*types.PointerType)
	if !ok {
		return ""
	}
	structType, ok := ptrType.ElemType.(*types.StructType)
	if !ok {
		return ""
	}
	for className, classType := range g.classTypes {
		if classType.Equal(structType) {
			return className
		}
	}
	return ""
}

func (g *CodeGenerator) getOrCreateMalloc() *ir.Func {
	// Look for existing malloc function
	for _, f := range g.module.Funcs {
		if f.Name() == "malloc" {
			return f
		}
	}
	// Create new malloc function if not found
	return g.module.NewFunc("malloc", types.I8Ptr, ir.NewParam("size", types.I64))
}
func (g *CodeGenerator) initializeAttribute(block *ir.Block, attrPtr value.Value, attrType string, initExpr ast.Expression) (*ir.Block, error) {

	// Get the destination type
	destType := attrPtr.Type().(*types.PointerType).ElemType

	if initExpr != nil {
		val, currentBlock, err := g.generateExpression(block, initExpr)
		if err != nil {
			return currentBlock, err
		}

		// Handle String type specially
		if g.getClassNameFromType(destType) == "String" {
			stringType := g.classTypes["String"]

			// If we have a raw i8* pointer, wrap it in a String object
			if val.Type().Equal(types.I8Ptr) {
				// Create new String object
				newString := currentBlock.NewAlloca(stringType)

				// Set vtable
				vtablePtr := currentBlock.NewGetElementPtr(stringType, newString,
					constant.NewInt(types.I32, 0),
					constant.NewInt(types.I32, 0),
				)
				currentBlock.NewStore(
					currentBlock.NewBitCast(g.vtables["String"], types.I8Ptr),
					vtablePtr,
				)

				// Set value field
				valuePtr := currentBlock.NewGetElementPtr(stringType, newString,
					constant.NewInt(types.I32, 0),
					constant.NewInt(types.I32, 1),
				)
				currentBlock.NewStore(val, valuePtr)

				val = newString
			}

			// Cast to destination type if needed
			if !val.Type().Equal(destType) {
				val = currentBlock.NewBitCast(val, destType)
			}

			currentBlock.NewStore(val, attrPtr)
			return currentBlock, nil
		}

		currentBlock.NewStore(val, attrPtr)
		return currentBlock, nil
	}

	// Default initialization
	var defaultVal value.Value
	switch strings.ToLower(attrType) {
	case "int":

		defaultVal = constant.NewInt(types.I32, 0)
	case "bool":
		defaultVal = constant.NewInt(types.I1, 0)
	case "string":
		// Create empty string
		stringType := g.classTypes["String"]
		obj := block.NewAlloca(stringType)

		// Set vtable
		vtablePtr := block.NewGetElementPtr(stringType, obj,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, 0),
		)
		block.NewStore(
			block.NewBitCast(g.vtables["String"], i8Ptr),
			vtablePtr,
		)

		// Set empty string value
		emptyStr := g.getOrCreateStringConstant("\x00")
		strPtr := block.NewGetElementPtr(
			types.NewArray(1, types.I8),
			emptyStr,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, 0),
		)
		defaultVal = strPtr

	default:
		if classType, exists := g.classTypes[attrType]; exists {
			defaultVal = constant.NewNull(types.NewPointer(classType))
		} else {
			defaultVal = constant.NewNull(types.NewPointer(types.I8))
		}
	}

	// Cast the default value to the correct type if needed
	destPtrType := attrPtr.Type().(*types.PointerType)
	if !defaultVal.Type().Equal(destPtrType.ElemType) {
		defaultVal = block.NewBitCast(defaultVal, destPtrType.ElemType)
	}

	block.NewStore(defaultVal, attrPtr)
	return block, nil
}

func (g *CodeGenerator) getOrCreateStringConstant(name string) *ir.Global {
	sanitizedName := strings.Map(func(r rune) rune {
		if !unicode.IsLetter(r) && !unicode.IsNumber(r) {
			return '_'
		}
		return r
	}, name)
	strConstName := fmt.Sprintf(".str.%s", sanitizedName)

	for _, global := range g.module.Globals {
		if global.Name() == strConstName {
			return global
		}
	}

	// Add null terminator
	strWithNull := name + "\x00"
	strConst := g.module.NewGlobalDef(strConstName, constant.NewCharArray([]byte(strWithNull)))
	return strConst
}

var i8Ptr = types.NewPointer(types.I8)

func (g *CodeGenerator) typeID(typeName string) int {
	switch typeName {
	case "Int":
		return 1
	case "Bool":
		return 2
	case "String":
		return 3
	default:
		return 0
	}
}

func (g *CodeGenerator) convertType(typeName string) types.Type {
	switch strings.ToLower(typeName) {
	case "int":
		return types.I32
	case "bool":
		return  types.I1
	default:
		// For user-defined types, pointer to their struct type
		if classType, exists := g.classTypes[typeName]; exists {
			return types.NewPointer(classType)
		} else {
			return  i8Ptr 
		}
	}
}



func (g *CodeGenerator) getTypeDepth(typeName string) int {
	depth := 0
	current := typeName
	for {
		info, exists := g.classTable[current]
		if !exists || info.Parent == "" {
			break
		}
		depth++
		current = info.Parent
	}
	return depth
}

func (g *CodeGenerator) GetClassTable() ClassTable {
	return g.classTable
}

func (g *CodeGenerator) GetModule() *ir.Module {
	return g.module
}

func (g *CodeGenerator) GetVTables() map[string]*ir.Global {
	return g.vtables
}
