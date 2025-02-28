package codegen

import (
	"cool-compiler/ast"
	"fmt"
	"strings"

	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/enum"
	"github.com/llir/llvm/ir/types"
	"github.com/llir/llvm/ir/value"
)

// generateExpression handles code generation for expressions
func (g *CodeGenerator) generateExpression(block *ir.Block, expr ast.Expression) (value.Value, *ir.Block, error) {
	if expr == nil {
		return nil, nil, fmt.Errorf("received nil expression")
	}

	switch e := expr.(type) {
	case *ast.IntegerLiteral:
		return constant.NewInt(types.I32, int64(e.Value)), block, nil

	case *ast.BooleanLiteral:
		boolType := g.convertType("Bool")
		if boolType == types.I8 {
			if e.Value {
				return constant.NewInt(types.I8, 1), block, nil
			}
			return constant.NewInt(types.I8, 0), block, nil
		} else {
			if e.Value {
				return constant.NewInt(types.I1, 1), block, nil
			}
			return constant.NewInt(types.I1, 0), block, nil
		}

	case *ast.CaseExpression:
		return g.generateCaseExpression(block, e)

	case *ast.StringLiteral:
		return g.generateStringLiteral(block, e)
	case *ast.InfixExpression:
		return g.generateInfixExpression(block, e)

	case *ast.IfExpression:
		return g.generateIfExpression(block, e)
	case *ast.LetExpression:
		return g.generateLetExpression(block, e)
	case *ast.ObjectIdentifier:
		return g.generateObjectIdentifier(block, e)
	case *ast.WhileExpression:
		return g.generateWhileExpression(block, e)
	case *ast.MethodCall:
		return g.generateMethodCall(block, e)

	case *ast.FunctionCall:
		return g.generateFunctionCall(block, e)

	case *ast.NewExpression:
		return g.generateNewExpression(block, e)
	case *ast.NotExpression:
		return g.generateNotExpression(block, e)

	case *ast.IsVoidExpression:
		return g.generateIsVoidExpression(block, e)

	case *ast.BlockExpression:
		return g.generateBlockExpression(block, e)

	case *ast.Assignment:
		return g.generateAssignment(block, e)

	default:
		return nil, nil, fmt.Errorf("unsupported expression type: %T", expr)
	}
}

func (g *CodeGenerator) generateCaseExpression(block *ir.Block, e *ast.CaseExpression) (value.Value, *ir.Block, error) {
	// Generate the target expression
	target, currentBlock, err := g.generateExpression(block, e.Expression)
	if err != nil {
		return nil, currentBlock, err
	}

	// Box primitive types if needed
	target, currentBlock, err = g.boxPrimitiveIfNeeded(currentBlock, target)
	if err != nil {
		return nil, currentBlock, err
	}

	// Setup control flow blocks
	endBlock := currentBlock.Parent.NewBlock("case.end")
	var incoming []*ir.Incoming

	// Handle null check
	nonNullBlock, _, err := g.setupNullCheck(currentBlock, target)
	if err != nil {
		return nil, currentBlock, err
	}
	currentBlock = nonNullBlock

	// Find Object case early
	objectCase := g.findObjectCase(e.Cases)

	// Create object case block
	objectCaseBlock := currentBlock.Parent.NewBlock("case.object")

	// Determine if we're dealing with a primitive type or class object
	targetType := g.getClassNameFromType(target.Type())
	isPrimitive := targetType == "Int" || targetType == "Bool" || targetType == "String"

	// Filter and sort cases
	nonObjectCases := g.filterNonObjectCases(e.Cases)

	// Create test blocks for non-Object cases
	testBlocks := g.createTestBlocks(currentBlock, len(nonObjectCases))

	// Branch to first test block or directly to Object case if no other cases
	if len(testBlocks) > 0 {
		currentBlock.NewBr(testBlocks[0])
	} else {
		currentBlock.NewBr(objectCaseBlock)
	}

	// Handle specific type cases
	if len(nonObjectCases) > 0 {
		if isPrimitive {
			// For primitive types, do direct type ID comparison
			incoming = g.handlePrimitiveCases(nonObjectCases, testBlocks, objectCaseBlock, target, targetType, endBlock, incoming)
		} else {
			// For object types, check the class hierarchy
			incoming = g.handleObjectCases(nonObjectCases, testBlocks, objectCaseBlock, target, endBlock, incoming)
		}
	}

	// Handle Object case
	if objectCase != nil {
		objectResult, objectBodyBlock, err := g.handleSingleCase(objectCaseBlock, objectCase, target, "Object")
		if err != nil {
			return nil, objectBodyBlock, err
		}

		objectBodyBlock.NewBr(endBlock)
		incoming = append(incoming, ir.NewIncoming(objectResult, objectBodyBlock))
	}

	// Create final phi node for the result
	if len(incoming) > 0 {
		phi := endBlock.NewPhi(incoming...)
		return phi, endBlock, nil
	} else {
		// When no cases match, return null pointer instead of i32 0
		return constant.NewNull(types.NewPointer(types.I8)), endBlock, nil
	}
}
func (g *CodeGenerator) generateStringLiteral(block *ir.Block, e *ast.StringLiteral) (value.Value, *ir.Block, error) {
	// Process escape sequences
	processedStr := g.processStringLiteral(e.Value)

	// Ensure null termination
	strWithNull := processedStr + "\x00"
	strGlobal := g.getOrCreateStringConstant(strWithNull)

	// Get pointer to first character
	arrType := types.NewArray(uint64(len(strWithNull)), types.I8)
	strPtr := block.NewGetElementPtr(arrType, strGlobal,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0),
	)

	// Allocate String object on the heap
	mallocFunc := g.getOrCreateMalloc()
	stringSize := constant.NewInt(types.I64, 16) // Size of String struct
	stringObjMem := block.NewCall(mallocFunc, stringSize)
	stringObj := block.NewBitCast(stringObjMem, types.NewPointer(g.classTypes["String"]))

	// Initialize vtable
	vtablePtr := block.NewGetElementPtr(g.classTypes["String"], stringObj,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0),
	)
	block.NewStore(
		block.NewBitCast(g.vtables["String"], i8Ptr),
		vtablePtr,
	)

	// Set value field
	valuePtr := block.NewGetElementPtr(g.classTypes["String"], stringObj,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 1),
	)
	block.NewStore(strPtr, valuePtr)

	return stringObj, block, nil
}
func (g *CodeGenerator) generateInfixExpression(block *ir.Block, e *ast.InfixExpression) (value.Value, *ir.Block, error) {
	// Generate code for left and right expressions
	left, leftBlock, err := g.generateExpression(block, e.Left)
	if err != nil {
		return nil, leftBlock, err
	}

	right, rightBlock, err := g.generateExpression(leftBlock, e.Right)
	if err != nil {
		return nil, rightBlock, err
	}

	if !left.Type().Equal(right.Type()) {
		if left.Type() == types.I1 && right.Type() == types.I8 {
			left = rightBlock.NewZExt(left, types.I8)
		} else if left.Type() == types.I8 && right.Type() == types.I1 {
			right = rightBlock.NewZExt(right, types.I8)
		} else if left.Type() == types.I1 && right.Type() == types.I32 {
			left = rightBlock.NewZExt(left, types.I32)
		} else if left.Type() == types.I32 && right.Type() == types.I1 {
			right = rightBlock.NewZExt(right, types.I32)
		} else if left.Type() == types.I8 && right.Type() == types.I32 {
			left = rightBlock.NewZExt(left, types.I32)
		} else if left.Type() == types.I32 && right.Type() == types.I8 {
			right = rightBlock.NewZExt(right, types.I32)
		}
	}

	// Handle different operators
	switch e.Operator {
	case "+":
		return rightBlock.NewAdd(left, right), rightBlock, nil
	case "-":
		return rightBlock.NewSub(left, right), rightBlock, nil
	case "*":
		return rightBlock.NewMul(left, right), rightBlock, nil
	case "/":
		return rightBlock.NewSDiv(left, right), rightBlock, nil
	case "<":
		// For comparisons, return as appropriate bool type
		result := rightBlock.NewICmp(enum.IPredSLT, left, right)
		boolType := g.convertType("Bool")
		if boolType == types.I8 && result.Type() == types.I1 {
			return rightBlock.NewZExt(result, types.I8), rightBlock, nil
		}
		return result, rightBlock, nil
	case "<=":
		result := rightBlock.NewICmp(enum.IPredSLE, left, right)
		boolType := g.convertType("Bool")
		if boolType == types.I8 && result.Type() == types.I1 {
			return rightBlock.NewZExt(result, types.I8), rightBlock, nil
		}
		return result, rightBlock, nil
	case "=":
		result := rightBlock.NewICmp(enum.IPredEQ, left, right)
		boolType := g.convertType("Bool")
		if boolType == types.I8 && result.Type() == types.I1 {
			return rightBlock.NewZExt(result, types.I8), rightBlock, nil
		}
		return result, rightBlock, nil
	default:
		return nil, rightBlock, fmt.Errorf("unsupported operator: %s", e.Operator)
	}
}

func (g *CodeGenerator) generateIfExpression(block *ir.Block, e *ast.IfExpression) (value.Value, *ir.Block, error) {
	// Generate unique labels using the expression pointer
	labelSuffix := fmt.Sprintf("%p", e)

	cond, currentBlock, err := g.generateExpression(block, e.Condition)
	if err != nil {
		return nil, nil, err
	}

	if cond.Type() != types.I1 {
		if ptrType, ok := cond.Type().(*types.PointerType); ok {
			cond = currentBlock.NewICmp(enum.IPredNE, cond, constant.NewNull(ptrType))
		} else if cond.Type() == types.I8 {
			cond = currentBlock.NewICmp(enum.IPredNE, cond, constant.NewInt(types.I8, 0))
		} else if cond.Type() == types.I32 {
			cond = currentBlock.NewICmp(enum.IPredNE, cond, constant.NewInt(types.I32, 0))
		} else {
			return nil, nil, fmt.Errorf("unsupported condition type: %v", cond.Type())
		}
	}

	thenBlock := currentBlock.Parent.NewBlock(fmt.Sprintf("if.then.%s", labelSuffix))
	elseBlock := currentBlock.Parent.NewBlock(fmt.Sprintf("if.else.%s", labelSuffix))
	mergeBlock := currentBlock.Parent.NewBlock(fmt.Sprintf("if.merge.%s", labelSuffix))

	currentBlock.NewCondBr(cond, thenBlock, elseBlock)

	thenVal, thenCurrentBlock, err := g.generateExpression(thenBlock, e.Consequence)
	if err != nil {
		return nil, nil, err
	}
	thenCurrentBlock.NewBr(mergeBlock)

	elseVal, elseCurrentBlock, err := g.generateExpression(elseBlock, e.Alternative)
	if err != nil {
		return nil, nil, err
	}
	elseCurrentBlock.NewBr(mergeBlock)

	// FIX HERE: Check if any of the values is an integer and we're expecting a pointer
	if !thenVal.Type().Equal(elseVal.Type()) {
		// Handle i8 (Bool) and i32 (Int) mismatches
		if thenVal.Type() == types.I8 && elseVal.Type() == types.I32 {
			// Convert i8 to i32
			thenVal = mergeBlock.NewZExt(thenVal, types.I32)
		} else if thenVal.Type() == types.I32 && elseVal.Type() == types.I8 {
			// Convert i8 to i32
			elseVal = mergeBlock.NewZExt(elseVal, types.I32)
		} else if types.IsPointer(thenVal.Type()) && elseVal.Type() == types.I32 {
			// If 'then' is a pointer and 'else' is an integer, convert integer to null pointer
			if ptrType, ok := thenVal.Type().(*types.PointerType); ok {
				elseVal = constant.NewNull(ptrType)
			}
		} else if thenVal.Type() == types.I32 && types.IsPointer(elseVal.Type()) {
			// If 'then' is an integer and 'else' is a pointer, convert integer to null pointer
			if ptrType, ok := elseVal.Type().(*types.PointerType); ok {
				thenVal = constant.NewNull(ptrType)
			}
		} else if types.IsPointer(thenVal.Type()) && types.IsPointer(elseVal.Type()) {
			// If both are pointers but of different types, use bitcast to make them the same type
			// Choose the more specific type (not Object) if possible
			if g.getClassNameFromType(thenVal.Type()) == "Object" {
				thenVal = mergeBlock.NewBitCast(thenVal, elseVal.Type())
			} else {
				elseVal = mergeBlock.NewBitCast(elseVal, thenVal.Type())
			}
		} else {
			return nil, nil, fmt.Errorf("incompatible types in if branches: %v and %v",
				thenVal.Type(), elseVal.Type())
		}
	}

	phi := mergeBlock.NewPhi(
		ir.NewIncoming(thenVal, thenCurrentBlock),
		ir.NewIncoming(elseVal, elseCurrentBlock),
	)

	return phi, mergeBlock, nil
}

func (g *CodeGenerator) generateLetExpression(block *ir.Block, e *ast.LetExpression) (value.Value, *ir.Block, error) {
	// Save original environment to restore later
	originalLocals := make(map[string]value.Value)
	originalTypes := make(map[string]string)
	for k, v := range g.locals {
		originalLocals[k] = v
	}
	for k, v := range g.localsTypes {
		originalTypes[k] = v
	}

	currentBlock := block

	// Process the first binding
	llvmType := g.convertType(e.Type.Value)
	varAlloca := currentBlock.NewAlloca(llvmType)

	// Generate initialization for first binding
	if e.Init != nil {
		initValue, initBlock, err := g.generateExpression(currentBlock, e.Init)
		if err != nil {
			return nil, initBlock, err
		}
		currentBlock = initBlock

		// Cast if needed
		var valueToStore value.Value
		if initValue.Type().Equal(llvmType) {
			valueToStore = initValue
		} else if types.IsPointer(llvmType) && types.IsPointer(initValue.Type()) {
			valueToStore = currentBlock.NewBitCast(initValue, llvmType)
		} else {
			return nil, currentBlock, fmt.Errorf("type mismatch in let declaration: got %v, expected %v",
				initValue.Type(), llvmType)
		}

		currentBlock.NewStore(valueToStore, varAlloca)
	} else {
		// Default initialization
		var err error
		currentBlock, err = g.initializeAttribute(currentBlock, varAlloca, e.Type.Value, nil)
		if err != nil {
			return nil, currentBlock, err
		}
	}

	// Add first binding to environment
	g.locals[e.Name.Value] = varAlloca
	g.localsTypes[e.Name.Value] = e.Type.Value

	// Process additional bindings if they exist
	for _, binding := range e.Bindings {
		bindingType := g.convertType(binding.Type.Value)
		bindingAlloca := currentBlock.NewAlloca(bindingType)

		// Generate initialization for additional binding
		if binding.Init != nil {
			initValue, initBlock, err := g.generateExpression(currentBlock, binding.Init)
			if err != nil {
				return nil, initBlock, err
			}
			currentBlock = initBlock

			// Cast if needed
			var valueToStore value.Value
			if initValue.Type().Equal(bindingType) {
				valueToStore = initValue
			} else if types.IsPointer(bindingType) && types.IsPointer(initValue.Type()) {
				valueToStore = currentBlock.NewBitCast(initValue, bindingType)
			} else {
				return nil, currentBlock, fmt.Errorf("type mismatch in let binding: got %v, expected %v",
					initValue.Type(), bindingType)
			}

			currentBlock.NewStore(valueToStore, bindingAlloca)
		} else {
			// Default initialization
			var err error
			currentBlock, err = g.initializeAttribute(currentBlock, bindingAlloca, binding.Type.Value, nil)
			if err != nil {
				return nil, currentBlock, err
			}
		}

		// Add binding to environment
		g.locals[binding.Name.Value] = bindingAlloca
		g.localsTypes[binding.Name.Value] = binding.Type.Value
	}

	// Generate body with all bindings in scope
	bodyValue, bodyBlock, err := g.generateExpression(currentBlock, e.Body)
	if err != nil {
		return nil, bodyBlock, err
	}

	// Restore original environment
	g.locals = originalLocals
	g.localsTypes = originalTypes

	return bodyValue, bodyBlock, nil
}
func (g *CodeGenerator) generateObjectIdentifier(block *ir.Block, e *ast.ObjectIdentifier) (value.Value, *ir.Block, error) {

	if e.Value == "self" {
		if selfVal, exists := g.locals["self"]; exists {
			return selfVal, block, nil
		}
		return nil, block, fmt.Errorf("self not found in locals")
	}

	if varAlloca, exists := g.locals[e.Value]; exists {

		ptrType, ok := varAlloca.Type().(*types.PointerType)
		if !ok {
			return nil, block, fmt.Errorf("unexpected type for variable: %s", e.Value)
		}
		loadInst := block.NewLoad(ptrType.ElemType, varAlloca)
		return loadInst, block, nil
	}

	// If current class is a built-in type, do not attempt attribute lookup.
	lowerClass := strings.ToLower(g.currentClass)
	if lowerClass == "int" || lowerClass == "bool" || lowerClass == "string" {
		return nil, block, fmt.Errorf("undefined variable: %s", e.Value)
	}

	// Otherwise, attempt attribute lookup.
	if g.currentClass != "" {
		// Find attribute info.
		classInfo := g.classTable[g.currentClass]

		var attrInfo *AttributeInfo
		for i := range classInfo.Attributes {
			attr := &classInfo.Attributes[i] // Get pointer to the actual element
			if attr.Name == e.Value {
				attrInfo = attr
				break
			}
		}
		if attrInfo != nil {

			// Get the self parameter from the parent block.
			self := block.Parent.Params[0]
			classType := g.classTypes[g.currentClass]
			// Cast self to the proper type if needed.
			var selfPtr value.Value
			if !self.Type().Equal(types.NewPointer(classType)) {
				selfPtr = block.NewBitCast(self, types.NewPointer(classType))
			} else {
				selfPtr = self
			}
			// Build indices: first index 0 (for the struct), then the attribute offset.
			indices := []value.Value{
				constant.NewInt(types.I32, 0),
				constant.NewInt(types.I32, int64(attrInfo.Offset)),
			}
			attrPtr := block.NewGetElementPtr(classType, selfPtr, indices...)
			// Load the attribute value.
			fieldType := classType.Fields[attrInfo.Offset]
			loadInst := block.NewLoad(fieldType, attrPtr)
			return loadInst, block, nil
		}
	}
	return nil, block, fmt.Errorf("undefined variable: %s", e.Value)
}

func (g *CodeGenerator) generateWhileExpression(block *ir.Block, e *ast.WhileExpression) (value.Value, *ir.Block, error) {
	// Generate unique names for blocks to avoid conflicts in nested loops
	condBlockName := fmt.Sprintf("while.%p.cond", e)
	bodyBlockName := fmt.Sprintf("while.%p.body", e)
	endBlockName := fmt.Sprintf("while.%p.end", e)

	condBlock := block.Parent.NewBlock(condBlockName)
	bodyBlock := block.Parent.NewBlock(bodyBlockName)
	endBlock := block.Parent.NewBlock(endBlockName)

	// Branch to condition block
	block.NewBr(condBlock)

	// Generate condition code
	condition, condEndBlock, err := g.generateExpression(condBlock, e.Condition)
	if err != nil {
		return nil, condEndBlock, err
	}

	// Convert condition to boolean if needed
	var condValue value.Value
	if condition.Type() == types.I1 {
		condValue = condition
	} else {
		condValue = condEndBlock.NewICmp(enum.IPredNE, condition, constant.NewInt(types.I32, 0))
	}

	// Conditional branch: if true go to body, if false go to end
	condEndBlock.NewCondBr(condValue, bodyBlock, endBlock)

	// Generate body code
	_, bodyEndBlock, err := g.generateExpression(bodyBlock, e.Body)
	if err != nil {
		return nil, bodyEndBlock, err
	}

	// Branch back to condition block
	bodyEndBlock.NewBr(condBlock)

	result := constant.NewNull(types.NewPointer(types.I8))
	return result, endBlock, nil
}

func (g *CodeGenerator) generateFunctionCall(block *ir.Block, e *ast.FunctionCall) (value.Value, *ir.Block, error) {
	// First, add self as the first argument
	args := make([]value.Value, 0, len(e.Arguments)+1) // +1 for self
	currentBlock := block

	// Get the self parameter
	selfPtr := g.locals["$self"]
	if selfPtr == nil {
		selfPtr = g.locals["self"]
	}

	if selfPtr == nil {
		return nil, currentBlock, fmt.Errorf("self not found in locals for function call %s", e.Function.Value)
	}

	// Add self as the first argument
	args = append(args, selfPtr)

	// Then generate and add the actual arguments
	for _, arg := range e.Arguments {
		argValue, argBlock, err := g.generateExpression(currentBlock, arg)
		if err != nil {
			return nil, argBlock, err
		}
		args = append(args, argValue)
		currentBlock = argBlock
	}

	// Get function from module
	funcName := fmt.Sprintf("%s_%s", g.currentClass, e.Function.Value)
	function := g.methods[funcName]

	// If not found in current class, try to find in Main class
	if function == nil {
		funcName = fmt.Sprintf("Main_%s", e.Function.Value)
		function = g.methods[funcName]
	}

	if function == nil {
		return nil, currentBlock, fmt.Errorf("undefined function: %s", e.Function.Value)
	}

	// Call the function with arguments
	call := currentBlock.NewCall(function, args...)
	return call, currentBlock, nil
}

func (g *CodeGenerator) generateNewExpression(block *ir.Block, e *ast.NewExpression) (value.Value, *ir.Block, error) {
	classType := g.classTypes[e.Type.Value]
	if classType == nil {
		return nil, block, fmt.Errorf("undefined type: %s", e.Type.Value)
	}
	
	info, ok := g.classTable[e.Type.Value]
    if !ok {
        return nil, block, fmt.Errorf("no class info for type: %s", e.Type.Value)
    }

    // Use malloc to allocate on the heap instead of alloca (stack)
    mallocFunc := g.getOrCreateMalloc()
    size := constant.NewInt(types.I64, int64(info.ObjectSize*8))
    objMem := block.NewCall(mallocFunc, size)
    objPtr := block.NewBitCast(objMem, types.NewPointer(classType))
	objPtr_i8 := block.NewBitCast(objPtr, i8Ptr)
	// Use the computed object size

	// Look up an existing memset intrinsic.
	memsetFunc := g.getOrCreateMemset()
	block.NewCall(memsetFunc, objPtr_i8, constant.NewInt(types.I8, 0), size,
		constant.NewInt(types.I32, 0), constant.NewInt(types.I1, 0))

	vtable := g.vtables[e.Type.Value]
	if vtable == nil {
		return nil, block, fmt.Errorf("vtable not found for type: %s", e.Type.Value)
	}
	vtablePtr := block.NewGetElementPtr(classType, objPtr,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0))
	castedVtable := block.NewBitCast(vtable, i8Ptr)
	block.NewStore(castedVtable, vtablePtr)

	// Initialize attributes using class-defined initializers
	for _, attr := range info.Attributes {
		// Find the AST attribute to get the initializer expression
		var attrAST *ast.Attribute
		for _, class := range g.program.Classes {
			if class.Name.Value == e.Type.Value {
				for _, feature := range class.Features {
					if a, ok := feature.(*ast.Attribute); ok && a.Name.Value == attr.Name {
						attrAST = a
						break
					}
				}
				if attrAST != nil {
					break
				}
			}
		}

		// Get pointer to the current attribute
		attrPtr := block.NewGetElementPtr(classType, objPtr,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, int64(attr.Offset)),
		)

		// Generate code for the initializer if present
		var initExpr ast.Expression
		if attrAST != nil {
			initExpr = attrAST.Init
		}

		var err error
		block, err = g.initializeAttribute(block, attrPtr, attr.Type, initExpr)
		if err != nil {
			return nil, block, fmt.Errorf("error initializing attribute %s: %v", attr.Name, err)
		}
	}

	return block.NewBitCast(objPtr, i8Ptr), block, nil

}

func (g *CodeGenerator) generateNotExpression(block *ir.Block, e *ast.NotExpression) (value.Value, *ir.Block, error) {
	// Generate the expression to negate
	expr, exprBlock, err := g.generateExpression(block, e.Expression)
	if err != nil {
		return nil, exprBlock, err
	}

	// Convert to boolean
	var boolValue value.Value
	if expr.Type() != types.I1 {
		boolValue = exprBlock.NewICmp(enum.IPredNE, expr, constant.NewInt(types.I32, 0))
	} else {
		boolValue = expr
	}
	// Negate the boolean and convert back to i32
	notValue := exprBlock.NewXor(boolValue, constant.NewInt(types.I1, 1))
	result := exprBlock.NewZExt(notValue, types.I32)

	return result, exprBlock, nil
}

func (g *CodeGenerator) generateIsVoidExpression(block *ir.Block, e *ast.IsVoidExpression) (value.Value, *ir.Block, error) {
	// Generate the expression to check
	expr, exprBlock, err := g.generateExpression(block, e.Expression)
	if err != nil {
		return nil, exprBlock, err
	}

	// If the expression's type is one of COOL's built‑in types, it's never void.
	if g.isBuiltin(expr.Type()) {
		// Return false as i32 (0)
		return exprBlock.NewZExt(constant.NewInt(types.I1, 0), types.I32), exprBlock, nil
	}

	// Otherwise, for object types, compare the pointer to null.
	ptrType, ok := expr.Type().(*types.PointerType)
	if !ok {
		// Not a pointer—treat as non‑void.
		return exprBlock.NewZExt(constant.NewInt(types.I1, 0), types.I32), exprBlock, nil
	}

	nullPtr := constant.NewNull(ptrType)
	isVoid := exprBlock.NewICmp(enum.IPredEQ, expr, nullPtr)
	result := exprBlock.NewZExt(isVoid, types.I32)
	return result, exprBlock, nil

}

func (g *CodeGenerator) generateBlockExpression(block *ir.Block, e *ast.BlockExpression) (value.Value, *ir.Block, error) {
	var lastValue value.Value
	currentBlock := block

	// Generate code for each expression in sequence
	for _, expr := range e.Expressions {
		var err error
		var exprValue value.Value

		exprValue, currentBlock, err = g.generateExpression(currentBlock, expr)
		if err != nil {
			return nil, currentBlock, err
		}

		lastValue = exprValue
	}

	// Return the value of the last expression
	return lastValue, currentBlock, nil
}

func (g *CodeGenerator) generateAssignment(block *ir.Block, e *ast.Assignment) (value.Value, *ir.Block, error) {
	// Generate the value to assign
	value, valueBlock, err := g.generateExpression(block, e.Expression)
	if err != nil {
		return nil, valueBlock, err
	}

	// Look up the variable location
	varPtr := g.locals[e.Name.Value]
	if varPtr == nil {
		return nil, valueBlock, fmt.Errorf("undefined variable: %s", e.Name.Value)
	}

	// Get destination type
	ptrType, ok := varPtr.Type().(*types.PointerType)
	if !ok {
		return nil, valueBlock, fmt.Errorf("expected a pointer type for variable %s, got %T", e.Name.Value, varPtr.Type())
	}
	destType := ptrType.ElemType

	// Cast value to destination type if necessary
	if value.Type() != destType {
		if types.IsPointer(value.Type()) && types.IsPointer(destType) {
			value = valueBlock.NewBitCast(value, destType)
		} else {
			return nil, valueBlock, fmt.Errorf("type mismatch in assignment: %s -> %s", value.Type(), destType)
		}
	}

	// Store the new value
	valueBlock.NewStore(value, varPtr)

	// Return the assigned value
	return value, valueBlock, nil
}

func (g *CodeGenerator) generateMethodCall(block *ir.Block, e *ast.MethodCall) (value.Value, *ir.Block, error) {
	// Get receiver object and type
	receiverObj, receiverType, block, err := g.resolveMethodReceiver(block, e)
	if err != nil {
		return nil, block, err
	}

	// Handle special methods
	if result, newBlock, handled, err := g.handleSpecialMethods(block, e, receiverObj, receiverType); handled {
		return result, newBlock, err
	}

	// Special handling for IO methods
	if receiverType == "IO" || receiverType == "Main" {
		methodName := e.Method.Value
		switch methodName {
		case "out_string", "out_int", "in_string", "in_int":
			// Generate arguments
			args := make([]value.Value, 0, len(e.Arguments))
			currentBlock := block

			for _, arg := range e.Arguments {
				argVal, newBlock, err := g.generateExpression(currentBlock, arg)
				if err != nil {
					return nil, newBlock, err
				}
				currentBlock = newBlock
				args = append(args, argVal)
			}

			// Call IO method helper with string method name and the receiver object
			return g.generateIOMethodCall(currentBlock, methodName, receiverObj, args...)
		}
	}

	// Handle regular method calls
	return g.generateRegularMethodCall(block, e, receiverObj, receiverType)
}

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
	case *ast.MethodCall:
		
		// Method call on another method call
		// The type of a method call is the return type of the method
		methodName := fmt.Sprintf("%s_%s", receiverType, expr.Method.Value)
		method := g.methods[methodName]
		if method == nil {
			err = fmt.Errorf("method %s not found", methodName)
			return
		}
		

	case *ast.FunctionCall:
		// Function call (like out_int, out_string) typically returns SELF_TYPE in COOL
		// The type of "self" is the current class
		receiverType = g.currentClass
	default:
		err = fmt.Errorf("unsupported receiver expression: %T", e.Object)
		return
	}

	return
}
