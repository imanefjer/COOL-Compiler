package codegen

import (
	"cool-compiler/ast"
	"cool-compiler/lexer"
	"fmt"
	"strings"

	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/enum"
	"github.com/llir/llvm/ir/types"
	"github.com/llir/llvm/ir/value"
)

type CodeGenerator struct {
	module     *ir.Module
	classTypes map[string]*types.StructType
	vtables    map[string]*ir.Global
	methods    map[string]*ir.Func
	locals     map[string]value.Value
	// NEW: record the declared (static) type for let–bound variables that are objects.
	localsTypes  map[string]string
	currentClass string              // Track current class during code generation
	classAttrs   map[string][]string // Track class attributes
	classTable   ClassTable
	program      *ast.Program // Add this field
}

func NewCodeGenerator() *CodeGenerator {
	return &CodeGenerator{
		module:      ir.NewModule(),
		classTypes:  make(map[string]*types.StructType),
		vtables:     make(map[string]*ir.Global),
		methods:     make(map[string]*ir.Func),
		locals:      make(map[string]value.Value),
		localsTypes: make(map[string]string),
		classTable:  make(ClassTable), // Initialize the class table map
	}
}

func (g *CodeGenerator) Generate(program *ast.Program) (*ir.Module, error) {
	g.program = program // Store the program
	// 1. Declare classes (collect local attributes and methods).
	g.addBuiltInClasses(program)

	if err := g.DeclareClasses(program.Classes); err != nil {
		return nil, err
	}

	// 2. Build the inheritance table (which merges inherited attributes).
	if err := g.BuildClassTable(program.Classes); err != nil {
		return nil, err
	}

	// 3. Compute object layouts (attribute offsets and total size).
	if err := g.ComputeObjectLayouts(); err != nil {
		return nil, err
	}

	// 4. Now, build the LLVM struct types from the full class table.
	if err := g.BuildClassTypesFromTable(); err != nil {
		return nil, err
	}

	// 5. Construct vtables.
	if err := g.ConstructVTables(); err != nil {
		return nil, err
	}

	// 6. Generate method implementations.
	for _, class := range program.Classes {
		if err := g.generateClass(class); err != nil {
			return nil, err
		}
	}

	// 7. Generate program entry point.
	if err := g.generateProgramEntryPoint(); err != nil {
		return nil, err
	}

	return g.module, nil
}

func (g *CodeGenerator) DeclareClasses(classes []*ast.Class) error {
	// Create Object type first.
	objectType := types.NewStruct(
		types.NewPointer(types.I8), // vtable pointer as i8*
	)
	g.classTypes["Object"] = objectType

	// Initialize maps.
	g.classAttrs = make(map[string][]string)
	classAttributes := make(map[string][]*ast.Attribute)

	// Collect attribute names and attribute nodes.
	for _, class := range classes {
		var attrNames []string
		var attrs []*ast.Attribute
		for _, feature := range class.Features {
			if attr, ok := feature.(*ast.Attribute); ok {
				attrNames = append(attrNames, attr.Name.Value)
				attrs = append(attrs, attr)
			}
		}
		g.classAttrs[class.Name.Value] = attrNames
		classAttributes[class.Name.Value] = attrs
	}

	// Create LLVM struct types for each class.
	for _, class := range classes {
		// Start with the vtable pointer field.
		fields := []types.Type{types.NewPointer(types.I8)}

		// For each attribute declared in this class, select its LLVM type based on its declared type.
		attrs := classAttributes[class.Name.Value]
		for _, attr := range attrs {
			var fieldType types.Type
			switch strings.ToLower(attr.Type.Value) {
			case "int":
				fieldType = types.I32
			case "bool":
				fieldType = types.I1
			default:
				// For "String" and user-defined types, use a pointer type.
				fieldType = types.NewPointer(types.I8)
			}
			fields = append(fields, fieldType)
		}
		classType := types.NewStruct(fields...)
		g.classTypes[class.Name.Value] = classType
	}

	// Declare methods remains unchanged.
	for _, class := range classes {
		for _, feature := range class.Features {
			if method, ok := feature.(*ast.Method); ok {
				methodName := fmt.Sprintf("%s_%s", class.Name.Value, method.Name.Value)
				var paramTypes []types.Type
				var retType types.Type

				// Handle return types based on declared type
				switch method.ReturnType.Value {
				case "Object":
					retType = types.NewPointer(types.I8)
				case "SELF_TYPE":
					retType = types.NewPointer(types.I8)
				case "Int":
					retType = types.I32
				case "Bool":
					retType = types.I1
				default:
					retType = types.NewPointer(types.I8)
				}

				// First parameter is always self
				paramTypes = append(paramTypes, types.NewPointer(types.I8))

				// Add other parameters
				for _, param := range method.Parameters {
					switch param.Type.Value {
					case "Int":
						paramTypes = append(paramTypes, types.I32)
					case "Bool":
						paramTypes = append(paramTypes, types.I1)
					default:
						paramTypes = append(paramTypes, types.NewPointer(types.I8))
					}
				}

				// Create function with proper signature
				fn := g.module.NewFunc(methodName, retType)
				// Add parameters
				params := make([]*ir.Param, len(paramTypes))
				params[0] = ir.NewParam("self", paramTypes[0])
				for i := 1; i < len(paramTypes); i++ {
					params[i] = ir.NewParam(fmt.Sprintf("p%d", i), paramTypes[i])
				}
				fn.Params = params
				g.methods[methodName] = fn
			}
		}
	}
	return nil
}

// initializeVTables initializes the vtables for all classes
func (g *CodeGenerator) initializeVTables(classes []*ast.Class) error {
	for _, class := range classes {
		// Count methods for vtable size
		methodCount := 0
		for _, feature := range class.Features {
			if _, ok := feature.(*ast.Method); ok {
				methodCount++
			}
		}

		// Create string constants for class names
		className := g.getOrCreateStringConstant(class.Name.Value)
		parentClassName := g.getOrCreateStringConstant("Object")

		// Create vtable type with correct size
		vtableType := types.NewStruct(
			types.NewPointer(types.I8),                                      // type name
			types.NewPointer(types.I8),                                      // parent class name
			types.NewArray(uint64(methodCount), types.NewPointer(types.I8)), // method array
		)

		// Create method list
		methodList := make([]constant.Constant, 0, methodCount)
		for _, feature := range class.Features {
			if method, ok := feature.(*ast.Method); ok {
				methodName := fmt.Sprintf("%s_%s", class.Name.Value, method.Name.Value)
				fn := g.methods[methodName]
				// Cast function to i8* for vtable
				methodPtr := constant.NewBitCast(fn, types.NewPointer(types.I8))
				methodList = append(methodList, methodPtr)
			}
		}

		// Create vtable initializer
		vtableInit := constant.NewStruct(vtableType,
			constant.NewBitCast(className, types.NewPointer(types.I8)),
			constant.NewBitCast(parentClassName, types.NewPointer(types.I8)),
			constant.NewArray(types.NewArray(uint64(methodCount), types.NewPointer(types.I8)),
				methodList...),
		)

		// Create vtable global
		vtable := g.module.NewGlobalDef(
			fmt.Sprintf("%s_vtable", class.Name.Value),
			vtableInit,
		)
		g.vtables[class.Name.Value] = vtable
	}
	return nil
}

// generateClass handles code generation for a single class
func (g *CodeGenerator) generateClass(class *ast.Class) error {
	g.currentClass = class.Name.Value
	defer func() { g.currentClass = "" }()

	// Rest of generateClass remains...
	for _, feature := range class.Features {
		if method, ok := feature.(*ast.Method); ok {
			if err := g.generateMethod(class.Name.Value, method); err != nil {
				return err
			}
		}
	}
	return nil
}

func (g *CodeGenerator) generateProgramEntryPoint() error {
	mainFn := g.module.NewFunc("main", types.I32)
	block := mainFn.NewBlock("entry")

	// Get Main class info and AST node
	mainClassInfo, ok := g.classTable["Main"]
	if !ok {
		return fmt.Errorf("Main class not found")
	}
	var mainClassAST *ast.Class
	for _, class := range g.program.Classes {
		if class.Name.Value == "Main" {
			mainClassAST = class
			break
		}
	}

	// Allocate Main object
	mainType := g.classTypes["Main"]
	mainObj := block.NewAlloca(mainType)

	// Initialize vtable pointer
	vtablePtr := block.NewGetElementPtr(mainType, mainObj,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0),
	)
	block.NewStore(
		block.NewBitCast(g.vtables["Main"], types.NewPointer(types.I8)),
		vtablePtr,
	)

	// Initialize attributes using AST initializers
	for _, attrInfo := range mainClassInfo.Attributes {
		// Find matching attribute in AST
		var attrAST *ast.Attribute
		for _, feature := range mainClassAST.Features {
			if a, ok := feature.(*ast.Attribute); ok && a.Name.Value == attrInfo.Name {
				attrAST = a
				break
			}
		}

		// Get pointer to attribute
		attrPtr := block.NewGetElementPtr(mainType, mainObj,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, int64(attrInfo.Offset)),
		)

		if attrAST != nil && attrAST.Init != nil {
			// Generate code for custom initializer
			initVal, initBlock, err := g.generateExpression(block, attrAST.Init)
			if err != nil {
				return fmt.Errorf("error initializing %s: %v", attrInfo.Name, err)
			}
			block = initBlock

			// Cast if necessary
			if !initVal.Type().Equal(attrPtr.Type().(*types.PointerType).ElemType) {
				initVal = block.NewBitCast(initVal, attrPtr.Type().(*types.PointerType).ElemType)
			}

			block.NewStore(initVal, attrPtr)
		} else {
			// Fallback to default initialization
			var defaultVal value.Value
			switch strings.ToLower(attrInfo.Type) {
			case "int":
				defaultVal = constant.NewInt(types.I32, 0)
			case "bool":
				defaultVal = constant.NewInt(types.I1, 0)
			case "string":
				emptyStr := g.getOrCreateStringConstant("")
				defaultVal = block.NewBitCast(emptyStr, types.NewPointer(types.I8))
			default:
				defaultVal = constant.NewNull(types.NewPointer(types.I8))
			}
			block.NewStore(defaultVal, attrPtr)
		}
	}

	// Call main method
	mainObjCast := block.NewBitCast(mainObj, types.NewPointer(types.I8))
	mainMethod := g.methods["Main_main"]
	block.NewCall(mainMethod, mainObjCast)

	block.NewRet(constant.NewInt(types.I32, 0))
	return nil
}

func (g *CodeGenerator) generateMethod(className string, method *ast.Method) error {
	methodName := fmt.Sprintf("%s_%s", className, method.Name.Value)
	fn, ok := g.methods[methodName]
	if !ok {
		return fmt.Errorf("method %s not found", methodName)
	}

	entryBlock := fn.NewBlock("entry")

	// Special case for IO built-in methods
	if className == "IO" {
		// Handle out_string
		if method.Name.Value == "out_string" {
			self := fn.Params[0]
			str := fn.Params[1]

			var printf *ir.Func
			for _, f := range g.module.Funcs {
				if f.Name() == "printf" {
					printf = f
					break
				}
			}
			if printf == nil {
				printf = g.module.NewFunc("printf", types.I32, ir.NewParam("format", i8Ptr))
				printf.Sig.Variadic = true
			}

			formatGlobal := g.getOrCreateStringConstant("%s\n")
			var formatPtr value.Value
			if arrType, ok := formatGlobal.Type().(*types.ArrayType); ok {
				formatPtr = entryBlock.NewGetElementPtr(arrType, formatGlobal,
					constant.NewInt(types.I32, 0),
					constant.NewInt(types.I32, 0))
			} else {
				formatPtr = formatGlobal
			}

			entryBlock.NewCall(printf, formatPtr, str)
			entryBlock.NewRet(self)
			return nil
		}

		// Handle out_int
		if method.Name.Value == "out_int" {
			self := fn.Params[0]
			intArg := fn.Params[1]

			var printf *ir.Func
			for _, f := range g.module.Funcs {
				if f.Name() == "printf" {
					printf = f
					break
				}
			}
			if printf == nil {
				printf = g.module.NewFunc("printf", types.I32, ir.NewParam("format", i8Ptr))
				printf.Sig.Variadic = true
			}

			formatGlobal := g.getOrCreateStringConstant("%d\n")
			var formatPtr value.Value
			if arrType, ok := formatGlobal.Type().(*types.ArrayType); ok {
				formatPtr = entryBlock.NewGetElementPtr(arrType, formatGlobal,
					constant.NewInt(types.I32, 0),
					constant.NewInt(types.I32, 0))
			} else {
				formatPtr = formatGlobal
			}

			entryBlock.NewCall(printf, formatPtr, intArg)
			entryBlock.NewRet(self)
			return nil
		}
	}

	// Normal method generation for non built-in methods:
	// Retrieve the self parameter (declared as i8*).
	self := fn.Params[0]
	classType, ok := g.classTypes[className]
	if !ok {
		return fmt.Errorf("class type not found for %s", className)
	}
	typedSelf := entryBlock.NewBitCast(self, types.NewPointer(classType))
	g.locals["$self"] = typedSelf
	g.localsTypes["$self"] = className // Store the static type of self.

	for i, attrName := range g.classAttrs[className] {
		attrPtr := entryBlock.NewGetElementPtr(
			classType,
			typedSelf,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, int64(i+1)),
		)
		g.locals[attrName] = attrPtr
	}

	value, currentBlock, err := g.generateExpression(entryBlock, method.Body)
	if err != nil {
		return err
	}

	currentBlock.NewRet(value)
	if method.ReturnType.Value == "Object" {
		// Return null pointer instead of integer
		currentBlock.NewRet(constant.NewNull(types.NewPointer(types.I8)))
	} else {
		currentBlock.NewRet(value)
	}
	return nil
}

var i8Ptr = types.NewPointer(types.I8)

// generateExpression handles code generation for expressions
func (g *CodeGenerator) generateExpression(block *ir.Block, expr ast.Expression) (value.Value, *ir.Block, error) {
	switch e := expr.(type) {
	case *ast.IntegerLiteral:
		return constant.NewInt(types.I32, int64(e.Value)), block, nil

	case *ast.BooleanLiteral:
		if e.Value {
			return constant.NewInt(types.I1, 1), block, nil
		}
		return constant.NewInt(types.I1, 0), block, nil
	case *ast.StringLiteral:
		// Create or retrieve a global constant for the string.
		strGlobal := g.getOrCreateStringConstant(e.Value)
		// Bitcast the global constant to i8*.
		return block.NewBitCast(strGlobal, types.NewPointer(types.I8)), block, nil

	case *ast.InfixExpression:
		// Generate code for left and right expressions
		left, leftBlock, err := g.generateExpression(block, e.Left)
		if err != nil {
			return nil, leftBlock, err
		}

		right, rightBlock, err := g.generateExpression(leftBlock, e.Right)
		if err != nil {
			return nil, rightBlock, err
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
			cmp := rightBlock.NewICmp(enum.IPredSLT, left, right)
			return rightBlock.NewZExt(cmp, types.I32), rightBlock, nil
		default:
			return nil, rightBlock, fmt.Errorf("unsupported operator: %s", e.Operator)
		}

	case *ast.IfExpression:
		cond, currentBlock, err := g.generateExpression(block, e.Condition)
		if err != nil {
			return nil, nil, err
		}
		if cond.Type() != types.I1 {
			cond = currentBlock.NewICmp(enum.IPredNE, cond, constant.NewInt(types.I32, 0))
		}
		thenBlock := currentBlock.Parent.NewBlock("if.then")
		elseBlock := currentBlock.Parent.NewBlock("if.else")
		mergeBlock := currentBlock.Parent.NewBlock("if.merge")

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

		// Ensure both incoming values have the same type.
		if !thenVal.Type().Equal(elseVal.Type()) {
			elseVal = mergeBlock.NewBitCast(elseVal, thenVal.Type())
		}
		phi := mergeBlock.NewPhi(ir.NewIncoming(thenVal, thenBlock), ir.NewIncoming(elseVal, elseBlock))
		return phi, mergeBlock, nil

	case *ast.LetExpression:
		var llvmType types.Type
		switch strings.ToLower(e.Type.Value) {
		case "int":
			llvmType = types.I32
		case "bool":
			llvmType = types.I1
		default:
			// For user-defined types, look up the class type or use i8*
			if classType, exists := g.classTypes[e.Type.Value]; exists {
				llvmType = types.NewPointer(classType)
			} else {
				llvmType = types.NewPointer(types.I8)
			}
		}

		// Allocate storage for the let variable
		varAlloca := block.NewAlloca(llvmType)

		// Generate the initialization expression
		initValue, initBlock, err := g.generateExpression(block, e.Init)
		if err != nil {
			return nil, initBlock, err
		}

		// Cast the value to the correct type if needed
		var valueToStore value.Value
		if initValue.Type().Equal(llvmType) {
			valueToStore = initValue
		} else if types.IsPointer(llvmType) && types.IsPointer(initValue.Type()) {
			// If both types are pointers, bitcast to the target type
			valueToStore = initBlock.NewBitCast(initValue, llvmType)
		} else {
			// If types don't match and aren't both pointers, this is an error
			return nil, initBlock, fmt.Errorf("type mismatch in let expression: got %v, expected %v",
				initValue.Type(), llvmType)
		}

		// Store the value
		initBlock.NewStore(valueToStore, varAlloca)

		// Save any previous binding and update locals
		oldValue, exists := g.locals[e.Name.Value]
		g.locals[e.Name.Value] = varAlloca

		// Generate code for the let body
		bodyValue, bodyBlock, err := g.generateExpression(initBlock, e.Body)
		if err != nil {
			return nil, bodyBlock, err
		}

		// Restore the old binding if it existed
		if exists {
			g.locals[e.Name.Value] = oldValue
		} else {
			delete(g.locals, e.Name.Value)
		}

		return bodyValue, bodyBlock, nil
	case *ast.ObjectIdentifier:
		// First check local variables.
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
			for _, attr := range classInfo.Attributes {
				if attr.Name == e.Value {
					attrInfo = &attr
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

	case *ast.WhileExpression:
		// Create blocks for condition, body, and merge
		condBlock := block.Parent.NewBlock("while.cond")
		bodyBlock := block.Parent.NewBlock("while.body")
		mergeBlock := block.Parent.NewBlock("while.end")
		mergeBlock.NewRet(constant.NewNull(types.NewPointer(types.I8)))

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

		// Conditional branch: if true go to body, if false go to merge
		condEndBlock.NewCondBr(condValue, bodyBlock, mergeBlock)

		// Generate body code
		_, bodyEndBlock, err := g.generateExpression(bodyBlock, e.Body)
		if err != nil {
			return nil, bodyEndBlock, err
		}

		// Branch back to condition block
		bodyEndBlock.NewBr(condBlock)

		// Return 0 as result (while always returns 0 in COOL)
		result := constant.NewInt(types.I32, 0)
		return result, mergeBlock, nil

	case *ast.MethodCall:
		// Get the receiver object and its type
		var receiverObj value.Value
		var receiverType string
		var err error

		if e.Object == nil {
			receiverObj = g.locals["self"]
			receiverType = g.currentClass
		} else {
			receiverObj, block, err = g.generateExpression(block, e.Object)
			if err != nil {
				return nil, block, err
			}
			if newExpr, ok := e.Object.(*ast.NewExpression); ok {
				receiverType = newExpr.Type.Value
			} else {
				return nil, block, fmt.Errorf("unsupported receiver type")
			}
		}

		// Find the method in the class hierarchy
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
		args := make([]value.Value, 0, len(e.Arguments)+1)
		args = append(args, receiverObj)

		for _, arg := range e.Arguments {
			argVal, newBlock, err := g.generateExpression(block, arg)
			if err != nil {
				return nil, newBlock, err
			}
			block = newBlock
			args = append(args, argVal)
		}

		// Call the method
		result := block.NewCall(methodFunc, args...)
		if result == nil {
			return nil, block, fmt.Errorf("failed to generate call to %s", methodName)
		}
		return result, block, nil

	case *ast.FunctionCall:
		// Generate argument expressions
		if e.Function.Value == "out_string" && g.currentClass == "IO" {
			// Implicit receiver: use self.
			methodCall := &ast.MethodCall{
				Token:     e.Token,
				Object:    &ast.ObjectIdentifier{Token: lexer.Token{Literal: "self"}, Value: "self"},
				Method:    e.Function,
				Arguments: e.Arguments,
			}
			// Now process as a MethodCall.
			return g.generateExpression(block, methodCall)
		}
		args := make([]value.Value, 0, len(e.Arguments))
		currentBlock := block

		for _, arg := range e.Arguments {
			argValue, argBlock, err := g.generateExpression(currentBlock, arg)
			if err != nil {
				return nil, argBlock, err
			}
			args = append(args, argValue)
			currentBlock = argBlock
		}

		// Get function from module
		funcName := fmt.Sprintf("%s_%s", "Main", e.Function.Value) // TODO: handle other classes
		function := g.methods[funcName]
		if function == nil {
			return nil, currentBlock, fmt.Errorf("undefined function: %s", e.Function.Value)
		}

		// Call the function with arguments
		call := currentBlock.NewCall(function, args...)
		return call, currentBlock, nil

	case *ast.NewExpression:
		classType := g.classTypes[e.Type.Value]
		if classType == nil {
			return nil, block, fmt.Errorf("undefined type: %s", e.Type.Value)
		}
		info, ok := g.classTable[e.Type.Value]
		if !ok {
			return nil, block, fmt.Errorf("no class info for type: %s", e.Type.Value)
		}

		objPtr := block.NewAlloca(classType)
		objPtr_i8 := block.NewBitCast(objPtr, i8Ptr)
		// Use the computed object size (assume each field is 8 bytes)
		size := constant.NewInt(types.I64, int64(info.ObjectSize*8))

		// Look up an existing memset intrinsic.
		var memsetFunc *ir.Func
		for _, f := range g.module.Funcs {
			if f.Name() == "llvm.memset.p0i8.i64" {
				memsetFunc = f
				break
			}
		}
		if memsetFunc == nil {
			memsetFunc = g.module.NewFunc("llvm.memset.p0i8.i64", types.Void,
				ir.NewParam("dst", i8Ptr),
				ir.NewParam("val", types.I8),
				ir.NewParam("len", types.I64),
				ir.NewParam("align", types.I32),
				ir.NewParam("isvolatile", types.I1))
		}
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

		for _, attr := range info.Attributes {
			attrPtr := block.NewGetElementPtr(classType, objPtr,
				constant.NewInt(types.I32, 0),
				constant.NewInt(types.I32, int64(attr.Offset)))
			var defaultVal value.Value
			switch strings.ToLower(attr.Type) {
			case "int":
				defaultVal = constant.NewInt(types.I32, 0)
			case "bool":
				defaultVal = constant.NewInt(types.I1, 0)
			case "string":
				emptyStr := g.getOrCreateStringConstant("")
				defaultVal = block.NewBitCast(emptyStr, i8Ptr)
			default:
				objType, exists := g.classTypes[attr.Type]
				if !exists {
					return nil, block, fmt.Errorf("unknown attribute type: %s", attr.Type)
				}
				defaultVal = constant.NewNull(types.NewPointer(objType))
			}
			block.NewStore(defaultVal, attrPtr)
		}

		return block.NewBitCast(objPtr, i8Ptr), block, nil
	case *ast.NotExpression:
		// Generate the expression to negate
		expr, exprBlock, err := g.generateExpression(block, e.Expression)
		if err != nil {
			return nil, exprBlock, err
		}

		// Convert to boolean if needed
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

	case *ast.IsVoidExpression:
		// Generate the expression to check
		expr, exprBlock, err := g.generateExpression(block, e.Expression)
		if err != nil {
			return nil, exprBlock, err
		}

		// Compare with null pointer
		var isVoid value.Value
		if expr.Type().Equal(types.I32) {
			// For basic types, always false
			isVoid = constant.NewInt(types.I1, 0)
		} else {
			// For objects, compare with null
			ptrType, ok := expr.Type().(*types.PointerType)
			if !ok {
				isVoid = constant.NewInt(types.I1, 0)
			} else {
				nullPtr := constant.NewNull(ptrType)
				isVoid = exprBlock.NewICmp(enum.IPredEQ, expr, nullPtr)
			}
		}

		// Convert boolean to i32
		result := exprBlock.NewZExt(isVoid, types.I32)

		return result, exprBlock, nil

	case *ast.BlockExpression:
		var lastValue value.Value
		currentBlock := block

		// Generate code for each expression in sequence
		for _, expr := range e.Expressions {
			var err error
			lastValue, currentBlock, err = g.generateExpression(currentBlock, expr)
			if err != nil {
				return nil, currentBlock, err
			}
		}

		// Return the value of the last expression
		return lastValue, currentBlock, nil

	case *ast.Assignment:
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

		// Store the new value
		valueBlock.NewStore(value, varPtr)

		// Assignment returns the assigned value
		return value, valueBlock, nil

	default:
		return nil, nil, fmt.Errorf("unsupported expression type: %T", expr)
	}
}

// getMethodIndex returns the index of a method in the vtable
func (g *CodeGenerator) getMethodIndex(methodName string) int64 {
	// TODO: implement proper method indexing based on class hierarchy
	switch methodName {
	case "main":
		return 0
	default:
		return 0 // For now, return 0 for all methods
	}
}

func (g *CodeGenerator) getOrCreateStringConstant(name string) *ir.Global {
	// Create a unique name for the string constant
	strConstName := fmt.Sprintf(".str.%s", name)

	// Check if it already exists
	for _, global := range g.module.Globals {
		if global.Name() == strConstName {
			return global
		}
	}

	// Create new string constant if it doesn't exist
	strConst := g.module.NewGlobalDef(strConstName,
		constant.NewCharArray(append([]byte(name), 0))) // null-terminated
	return strConst
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

func (g *CodeGenerator) BuildClassTypesFromTable() error {
	for className, info := range g.classTable {
		// Start with vtable pointer
		fields := []types.Type{types.NewPointer(types.I8)}

		// Add all attributes (inherited and local)
		for _, attr := range info.Attributes {
			var fieldType types.Type
			switch strings.ToLower(attr.Type) {
			case "int":
				fieldType = types.I32
			case "bool":
				fieldType = types.I1
			default:
				// For class types, use pointer to the class struct type
				if classType, exists := g.classTypes[attr.Type]; exists {
					fieldType = types.NewPointer(classType)
				} else {
					fieldType = types.NewPointer(types.I8)
				}
			}
			fields = append(fields, fieldType)
		}

		classType := types.NewStruct(fields...)
		g.classTypes[className] = classType
	}
	return nil
}

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

// isSubclass checks if subclass is a descendant of superclass
func (g *CodeGenerator) isSubclass(subclass, superclass string) bool {
	current := subclass
	for current != "" {
		if current == superclass {
			return true
		}
		info, exists := g.classTable[current]
		if !exists {
			return false
		}
		current = info.Parent
	}
	return false
}

// getMethodIndexFor looks up the vtable slot for a given method in the specified class.
func (g *CodeGenerator) getMethodIndexFor(receiverClass, methodName string) int64 {
	if classInfo, ok := g.classTable[receiverClass]; ok {
		if methodInfo, ok := classInfo.Methods[methodName]; ok {
			return int64(methodInfo.Index)
		}
	}
	// Fallback to 0 if not found.
	return 0
}
func (g *CodeGenerator) addBuiltInClasses(program *ast.Program) {
	// Create built-in IO class with its methods.
	ioClass := &ast.Class{
		Token:  lexer.Token{Literal: "class"},
		Name:   &ast.ObjectIdentifier{Token: lexer.Token{Literal: "IO"}, Value: "IO"},
		Parent: &ast.TypeIdentifier{Token: lexer.Token{Literal: "inherits"}, Value: "Object"},
		Features: []ast.Feature{
			// out_string method
			&ast.Method{
				Token: lexer.Token{Literal: "method"},
				Name:  &ast.ObjectIdentifier{Token: lexer.Token{Literal: "out_string"}, Value: "out_string"},
				Parameters: []*ast.Formal{
					{
						Name: &ast.ObjectIdentifier{Token: lexer.Token{Literal: "x"}, Value: "x"},
						Type: &ast.TypeIdentifier{Token: lexer.Token{Literal: "String"}, Value: "String"},
					},
				},
				ReturnType: &ast.TypeIdentifier{Token: lexer.Token{Literal: "SELF_TYPE"}, Value: "SELF_TYPE"},
				Body:       nil,
			},
			// Add out_int method
			&ast.Method{
				Token: lexer.Token{Literal: "method"},
				Name:  &ast.ObjectIdentifier{Token: lexer.Token{Literal: "out_int"}, Value: "out_int"},
				Parameters: []*ast.Formal{
					{
						Name: &ast.ObjectIdentifier{Token: lexer.Token{Literal: "x"}, Value: "x"},
						Type: &ast.TypeIdentifier{Token: lexer.Token{Literal: "Int"}, Value: "Int"},
					},
				},
				ReturnType: &ast.TypeIdentifier{Token: lexer.Token{Literal: "SELF_TYPE"}, Value: "SELF_TYPE"},
				Body:       nil,
			},
			
		},
	}

	// Prepend IO to the program classes.
	program.Classes = append([]*ast.Class{ioClass}, program.Classes...)

	// Initialize Object class.
	g.classTable["Object"] = &ClassInfo{
		Name:       "Object",
		Parent:     "",
		Attributes: []AttributeInfo{},
		Methods:    make(map[string]MethodInfo),
	}
	g.classTypes["Object"] = types.NewStruct(types.NewPointer(types.I8))

	// Initialize IO class as a subclass of Object.
	g.classTable["IO"] = &ClassInfo{
		Name:       "IO",
		Parent:     "Object",
		Attributes: []AttributeInfo{},
		Methods:    make(map[string]MethodInfo),
	}
	g.classTypes["IO"] = types.NewStruct(types.NewPointer(types.I8))

	// Register both built-in methods
	g.classTable["IO"].Methods["out_string"] = MethodInfo{
		Name:  "IO_out_string",
		Index: 0,
	}
	g.classTable["IO"].Methods["out_int"] = MethodInfo{
		Name:  "IO_out_int",
		Index: 1, // Next available index
	}
}
