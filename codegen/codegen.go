package codegen

import (
	"cool-compiler/ast"
	"fmt"

	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/enum"
	"github.com/llir/llvm/ir/types"
	"github.com/llir/llvm/ir/value"
)

type CodeGenerator struct {
	module       *ir.Module
	classTypes   map[string]*types.StructType
	vtables      map[string]*ir.Global
	methods      map[string]*ir.Func
	locals       map[string]value.Value
	currentClass string              // Track current class during code generation
	classAttrs   map[string][]string // Track class attributes
}

func NewCodeGenerator() *CodeGenerator {
	return &CodeGenerator{
		module:     ir.NewModule(),
		classTypes: make(map[string]*types.StructType),
		vtables:    make(map[string]*ir.Global),
		methods:    make(map[string]*ir.Func),
		locals:     make(map[string]value.Value),
		classAttrs: make(map[string][]string),
	}
}

// Generate is the main entry point for code generation
func (g *CodeGenerator) Generate(program *ast.Program) (*ir.Module, error) {
	// First pass: declare all classes and methods
	if err := g.declareClasses(program.Classes); err != nil {
		return nil, err
	}

	// Second pass: initialize vtables
	if err := g.initializeVTables(program.Classes); err != nil {
		return nil, err
	}

	// Third pass: generate method implementations
	for _, class := range program.Classes {
		if err := g.generateClass(class); err != nil {
			return nil, err
		}
	}

	// Finally, generate the program entry point
	if err := g.generateProgramEntryPoint(); err != nil {
		return nil, err
	}

	fmt.Println(g.module.String())
	return g.module, nil
}

// declareClasses creates the LLVM types for all classes
func (g *CodeGenerator) declareClasses(classes []*ast.Class) error {
	// Create Object type first
	objectType := types.NewStruct(
		types.NewPointer(types.I8), // vtable pointer as i8*
	)
	g.classTypes["Object"] = objectType

	// Declare all class types
	for _, class := range classes {
		var attrs []string
		for _, feature := range class.Features {
			if attr, ok := feature.(*ast.Attribute); ok {
				attrs = append(attrs, attr.Name.Value)
			}
		}
		g.classAttrs[class.Name.Value] = attrs
	}
	// Create class types with attributes
	for _, class := range classes {
		// Start with vtable pointer
		fields := []types.Type{types.NewPointer(types.I8)}

		// Add attribute fields (all as i32 for simplicity)
		for range g.classAttrs[class.Name.Value] {
			fields = append(fields, types.I32)
		}

		classType := types.NewStruct(fields...)
		g.classTypes[class.Name.Value] = classType
	}
	// Declare methods first
	for _, class := range classes {
		for _, feature := range class.Features {
			if method, ok := feature.(*ast.Method); ok {
				methodName := fmt.Sprintf("%s_%s", class.Name.Value, method.Name.Value)
				// Use i8* for self parameter type
				paramTypes := []types.Type{types.NewPointer(types.NewStruct(types.NewPointer(types.I8)))}
				fn := g.module.NewFunc(methodName, types.I32, ir.NewParam("self", paramTypes[0]))
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
	// Create the C main function
	mainFn := g.module.NewFunc("main", types.I32)
	block := mainFn.NewBlock("entry")

	// Create new Main object
	mainType := g.classTypes["Main"]
	mainObj := block.NewAlloca(mainType)

	// Initialize vtable pointer
	vtablePtr := block.NewGetElementPtr(mainType, mainObj,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0))

	// Cast vtable to correct type before storing
	castedVtable := block.NewBitCast(g.vtables["Main"], types.NewPointer(types.I8))
	block.NewStore(castedVtable, vtablePtr)

	// Call Main_main method
	mainMethod := g.methods["Main_main"]
	result := block.NewCall(mainMethod, mainObj)

	// Return the result
	block.NewRet(result)
	return nil
}

// generateMethod creates an LLVM function for a COOL method
func (g *CodeGenerator) generateMethod(className string, method *ast.Method) error {
	methodName := fmt.Sprintf("%s_%s", className, method.Name.Value)
	fn := g.methods[methodName]

	// Create entry block
	entryBlock := fn.NewBlock("entry")

	// Initialize class attributes
	self := fn.Params[0]
	classType := g.classTypes[className]

	// Get attribute indices from classAttrs
	for i, attrName := range g.classAttrs[className] {
		// Get pointer to attribute field (i+1 to skip vtable)
		attrPtr := entryBlock.NewGetElementPtr(classType, self,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, int64(i+1))) // +1 to skip vtable

		// Add to locals map
		g.locals[attrName] = attrPtr
	}

	// Generate method body
	value, currentBlock, err := g.generateExpression(entryBlock, method.Body)
	if err != nil {
		return err
	}

	// Return the value
	currentBlock.NewRet(value)
	return nil
}

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
		// Generate condition
		cond, currentBlock, err := g.generateExpression(block, e.Condition)
		if err != nil {
			return nil, nil, err
		}

		// Convert condition to i1 if necessary
		if cond.Type() != types.I1 {
			cond = currentBlock.NewICmp(enum.IPredNE, cond, constant.NewInt(types.I32, 0))
		}

		// Create blocks
		thenBlock := currentBlock.Parent.NewBlock("if.then")
		elseBlock := currentBlock.Parent.NewBlock("if.else")
		mergeBlock := currentBlock.Parent.NewBlock("if.merge")

		// Branch based on condition
		currentBlock.NewCondBr(cond, thenBlock, elseBlock)

		// Generate then branch
		thenVal, thenCurrentBlock, err := g.generateExpression(thenBlock, e.Consequence)
		if err != nil {
			return nil, nil, err
		}
		thenCurrentBlock.NewBr(mergeBlock)

		// Generate else branch
		elseVal, elseCurrentBlock, err := g.generateExpression(elseBlock, e.Alternative)
		if err != nil {
			return nil, nil, err
		}
		elseCurrentBlock.NewBr(mergeBlock)

		// Create phi node in merge block
		phi := mergeBlock.NewPhi(ir.NewIncoming(thenVal, thenBlock), ir.NewIncoming(elseVal, elseBlock))

		// Return the phi value and the merge block as the current block
		return phi, mergeBlock, nil

	case *ast.LetExpression:
		// Create alloca for the variable in the entry block
		varAlloca := block.NewAlloca(types.I32)

		// Generate initialization value
		initValue, initBlock, err := g.generateExpression(block, e.Init)
		if err != nil {
			return nil, initBlock, err
		}

		// Store the initialization value
		initBlock.NewStore(initValue, varAlloca)

		// Save the old value if the variable already exists
		oldValue, exists := g.locals[e.Name.Value]

		// Add variable to symbol table
		g.locals[e.Name.Value] = varAlloca

		// Generate the body expression with the new variable in scope
		bodyValue, bodyBlock, err := g.generateExpression(initBlock, e.Body)
		if err != nil {
			return nil, bodyBlock, err
		}

		// Restore the old value or remove the variable
		if exists {
			g.locals[e.Name.Value] = oldValue
		} else {
			delete(g.locals, e.Name.Value)
		}

		return bodyValue, bodyBlock, nil

	case *ast.ObjectIdentifier:
		// First check local variables
		if varAlloca, exists := g.locals[e.Value]; exists {
			loadInst := block.NewLoad(types.I32, varAlloca)
			return loadInst, block, nil
		}

		// Check if it's a class attribute
		if g.currentClass != "" {
			// Get attribute index
			attrs := g.classAttrs[g.currentClass]
			index := -1
			for i, name := range attrs {
				if name == e.Value {
					index = i
					break
				}
			}

			if index != -1 {
				// self is the first parameter
				self := block.Parent.Params[0]

				// Getelementptr to access the attribute
				attrPtr := block.NewGetElementPtr(
					g.classTypes[g.currentClass],
					self,
					constant.NewInt(types.I32, 0),
					constant.NewInt(types.I32, int64(index+1)), // +1 to skip vtable
				)

				// Load the attribute value
				loadInst := block.NewLoad(types.I32, attrPtr)
				return loadInst, block, nil
			}
		}
		return nil, block, fmt.Errorf("undefined variable: %s", e.Value)

	case *ast.WhileExpression:
		// Create blocks for condition, body, and merge
		condBlock := block.Parent.NewBlock("while.cond")
		bodyBlock := block.Parent.NewBlock("while.body")
		mergeBlock := block.Parent.NewBlock("while.end")

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
		// Generate receiver object code (self if no explicit receiver)
		var receiver value.Value
		var receiverBlock *ir.Block
		var err error

		if e.Object != nil {
			receiver, receiverBlock, err = g.generateExpression(block, e.Object)
			if err != nil {
				return nil, receiverBlock, err
			}
		} else {
			// Use self parameter if no explicit receiver
			receiver = block.Parent.Params[0]
			receiverBlock = block
		}

		// Generate argument expressions
		args := make([]value.Value, 0, len(e.Arguments))
		currentBlock := receiverBlock

		for _, arg := range e.Arguments {
			argValue, argBlock, err := g.generateExpression(currentBlock, arg)
			if err != nil {
				return nil, argBlock, err
			}
			args = append(args, argValue)
			currentBlock = argBlock
		}

		// Get vtable pointer from object
		vtablePtr := currentBlock.NewGetElementPtr(receiver.Type(), receiver,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, 0))

		// Load vtable pointer
		vtable := currentBlock.NewLoad(types.NewPointer(types.I8), vtablePtr)

		// Get method pointer from vtable
		methodIndex := g.getMethodIndex(e.Method.Value) // You'll need to implement this
		methodPtr := currentBlock.NewGetElementPtr(vtable.Type(), vtable,
			constant.NewInt(types.I32, methodIndex))

		// Load method pointer
		method := currentBlock.NewLoad(types.NewPointer(types.I8), methodPtr)

		// Cast method pointer to correct function type
		funcType := types.NewPointer(types.NewFunc(types.I32, types.I8Ptr))
		methodFunc := currentBlock.NewBitCast(method, funcType)

		// Call the method with receiver and arguments
		call := currentBlock.NewCall(methodFunc, append([]value.Value{receiver}, args...)...)
		return call, currentBlock, nil

	case *ast.FunctionCall:
		// Generate argument expressions
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
		// Handle predefined types
		switch e.Type.Value {
		case "Int":
			// For Int, allocate an i32 and initialize to 0
			intAlloc := block.NewAlloca(types.I32)
			block.NewStore(constant.NewInt(types.I32, 0), intAlloc)
			// Load the value before returning
			return block.NewLoad(types.I32, intAlloc), block, nil
		case "Bool":
			// For Bool, allocate an i1 and initialize to false
			boolAlloc := block.NewAlloca(types.I1)
			block.NewStore(constant.NewInt(types.I1, 0), boolAlloc)
			// Load the value before returning
			return block.NewLoad(types.I1, boolAlloc), block, nil
		default:
			// Handle user-defined classes
			classType := g.classTypes[e.Type.Value]
			if classType == nil {
				return nil, block, fmt.Errorf("undefined type: %s", e.Type.Value)
			}

			// Allocate memory for the object
			objPtr := block.NewAlloca(classType)

			// Get the vtable for this class
			vtable := g.vtables[e.Type.Value]
			if vtable == nil {
				return nil, block, fmt.Errorf("vtable not found for type: %s", e.Type.Value)
			}

			// Initialize vtable pointer
			vtablePtr := block.NewGetElementPtr(classType, objPtr,
				constant.NewInt(types.I32, 0),
				constant.NewInt(types.I32, 0))

			// Cast vtable to correct type and store
			castedVtable := block.NewBitCast(vtable, types.NewPointer(types.I8))
			block.NewStore(castedVtable, vtablePtr)

			return objPtr, block, nil
		}

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
