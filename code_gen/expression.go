package code_gen

import (
	"cool-compiler/ast"
	"fmt"
	"sort"

	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/enum"
	"github.com/llir/llvm/ir/types"
	"github.com/llir/llvm/ir/value"
)

var functionFuncs = make(map[string]*ir.Func)

type Env map[string]value.Value

// generateExpression generates LLVM IR for a COOL expression.
// It returns the computed value, the current basic block (i.e. where control should continue),
// and an error, if any.
var newObjCounter int = 0

func generateExpression(block *ir.Block, expr ast.Expression, className string, ct *ClassTable, mod *ir.Module, env Env) (value.Value, *ir.Block, error) {
	switch e := expr.(type) {
	case *ast.IntegerLiteral:
		return constant.NewInt(types.I32, int64(e.Value)), block, nil

	case *ast.StringLiteral:
		// Create a global string constant.
		strConst := constant.NewCharArrayFromString(e.Value + "\x00")
		global := block.NewAlloca(types.NewArray(uint64(len(e.Value)+1), types.I8))
		global.SetName(fmt.Sprintf(".str.%d", len(e.Value)))
		block.NewStore(strConst, global)
		val := block.NewGetElementPtr(
			types.NewArray(uint64(len(e.Value)+1), types.I8),
			global,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, 0),
		)
		return val, block, nil

	case *ast.BooleanLiteral:
		return constant.NewInt(types.I1, map[bool]int64{false: 0, true: 1}[e.Value]), block, nil

	case *ast.ObjectIdentifier:
		if e.Value == "self" {
			if selfAlloca, ok := env["self"]; ok {
				_, ok := selfAlloca.Type().(*types.PointerType)
				if !ok {
					return nil, block, fmt.Errorf("expected pointer type for self")
				}
				val := selfAlloca
				return val, block, nil
			}
			return nil, block, fmt.Errorf("no 'self' parameter found")
		}
		// First, try the local environment.
		if val, ok := env[e.Value]; ok {
			pt, ok := val.Type().(*types.PointerType)
			if !ok {
				return nil, block, fmt.Errorf("expected pointer type for variable %s", e.Value)
			}
			loaded := block.NewLoad(pt.ElemType, val)
			return loaded, block, nil
		}
		// Not found locally: treat as attribute access on self.
		selfAlloca, ok := env["self"]
		if !ok {
			return nil, block, fmt.Errorf("no 'self' parameter found for attribute access")
		}
		selfPtr := selfAlloca
		classStruct, ok := llvmClassTypes[className]
		if !ok {
			return nil, block, fmt.Errorf("class %s not defined", className)
		}
		idx, err := getAttributeIndex(className, e.Value, ct)
		if err != nil {
			return nil, block, fmt.Errorf("undefined variable: %s", e.Value)
		}
		gep := block.NewGetElementPtr(
			classStruct,
			selfPtr,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, int64(idx+1)), // +1 to skip the vtable.
		)
		fieldType := classStruct.Fields[idx+1]
		loadedField := block.NewLoad(fieldType, gep)
		return loadedField, block, nil

	case *ast.Assignment:
		rhs, currBlock, err := generateExpression(block, e.Expression, className, ct, mod, env)
		if err != nil {
			return nil, currBlock, err
		}

		// Look up a local variable.
		if varAlloc, exists := lookupVariable(currBlock, e.Name.Value); exists {
			currBlock.NewStore(rhs, varAlloc)
			return rhs, currBlock, nil
		}

		// Otherwise, treat it as an attribute assignment.
		// Get 'self' from the environment.
		selfAlloca, ok := env["self"]
		if !ok {
			return nil, currBlock, fmt.Errorf("assignment to attribute %s: no 'self' parameter found", e.Name.Value)
		}

		// Use 'selfAlloca' directly as a pointer to the struct
		selfPtr := selfAlloca

		classStruct, ok := llvmClassTypes[className]
		if !ok {
			return nil, currBlock, fmt.Errorf("class %s not defined", className)
		}

		idx, err := getAttributeIndex(className, e.Name.Value, ct)
		if err != nil {
			return nil, currBlock, fmt.Errorf("assignment: undefined attribute %s", e.Name.Value)
		}

		// Create GEP to the specific attribute field
		gep := currBlock.NewGetElementPtr(
			classStruct, // Element type
			selfPtr,     // Pointer to the struct
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, int64(idx+1)), // +1 to skip the vtable.
		)

		// Store the rhs value into the field
		currBlock.NewStore(rhs, gep)

		return rhs, currBlock, nil

	case *ast.MethodCall:
		fmt.Printf("Processing method call %s\n", e.Method.Value)
		var receiver value.Value
		var receiverClass string
		var err error
		currBlock := block
		if e.Object != nil {
			receiver, currBlock, err = generateExpression(block, e.Object, className, ct, mod, env)
			if err != nil {
				return nil, currBlock, err
			}
			if objId, ok := e.Object.(*ast.ObjectIdentifier); ok {
				if varType, exists := variableTypes[objId.Value]; exists {
					receiverClass = varType
				} else {
					// Fallback: look up the attribute in the current class's attributes.
					sym, exists := ct.Classes[className].Attributes[objId.Value]
					if exists {
						receiverClass = sym.Type.Value
					} else {
						return nil, currBlock, fmt.Errorf("undefined attribute: %s", objId.Value)
					}
				}
			}
		} else {
			receiver = block.Parent.Params[0]
			receiverClass = className
			currBlock = block
		}
		args := []value.Value{receiver}
		for _, arg := range e.Arguments {
			var argVal value.Value
			argVal, currBlock, err = generateExpression(currBlock, arg, className, ct, mod, env)
			if err != nil {
				return nil, currBlock, err
			}
			args = append(args, argVal)
		}
		dispatchClass := receiverClass
		if e.Type != nil {
			dispatchClass = e.Type.Value
		}
		fmt.Printf("Looking for method %s in class %s\n", e.Method.Value, dispatchClass)
		methodName := dispatchClass + "_" + e.Method.Value
		fn := methodFuncs[methodName]
		if fn == nil {
			fmt.Printf("Method %s not found in class %s, searching parents\n", e.Method.Value, dispatchClass)
			visited := make(map[string]bool)
			currentClass := ct.Classes[dispatchClass]
			for currentClass != nil && currentClass.Parent != "" && !visited[currentClass.Name] {
				fmt.Printf("  Checking class %s\n", currentClass.Name)
				visited[currentClass.Name] = true
				parentMethodName := currentClass.Parent + "_" + e.Method.Value
				fmt.Printf("  Looking for method %s\n", parentMethodName)
				if fn = methodFuncs[parentMethodName]; fn != nil {
					fmt.Printf("  Found method in parent class %s\n", currentClass.Parent)
					break
				}
				currentClass = ct.Classes[currentClass.Parent]
			}
		}
		if fn == nil {
			return nil, currBlock, fmt.Errorf("undefined method: %s (class: %s)", e.Method.Value, dispatchClass)
		}
		callResult := currBlock.NewCall(fn, args...)
		var methodReturnType string
		if currentClass := ct.Classes[dispatchClass]; currentClass != nil {
			if method := currentClass.Methods[e.Method.Value]; method != nil {
				methodReturnType = method.ReturnType.Value
			}
		}
		if methodReturnType == "Int" {
			return callResult, currBlock, nil
		}
		return callResult, currBlock, nil

	case *ast.BlockExpression:
		currBlock := block
		var lastVal value.Value
		for _, expr := range e.Expressions {
			var err error
			lastVal, currBlock, err = generateExpression(currBlock, expr, className, ct, mod, env)
			if err != nil {
				return nil, currBlock, err
			}
		}
		return lastVal, currBlock, nil

	case *ast.IfExpression:
		// Create new blocks for then, else, and merge.
		thenBlock := block.Parent.NewBlock("if.then")
		elseBlock := block.Parent.NewBlock("if.else")
		mergeBlock := block.Parent.NewBlock("if.end")
		cond, currBlock, err := generateExpression(block, e.Condition, className, ct, mod, env)
		if err != nil {
			return nil, currBlock, err
		}
		// End current block with a conditional branch.
		block.NewCondBr(cond, thenBlock, elseBlock)
		thenVal, thenCurrent, err := generateExpression(thenBlock, e.Consequence, className, ct, mod, env)
		if err != nil {
			return nil, thenCurrent, err
		}
		thenCurrent.NewBr(mergeBlock)
		elseVal, elseCurrent, err := generateExpression(elseBlock, e.Alternative, className, ct, mod, env)
		if err != nil {
			return nil, elseCurrent, err
		}
		elseCurrent.NewBr(mergeBlock)
		phi := mergeBlock.NewPhi(ir.NewIncoming(thenVal, thenBlock), ir.NewIncoming(elseVal, elseBlock))
		return phi, mergeBlock, nil

	case *ast.LetExpression:
		newEnv := make(Env)
		for k, v := range env {
			newEnv[k] = v
		}
		// Process the first binding.
		if e.Name != nil {
			fmt.Printf("Processing first binding: %s of type %s\n", e.Name.Value, e.Type.Value)
			varType := mapCoolTypeToLLVM(e.Type.Value)
			alloca := block.NewAlloca(varType)
			alloca.SetName(e.Name.Value)
			var varInit value.Value
			var err error
			if e.Init != nil {
				varInit, block, err = generateExpression(block, e.Init, className, ct, mod, newEnv)
				if err != nil {
					return nil, block, fmt.Errorf("error in let binding %s: %w", e.Name.Value, err)
				}
			} else {
				switch t := varType.(type) {
				case *types.PointerType:
					varInit = constant.NewNull(t)
				case *types.IntType:
					varInit = constant.NewInt(t, 0)
				default:
					varInit = constant.NewZeroInitializer(varType)
				}
			}
			block.NewStore(varInit, alloca)
			newEnv[e.Name.Value] = alloca
			variableTypes[e.Name.Value] = e.Type.Value
		}
		// Process additional bindings.
		for i, binding := range e.Bindings {
			fmt.Printf("  Processing binding %d: %s of type %s\n", i, binding.Name.Value, binding.Type.Value)
			varType := mapCoolTypeToLLVM(binding.Type.Value)
			alloca := block.NewAlloca(varType)
			alloca.SetName(binding.Name.Value)
			var err error
			var initVal value.Value
			initVal, block, err = generateExpression(block, binding.Init, className, ct, mod, newEnv)
			if err != nil {
				return nil, block, fmt.Errorf("error in let binding %s: %w", binding.Name.Value, err)
			}
			block.NewStore(initVal, alloca)
			newEnv[binding.Name.Value] = alloca
			variableTypes[binding.Name.Value] = binding.Type.Value
		}
		fmt.Printf("Generating let body expression\n")
		return generateExpression(block, e.Body, className, ct, mod, newEnv)

	case *ast.NewExpression:
		structType, exists := llvmClassTypes[e.Type.Value]
		if !exists {
			return nil, block, fmt.Errorf("unknown class type: %s", e.Type.Value)
		}
		newObjCounter++
		objName := fmt.Sprintf("new_obj_%d", newObjCounter)
		objAlloc := block.NewAlloca(structType)
		objAlloc.SetName(objName)
		// Initialize vtable
		vtableGlobalName := e.Type.Value + "_vtable"
		if !globalExists(mod, vtableGlobalName) {
			return nil, block, fmt.Errorf("vtable for class %s not defined", e.Type.Value)
		}
		vtableGlobal := mod.NewGlobalDef(vtableGlobalName, constant.NewNull(types.NewPointer(types.I8)))
		gep := block.NewGetElementPtr(structType, objAlloc, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
		castedVtable := block.NewBitCast(vtableGlobal, types.NewPointer(types.NewPointer(types.I8)))
		block.NewStore(castedVtable, gep)
		return objAlloc, block, nil

	case *ast.InfixExpression:
		leftVal, block, err := generateExpression(block, e.Left, className, ct, mod, env)
		if err != nil {
			return nil, block, err
		}
		rightVal, block, err := generateExpression(block, e.Right, className, ct, mod, env)
		if err != nil {
			return nil, block, err
		}
		if pt, ok := leftVal.Type().(*types.PointerType); ok {
			leftVal = block.NewLoad(pt.ElemType, leftVal)
		}
		if pt, ok := rightVal.Type().(*types.PointerType); ok {
			rightVal = block.NewLoad(pt.ElemType, rightVal)
		}
		if fnType, ok := leftVal.Type().(*types.FuncType); ok {
			leftVal = block.NewLoad(fnType.RetType, leftVal)
		}
		if fnType, ok := rightVal.Type().(*types.FuncType); ok {
			rightVal = block.NewLoad(fnType.RetType, rightVal)
		}
		if !types.IsInt(leftVal.Type()) || !types.IsInt(rightVal.Type()) {
			return nil, block, fmt.Errorf("can only compare integer values, got %v and %v", leftVal.Type(), rightVal.Type())
		}
		switch e.Operator {
		case "+":
			return block.NewAdd(leftVal, rightVal), block, nil
		case "-":
			return block.NewSub(leftVal, rightVal), block, nil
		case "*":
			return block.NewMul(leftVal, rightVal), block, nil
		case "/":
			return block.NewSDiv(leftVal, rightVal), block, nil
		case "<":
			return block.NewICmp(enum.IPredSLT, leftVal, rightVal), block, nil
		case "<=":
			return block.NewICmp(enum.IPredSLE, leftVal, rightVal), block, nil
		case "=":
			return block.NewICmp(enum.IPredEQ, leftVal, rightVal), block, nil
		default:
			return nil, block, fmt.Errorf("unsupported infix operator: %s", e.Operator)
		}

	case *ast.NegExpression:
		operand, block, err := generateExpression(block, e.Expression, className, ct, mod, env)
		if err != nil {
			return nil, block, err
		}
		return block.NewXor(operand, constant.NewInt(operand.Type().(*types.IntType), -1)), block, nil

	case *ast.FunctionCall:
		// Evaluate the argument expressions
		args := []value.Value{}
		currBlock := block
		var err error
		for _, arg := range e.Arguments {
			var argVal value.Value
			argVal, currBlock, err = generateExpression(currBlock, arg, className, ct, mod, env)
			if err != nil {
				return nil, currBlock, err
			}
			args = append(args, argVal)
		}

		// Look up the function in our global function mapping.
		// (For example, out_string's LLVM function should be registered in functionFuncs.)
		fn, ok := functionFuncs[e.Function.Value]
		if !ok || fn == nil {
			return nil, currBlock, fmt.Errorf("undefined function: %s", e.Function.Value)
		}

		// Generate the LLVM call instruction.
		call := currBlock.NewCall(fn, args...)
		return call, currBlock, nil

	default:
		return nil, block, fmt.Errorf("unsupported expression type: %T", expr)
	}
}

// getTotalAttributeCount returns the total number of attributes inherited
// by class 'className' (including those declared in that class).
func getTotalAttributeCount(className string, ct *ClassTable) (int, error) {
	ci, ok := ct.Classes[className]
	if !ok {
		return 0, fmt.Errorf("class %s not found", className)
	}
	count := len(ci.Attributes)
	if ci.Parent != "" && ci.Parent != "Object" {
		parentCount, err := getTotalAttributeCount(ci.Parent, ct)
		if err != nil {
			return 0, err
		}
		count += parentCount
	}
	return count, nil
}

// getAttributeIndex returns the index of the attribute 'attrName' in the
// overall attribute order for class 'className'. In the LLVM struct, field 0 is reserved
// for the vtable, and fields 1..N are for the attributes in parent-first order.
func getAttributeIndex(className, attrName string, ct *ClassTable) (int, error) {
	ci, ok := ct.Classes[className]
	if !ok {
		return 0, fmt.Errorf("class %s not found", className)
	}
	// First, check if the attribute is declared in this class.
	// (Assuming ci.Attributes is a map of attribute name to attribute info.)
	if _, exists := ci.Attributes[attrName]; exists {
		// Count how many attributes are inherited from parents.
		inherited := 0
		if ci.Parent != "" && ci.Parent != "Object" {
			var err error
			inherited, err = getTotalAttributeCount(ci.Parent, ct)
			if err != nil {
				return 0, err
			}
		}
		// Now, determine the index of attrName among this class's attributes.
		// Since maps are unordered, we sort the attribute names.
		keys := make([]string, 0, len(ci.Attributes))
		for k := range ci.Attributes {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		for i, k := range keys {
			if k == attrName {
				return inherited + i, nil
			}
		}
		// Should not get here.
		return 0, fmt.Errorf("attribute %s not found in class %s", attrName, className)
	}
	// If not declared in this class, check the parent.
	if ci.Parent != "" && ci.Parent != "Object" {
		return getAttributeIndex(ci.Parent, attrName, ct)
	}
	return 0, fmt.Errorf("attribute %s not found in class %s", attrName, className)
}

// Helper functions for variable management

// variableAllocations maps blocks to their symbol tables
var variableAllocations = map[*ir.Block]map[string]value.Value{}

// lookupVariable retrieves the alloca for a variable in the current block or parent blocks
func lookupVariable(block *ir.Block, name string) (value.Value, bool) {
	curr := block
	// Walk up until we reach the entry block.
	for curr != nil {
		if vars, exists := variableAllocations[curr]; exists {
			if val, found := vars[name]; found {
				return val, true
			}
		}
		// If we are already in the entry block, stop.
		entry := curr.Parent.Blocks[0]
		if curr == entry {
			break
		}
		curr = entry
	}
	// Finally, try the entry block.
	entry := block.Parent.Blocks[0]
	if vars, exists := variableAllocations[entry]; exists {
		if val, found := vars[name]; found {
			return val, true
		}
	}
	return nil, false
}

// registerVariable registers a variable's alloca in the current block's symbol table
func registerVariable(block *ir.Block, name string, alloc value.Value) {
	if _, exists := variableAllocations[block]; !exists {
		variableAllocations[block] = make(map[string]value.Value)
	}
	variableAllocations[block][name] = alloc
}

// findBinding looks up a variable's binding information
func findBinding(block *ir.Block, name string) *ast.Binding {
	// This should return the binding that includes type information
	// You'll need to track this during let expressions
	return nil // TODO: Implement this
}

// storeBinding stores binding information for later type lookups
func storeBinding(block *ir.Block, binding *ast.Binding) {
	// Implementation of storeBinding function
}

// Add at the top with other vars
var variableTypes = map[string]string{} // maps variable names to their COOL types

// IsInt returns true if the type is an integer type
func IsInt(t types.Type) bool {
	_, ok := t.(*types.IntType)
	return ok
}

func initBuiltins(mod *ir.Module) {
	// Register out_string: parameter is a pointer to i8.
	outStringFunc := mod.NewFunc("out_string", types.Void, ir.NewParam("str", types.NewPointer(types.I8)))
	functionFuncs["out_string"] = outStringFunc

	// Register out_int: parameter is an i32.
	outIntFunc := mod.NewFunc("out_int", types.Void, ir.NewParam("i", types.I32))
	functionFuncs["out_int"] = outIntFunc
}
