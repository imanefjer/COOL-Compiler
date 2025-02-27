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
		
		if e.Value {
			return constant.NewInt(types.I1, 1), block, nil
		}
		return constant.NewInt(types.I1, 0), block, nil

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
		// This should not happen with well-typed COOL programs
		return constant.NewNull(types.NewPointer(types.I8)), endBlock, nil
	}
}

func (g *CodeGenerator) generateStringLiteral(block *ir.Block, e *ast.StringLiteral) (value.Value, *ir.Block, error) {
	// Ensure null termination
	strWithNull := e.Value + "\x00"
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
		case "=":
			cmp := rightBlock.NewICmp(enum.IPredEQ, left, right)
			return rightBlock.NewZExt(cmp, types.I32), rightBlock, nil
		default:
			return nil, rightBlock, fmt.Errorf("unsupported operator: %s", e.Operator)
		}
}

func (g *CodeGenerator) generateIfExpression(block *ir.Block, e *ast.IfExpression) (value.Value, *ir.Block, error) {

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
}

func (g *CodeGenerator) generateLetExpression(block *ir.Block, e *ast.LetExpression) (value.Value, *ir.Block, error) {
		llvmType := g.convertType(e.Type.Value)
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

		// NEW: Record the variable's type in localsTypes
		oldType, typeExists := g.localsTypes[e.Name.Value]
		g.localsTypes[e.Name.Value] = e.Type.Value

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

		// NEW: Restore the old type
		if typeExists {
			g.localsTypes[e.Name.Value] = oldType
		} else {
			delete(g.localsTypes, e.Name.Value)
		}

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

	objPtr := block.NewAlloca(classType)
	objPtr_i8 := block.NewBitCast(objPtr, i8Ptr)
	// Use the computed object size 
	size := constant.NewInt(types.I64, int64(info.ObjectSize*8))

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
			lastValue, currentBlock, err = g.generateExpression(currentBlock, expr)
			if err != nil {
				return nil, currentBlock, err
			}
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

// Extract method call handling to a separate function
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
	
	// Handle regular method calls
	return g.generateRegularMethodCall(block, e, receiverObj, receiverType)
}
