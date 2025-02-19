package code_gen

import (
	"cool-compiler/ast"

	"fmt"

	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/types"
)

var methodFuncs = map[string]*ir.Func{}
// Helper: getMethodOrigin traverses up the class hierarchy to determine where method mName was originally defined. 
func getMethodOrigin(ct *ClassTable, className, mName string) string { 
	origin := className 
	for { 
		current := ct.Classes[origin] 
		if current == nil || current.Parent == "" { 
			break 
			} 
		parent := ct.Classes[current.Parent] 
		if parent == nil { 
			break 
		} 
		// If the parent defines mName, update the origin. 
		if _, exists := parent.Methods[mName]; exists { 
			origin = parent.Name 
		} else { break } 
		} 
		return origin 
	}

func DeclareAllMethods(mod *ir.Module, ct *ClassTable) error { 
	// First declare Object's basic methods.
	if err := declareObjectMethods(mod); err != nil {
		return err
	}
	// Then declare all user-defined methods.
for className, cInfo := range ct.Classes {
	fmt.Printf("Declaring methods for class: %s\n", className)
	for mName, method := range cInfo.Methods {
		// Determine the origin of the method.
		originClass := getMethodOrigin(ct, className, mName)
		fullName := className + "_" + mName
		fmt.Printf("  Declaring method: %s (origin: %s)\n", fullName, originClass)

		returnType := mapCoolTypeToLLVM(method.ReturnType.Value)
		// Use the original class's self pointer type.
		selfType := types.NewPointer(llvmClassTypes[originClass])
		paramTypes := []types.Type{selfType} // 'self' parameter

		for _, formal := range method.Parameters {
			paramTypes = append(paramTypes, mapCoolTypeToLLVM(formal.Type.Value))
		}

		fnType := types.NewFunc(returnType, paramTypes...)
		fn := mod.NewFunc(fullName, fnType)

		// Populate function parameters if not already present.
		if len(fn.Params) == 0 {
			expected := len(paramTypes)
			fn.Params = make([]*ir.Param, expected)
			for i := 0; i < expected; i++ {
				fn.Params[i] = ir.NewParam("", paramTypes[i])
			}
		}

		// Name the parameters.
		if len(fn.Params) > 0 {
			fn.Params[0].SetName("self")
			for i, formal := range method.Parameters {
				if i+1 < len(fn.Params) {
					fn.Params[i+1].SetName(formal.Name.Value)
				}
			}
		}
		for i, param := range fn.Params {
			fmt.Printf("Parameter %d: %q of type %s\n", i, param.Name(), param.Type())
		}

		methodFuncs[fullName] = fn
	}
}
return nil
}
func DeclareAllMethods2(mod *ir.Module, ct *ClassTable) error {
	// First declare Object's basic methods.
	if err := declareObjectMethods(mod); err != nil {
		return err
	}

	// Then declare all user-defined methods.
	for className, cInfo := range ct.Classes {
		fmt.Printf("Declaring methods for class: %s\n", className)
		for mName, method := range cInfo.Methods {
			fullName := className + "_" + mName
			fmt.Printf("  Declaring method: %s\n", fullName)

			returnType := mapCoolTypeToLLVM(method.ReturnType.Value)
			paramTypes := []types.Type{types.NewPointer(llvmClassTypes[className])} // 'self' parameter

			for _, formal := range method.Parameters {
				paramTypes = append(paramTypes, mapCoolTypeToLLVM(formal.Type.Value))
			}

			fnType := types.NewFunc(returnType, paramTypes...)
			fn := mod.NewFunc(fullName, fnType)

			// Manually populate fn.Params from the function signature if not already present.
			if len(fn.Params) == 0 {
				expected := len(paramTypes)
				fn.Params = make([]*ir.Param, expected)
				for i := 0; i < expected; i++ {
					fn.Params[i] = ir.NewParam("", paramTypes[i])
				}
			}

			// Name the parameters.
			if len(fn.Params) > 0 {
				fn.Params[0].SetName("self")
				for i, formal := range method.Parameters {
					if i+1 < len(fn.Params) {
						fn.Params[i+1].SetName(formal.Name.Value)
					}
				}
			}
			for i, param := range fn.Params {
				fmt.Printf("Parameter %d: %q of type %s\n", i, param.Name(), param.Type())
			}

			methodFuncs[fullName] = fn
		}
	}
	return nil
}

// declareObjectMethods declares the basic Object class methods
func declareObjectMethods1(mod *ir.Module) error {
	fmt.Println("Declaring Object methods")

	// Make sure Object type exists
	if _, exists := llvmClassTypes["Object"]; !exists {
		llvmClassTypes["Object"] = types.NewStruct()
	}

	// abort() : Object
	abortTy := types.NewPointer(llvmClassTypes["Object"])
	abort := mod.NewFunc("Object_abort", abortTy,
		ir.NewParam("self", types.NewPointer(llvmClassTypes["Object"])))
	methodFuncs["Object_abort"] = abort

	// type_name() : String
	typeNameTy := types.NewPointer(llvmClassTypes["String"])
	typeName := mod.NewFunc("Object_type_name", typeNameTy,
		ir.NewParam("self", types.NewPointer(llvmClassTypes["Object"])))
	methodFuncs["Object_type_name"] = typeName

	// copy() : SELF_TYPE
	copyTy := types.NewPointer(llvmClassTypes["Object"])
	copy := mod.NewFunc("Object_copy", copyTy,
		ir.NewParam("self", types.NewPointer(llvmClassTypes["Object"])))
	methodFuncs["Object_copy"] = copy

	return nil
}
func declareObjectMethods(mod *ir.Module) error {
	fmt.Println("Declaring Object methods") // Instead of defaulting to an empty struct, define Object with a vtable pointer.
	if _, exists := llvmClassTypes["Object"]; !exists {
		objStruct := types.NewStruct([]types.Type{types.NewPointer(types.NewPointer(types.I8))}...)
		objStruct.SetName("Object")
		llvmClassTypes["Object"] = objStruct
	} // Now, for example, declare abort() : Object using the proper type.
	abortTy := types.NewPointer(llvmClassTypes["Object"])
	abort := mod.NewFunc("Object_abort", abortTy, ir.NewParam("self", types.NewPointer(llvmClassTypes["Object"])))
	methodFuncs["Object_abort"] = abort
	// ... similarly for type_name() and copy()
	// type_name() : String
	typeNameTy := types.NewPointer(llvmClassTypes["String"])
	typeName := mod.NewFunc("Object_type_name", typeNameTy,
		ir.NewParam("self", types.NewPointer(llvmClassTypes["Object"])))
	methodFuncs["Object_type_name"] = typeName

	// copy() : SELF_TYPE (returning a pointer to Object)
	copyTy := types.NewPointer(llvmClassTypes["Object"])
	copy := mod.NewFunc("Object_copy", copyTy,
		ir.NewParam("self", types.NewPointer(llvmClassTypes["Object"])))
	methodFuncs["Object_copy"] = copy

	return nil
}

// buildMethodParams constructs the parameter list for a method
func buildMethodParams(cName string, method *ast.Method) ([]*ir.Param, types.Type) {
	var params []*ir.Param

	// First param is always 'self'
	selfTy := llvmClassTypes[cName]
	if selfTy == nil {
		selfTy = types.NewStruct()
	}
	selfParam := ir.NewParam("tmpSelf", types.NewPointer(selfTy))
	params = append(params, selfParam)

	// Add formal parameters
	for _, formal := range method.Parameters {
		pTy := mapCoolTypeToLLVM(formal.Type.Value)
		param := ir.NewParam(formal.Name.Value, pTy)
		params = append(params, param)
	}

	// Handle return type
	var retTy types.Type
	if method.ReturnType.Value == "SELF_TYPE" {
		retTy = types.NewPointer(selfTy)
	} else {
		retTy = mapCoolTypeToLLVM(method.ReturnType.Value)
	}

	return params, retTy
}

func DefineMethodBodies(mod *ir.Module, ct *ClassTable) error {
	fmt.Println("Starting method body definitions...")
	for cName, cInfo := range ct.Classes {
		fmt.Printf("Processing class: %s\n", cName)
		for mName, method := range cInfo.Methods {
			fmt.Printf("  Defining method: %s_%s\n", cName, mName)
			fullName := cName + "_" + mName
			fn := methodFuncs[fullName]
			if fn == nil {
				fmt.Printf("  Warning: Method %s not found in methodFuncs\n", fullName)
				continue
			}

			// Clear existing blocks and create a new entry block
			fn.Blocks = fn.Blocks[:0]
			entry := fn.NewBlock("entry")

			// Create an initial environment and allocate storage for 'self'
			env := make(Env)
			if len(fn.Params) > 0 {
				env["self"] = fn.Params[0]
			}
			for i := 1; i < len(fn.Params); i++ {
				env[fn.Params[i].Name()] = fn.Params[i]
			}
			// Allocate storage for other parameters
			for i := 1; i < len(fn.Params); i++ {
				param := fn.Params[i]
				alloca := entry.NewAlloca(param.Type())
				entry.NewStore(param, alloca)
				env[param.Name()] = alloca
			}

			// Generate code for the method body, passing the environment.
			fmt.Printf("    Generating body for %s_%s\n", cName, mName)
			if err := generateMethodBody(entry, method.Body, cName, ct, env); err != nil {
				return fmt.Errorf("error in %s_%s: %w", cName, mName, err)
			}
			fmt.Printf("    Completed %s_%s\n", cName, mName)
		}
		fmt.Printf("Completed class: %s\n", cName)
	}
	fmt.Println("Method body definitions completed")
	return nil
}

//	func generateMethodBody(block *ir.Block, expr ast.Expression, className string, ct *ClassTable, env Env) error {
//		fmt.Printf("In generateMethodBody for class %s, env keys: ", className)
//		for k := range env {
//			fmt.Printf("%q ", k)
//		}
//		val, err := generateExpression(block, expr, className, ct, block.Parent.Parent, env)
//		if err != nil {
//			return err
//		}
//		block.NewRet(val)
//		return nil
//	}
func generateMethodBody(block *ir.Block, expr ast.Expression, className string, ct *ClassTable, env Env) error {
	val, currBlock, err := generateExpression(block, expr, className, ct, block.Parent.Parent, env)
	if err != nil {
		return err
	}
	// Insert the final terminator in the current (active) block.
	currBlock.NewRet(val)
	return nil
}

// methodExists checks if a method is declared
func methodExists(className, methodName string) bool {
	fullName := className + "_" + methodName
	_, exists := methodFuncs[fullName]
	return exists
}
