package code_gen

import (
	"fmt"

	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/types"
)

// globalExists checks whether a global with the given name exists in the module.
func globalExists(mod *ir.Module, name string) bool {
	for _, g := range mod.Globals {
		if g.Name() == name {
			return true
		}
	}
	return false
}

// DefineGlobalVtables creates a global constant vtable for each class.
// Each vtable is defined as a constant array of function pointers (i8*),
// and we ensure that we do not redefine an existing global.
func DefineGlobalVtables1(mod *ir.Module, vt *VtableTable, ct *ClassTable) error {
	for className, layout := range vt.Layouts {
		var elems []constant.Constant
		for _, entry := range layout.Entries {
			implementingClass := findImplementingClass(entry, className, ct)
			if implementingClass == "" {
				return fmt.Errorf("cannot find implementing class for method %s in class %s", entry.MethodName, className)
			}

			methodFullName := implementingClass + "_" + entry.MethodName
			fn, ok := methodFuncs[methodFullName]
			if !ok {
				return fmt.Errorf("no function found for \"%s\"", methodFullName)
			}

			// Cast the function pointer to i8*.
			casted := constant.NewBitCast(fn, types.NewPointer(types.I8))
			elems = append(elems, casted)
		}

		// Each element is of type i8*. Build a constant array.
		elemType := types.NewPointer(types.I8)
		arrType := types.NewArray(uint64(len(elems)), elemType)
		arrConst := constant.NewArray(arrType, elems...)

		globalName := className + "_vtable"
		if globalExists(mod, globalName) {
			// Already defined: skip.
			continue
		}
		g := mod.NewGlobalDef(globalName, arrConst)
		g.Immutable = true
	}
	return nil
}

func DefineGlobalVtables(mod *ir.Module, vt *VtableTable, ct *ClassTable) error {
	// Track all vtables we've defined
	definedVtables := make(map[string]*ir.Global)

	// First define empty vtables for basic classes
	basicClasses := []string{"Object", "IO", "String", "Int", "Bool"}
	for _, className := range basicClasses {
		globalName := className + "_vtable"
		if definedVtables[globalName] != nil {
			continue
		}

		// Create empty vtable
		elemType := types.NewPointer(types.I8)
		arrType := types.NewArray(0, elemType)
		arrConst := constant.NewArray(arrType)
		g := mod.NewGlobalDef(globalName, arrConst)
		g.Immutable = true
		definedVtables[globalName] = g
	}

	// Then define vtables for user classes
	for className, layout := range vt.Layouts {
		globalName := className + "_vtable"
		if definedVtables[globalName] != nil {
			continue
		}

		// Build all entries first
		entries := make([]constant.Constant, len(layout.Entries))
		for i, entry := range layout.Entries {
			methodFullName := entry.ImplementingClass + "_" + entry.MethodName
			fn, ok := methodFuncs[methodFullName]
			if !ok {
				return fmt.Errorf("no function found for \"%s\"", methodFullName)
			}

			// Create the bitcast with explicit type checking
			if fn.Sig == nil {
				return fmt.Errorf("function %s has no signature", methodFullName)
			}
			entries[i] = constant.NewBitCast(fn, types.NewPointer(types.I8))
		}

		// Create the vtable array in one atomic operation
		elemType := types.NewPointer(types.I8)
		arrType := types.NewArray(uint64(len(entries)), elemType)
		vtableConst := constant.NewArray(arrType, entries...)

		// Define the global vtable
		g := mod.NewGlobalDef(globalName, vtableConst)
		g.Immutable = true
		definedVtables[globalName] = g
	}

	return nil
}

// findImplementingClass traverses the inheritance chain starting at currentClass,
// returning the first class that defines the method named entry.MethodName.
func findImplementingClass(entry VtableEntry, currentClass string, ct *ClassTable) string {
	cName := currentClass
	for cName != "" {
		cInfo := ct.Classes[cName]
		if cInfo == nil {
			return ""
		}
		if _, ok := cInfo.Methods[entry.MethodName]; ok {
			return cName
		}
		if cInfo.Parent == "" || cInfo.Parent == "Object" {
			break
		}
		cName = cInfo.Parent
	}
	return ""
}
