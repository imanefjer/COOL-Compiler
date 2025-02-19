// structs.go
package code_gen

import (
	"fmt"
	"sort"

	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/types"
)

var llvmClassTypes = map[string]*types.StructType{}

func DefineClassStructs(mod *ir.Module, ct *ClassTable) error {
	// Define Object struct first (it has no parent)
	objectFields := []types.Type{
		types.NewPointer(types.NewPointer(types.I8)), // vtable pointer
	}
	objectStruct := types.NewStruct(objectFields...)
	objectStruct.SetName("Object")
	llvmClassTypes["Object"] = objectStruct

	// Define basic class structs in order
	basicClasses := []string{"IO", "String", "Int", "Bool"}
	for _, className := range basicClasses {
		cInfo := ct.Classes[className]
		if cInfo == nil {
			return fmt.Errorf("basic class %s not found in class table", className)
		}
		fields, err := computeFieldsForClass(className, cInfo, ct)
		if err != nil {
			return fmt.Errorf("failed defining structs: %v", err)
		}
		s := types.NewStruct(fields...)
		s.SetName(className)
		llvmClassTypes[className] = s
	}

	// Then define user-defined class structs
	for cName, cInfo := range ct.Classes {
		// Skip basic classes as they're already defined
		if cName == "Object" || cName == "IO" || cName == "String" || cName == "Int" || cName == "Bool" {
			continue
		}
		fields, err := computeFieldsForClass(cName, cInfo, ct)
		if err != nil {
			return fmt.Errorf("failed defining structs: %v", err)
		}
		s := types.NewStruct(fields...)
		s.SetName(cName)
		llvmClassTypes[cName] = s
	}
	return nil
}

func computeFieldsForClass(className string, cInfo *ClassInfo, ct *ClassTable) ([]types.Type, error) {
	// Field 0: vtable pointer.
	fields := []types.Type{types.NewPointer(types.NewPointer(types.I8))}

	// If the class has a parent (other than Object), copy the parent's attribute fields.
	if cInfo.Parent != "" && cInfo.Parent != "Object" {
		parentStruct, ok := llvmClassTypes[cInfo.Parent]
		if !ok {
			return nil, fmt.Errorf("parent class %s struct not defined", cInfo.Parent)
		}
		// The parent's struct: field 0 is the vtable pointer, fields 1.. are attributes.
		parentAttrTypes := parentStruct.Fields[1:]
		fields = append(fields, parentAttrTypes...)
	}

	// Now add the attributes declared in this class.
	// To get a deterministic order, sort attribute names.
	keys := make([]string, 0, len(cInfo.Attributes))
	for k := range cInfo.Attributes {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	for _, attrName := range keys {
		fieldTy := mapCoolTypeToLLVM(cInfo.Attributes[attrName].Type.Value)
		fields = append(fields, fieldTy)
	}
	return fields, nil
}

// For demonstration.
func mapCoolTypeToLLVM1(coolType string) types.Type {
	switch coolType {
	case "Object":
		if st, ok := llvmClassTypes["Object"]; ok {
			return types.NewPointer(st)
		}
		return types.NewPointer(types.I8)
	case "Int":
		return types.I32
	case "Bool":
		return types.I1
	case "String":
		return types.NewPointer(types.I8)
	default:
		if st, ok := llvmClassTypes[coolType]; ok {
			// user-defined class => pointer to that struct
			return types.NewPointer(st)
		}
		// fallback
		return types.NewPointer(types.I8)
	}
}
func mapCoolTypeToLLVM(coolType string) types.Type {
	switch coolType {
	case "Object": // Use the defined Object struct if available.
		if st, ok := llvmClassTypes["Object"]; ok {
			return types.NewPointer(st)
		} // Otherwise, create an Object struct with a vtable pointer.
		objStruct := types.NewStruct([]types.Type{types.NewPointer(types.NewPointer(types.I8))}...)
		objStruct.SetName("Object")
		llvmClassTypes["Object"] = objStruct
		return types.NewPointer(objStruct)
	case "Int":
		return types.I32
	case "Bool":
		return types.I1
	case "String":
		if st, ok := llvmClassTypes["String"]; ok {
			return types.NewPointer(st)
		}
		return types.NewPointer(types.I8)
	default:
		if st, ok := llvmClassTypes[coolType]; ok {
			return types.NewPointer(st)
		}
		return types.NewPointer(types.I8)
	}
}
