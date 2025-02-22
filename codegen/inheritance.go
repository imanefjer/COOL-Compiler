package codegen

import (
	"cool-compiler/ast"
	"fmt"

	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/types"

	"sort"
)

// ClassTable maps a class name to its inheritance information.
type ClassTable map[string]*ClassInfo

// ClassInfo holds the layout and dispatch data for a class.
type ClassInfo struct {
	Name       string
	Parent     string
	Attributes []AttributeInfo       // Ordered attributes (inherited then local)
	Methods    map[string]MethodInfo // Method name -> method info
	ObjectSize int
}

// AttributeInfo represents an attribute with its type and offset in the object layout.
type AttributeInfo struct {
	Name   string
	Type   string
	Offset int // e.g., index 0 might be reserved for the vtable pointer.
}

// MethodInfo represents a method with its fixed slot in the vtable.
type MethodInfo struct {
	Name  string // fully qualified name: e.g., "ClassName_methodName"
	Index int    // fixed slot in the vtable
}

// BuildClassTable constructs the inheritance information from the AST.
// It assumes that semantic analysis already checked for inheritance cycles and illegal redefinitions.
func (g *CodeGenerator) BuildClassTable(classes []*ast.Class) error {
	g.classTable = make(ClassTable)

	// Create Object root class
	g.classTable["Object"] = &ClassInfo{
		Name:       "Object",
		Parent:     "",
		Attributes: []AttributeInfo{},
		Methods:    make(map[string]MethodInfo),
	}

	// First pass: create initial entries
	for _, class := range classes {
		parentName := "Object"
		if class.Parent != nil {
			parentName = class.Parent.Value
		}
		g.classTable[class.Name.Value] = &ClassInfo{
			Name:       class.Name.Value,
			Parent:     parentName,
			Attributes: []AttributeInfo{},
			Methods:    make(map[string]MethodInfo),
		}
	}

	// Process classes in inheritance order
	sortedClasses := sortClassesByInheritance(classes, g.classTable)
	for _, class := range sortedClasses {
		info := g.classTable[class.Name.Value]

		// First inherit from parent
		if parentInfo, exists := g.classTable[info.Parent]; exists {
			// Copy parent's attributes
			for _, attr := range parentInfo.Attributes {
				info.Attributes = append(info.Attributes, AttributeInfo{
					Name:   attr.Name,
					Type:   attr.Type,
					Offset: attr.Offset,
				})
			}

			// Copy parent's methods
			for name, method := range parentInfo.Methods {
				info.Methods[name] = method
			}
		}

		// Process local features
		offset := len(info.Attributes)
		methodIndex := len(info.Methods)

		for _, feature := range class.Features {
			switch f := feature.(type) {
			case *ast.Attribute:
				// Verify no redefinition
				for _, attr := range info.Attributes {
					if attr.Name == f.Name.Value {
						return fmt.Errorf("attribute %s redefined in class %s", f.Name.Value, class.Name.Value)
					}
				}
				// Add local attribute
				info.Attributes = append(info.Attributes, AttributeInfo{
					Name:   f.Name.Value,
					Type:   f.Type.Value,
					Offset: offset + 1, // +1 for vtable pointer
				})
				offset++

			case *ast.Method:
				methodName := fmt.Sprintf("%s_%s", class.Name.Value, f.Name.Value)
				if _, exists := info.Methods[f.Name.Value]; exists {
					// Override: keep same vtable slot
					info.Methods[f.Name.Value] = MethodInfo{
						Name:  methodName,
						Index: info.Methods[f.Name.Value].Index,
					}
				} else {
					// New method: assign new vtable slot
					info.Methods[f.Name.Value] = MethodInfo{
						Name:  methodName,
						Index: methodIndex,
					}
					methodIndex++
				}
			}
		}
	}
	return nil
}

// sortClassesByInheritance returns a slice of *ast.Class sorted in order such that parents come before children.
func sortClassesByInheritance(classes []*ast.Class, table ClassTable) []*ast.Class {
	// For simplicity, sort by depth in the inheritance tree.
	depths := make(map[string]int)
	var getDepth func(name string) int
	getDepth = func(name string) int {
		if d, ok := depths[name]; ok {
			return d
		}
		info, ok := table[name]
		if !ok || info.Parent == "" {
			depths[name] = 0
			return 0
		}
		d := 1 + getDepth(info.Parent)
		depths[name] = d
		return d
	}
	sorted := make([]*ast.Class, len(classes))
	copy(sorted, classes)
	sort.Slice(sorted, func(i, j int) bool {
		return getDepth(sorted[i].Name.Value) < getDepth(sorted[j].Name.Value)
	})
	return sorted
}
func (g *CodeGenerator) ComputeObjectLayouts() error {
	// Iterate over each class in the class table.
	for _, classInfo := range g.classTable {
		// Start with offset 1 since offset 0 is reserved for the vtable pointer.
		offset := 1
		for i := range classInfo.Attributes {
			classInfo.Attributes[i].Offset = offset
			offset++
		}
		// Set the computed object size.
		classInfo.ObjectSize = offset
	}
	return nil
}

func (g *CodeGenerator) ConstructVTables() error {
	fmt.Println("\n=== Debug: Constructing VTables ===")
        sortedClasses := sortClassesByInheritanceFromTable(g.classTable)
        for _, className := range sortedClasses {
            info := g.classTable[className]
    
            // Collect all methods including inherited ones
            var allMethods []MethodInfo
		currentClass := className
		for currentClass != "" {
			if ci, exists := g.classTable[currentClass]; exists {
				// Collect methods in reverse order to ensure parent methods come first
				ms := make([]MethodInfo, 0, len(ci.Methods))
				for _, m := range ci.Methods {
					ms = append(ms, m)
				}
				// Sort methods by index to maintain order
				sort.Slice(ms, func(i, j int) bool {
					return ms[i].Index < ms[j].Index
				})
				// Prepend to maintain inheritance order
				allMethods = append(ms, allMethods...)
				currentClass = ci.Parent
			} else {
				break
			}
		}
        methodMap := make(map[string]MethodInfo)
		for _, m := range allMethods {
			methodMap[m.Name] = m
		}
		// Convert the method map to a slice and sort by the stored index.
		allMethods = make([]MethodInfo, 0, len(methodMap))
		for _, m := range methodMap {
			allMethods = append(allMethods, m)
		}
		sort.Slice(allMethods, func(i, j int) bool {
			return allMethods[i].Index < allMethods[j].Index
		})

		// If no methods were found in the chain, fall back to the parent's vtable method count.
		n := uint64(len(allMethods))
		if n == 0 && info.Parent != "" {
			parentVtableGlobal := g.vtables[info.Parent]
			if parentVtableGlobal != nil {
				parentVtableType := parentVtableGlobal.Type().(*types.PointerType).ElemType.(*types.StructType)
				parentMethodArrayType := parentVtableType.Fields[2].(*types.ArrayType)
				n = parentMethodArrayType.Len
				// Also, inherit parent's methods.
				allMethods = []MethodInfo{}
				for _, m := range g.classTable[info.Parent].Methods {
					allMethods = append(allMethods, m)
				}
				sort.Slice(allMethods, func(i, j int) bool {
					return allMethods[i].Index < allMethods[j].Index
				})
			}
		}

		fmt.Printf("For class %s, computed method count = %d\n", className, n)
		// If still zero, force a dummy method.
		if n == 0 {
			dummyFn := g.module.NewFunc(className+"_dummy", types.I32, ir.NewParam("self", i8Ptr))
			dummyBlock := dummyFn.NewBlock("entry")
			dummyBlock.NewRet(constant.NewInt(types.I32, 0))
			g.methods[dummyFn.Name()] = dummyFn
			allMethods = []MethodInfo{{Name: dummyFn.Name(), Index: 0}}
			n = 1
		}

		// Determine parent's vtable pointer.
		var parentVtable constant.Constant
		if className == "Object" {
			parentVtable = constant.NewNull(i8Ptr)
		} else if info.Parent != "" {
			parentGlobal, ok := g.vtables[info.Parent]
			if !ok {
				return fmt.Errorf("parent vtable not found for class %s", className)
			}
			parentVtable = constant.NewBitCast(parentGlobal, i8Ptr)
		} else {
			parentVtable = constant.NewNull(i8Ptr)
		}

		classNameConst := g.getOrCreateStringConstant(className)
		vtableArrayType := types.NewArray(n, i8Ptr)
		vtableType := types.NewStruct(i8Ptr, i8Ptr, vtableArrayType)

		var methodTypes []types.Type
		for _, m := range allMethods {
			fn := g.methods[m.Name]
			if fn != nil {
				methodTypes = append(methodTypes, fn.Type())
			} else {
				// Fallback to dummy function type if method not found
				methodTypes = append(methodTypes, types.NewPointer(types.NewFunc(types.I32, i8Ptr)))
			}
		}


		// Prepare method constants with correct casting
		var methodConstants []constant.Constant
		for _, m := range allMethods {
			fn := g.methods[m.Name]
			if fn == nil {
				return fmt.Errorf("method %s not found in methods map", m.Name)
			}
			// Cast to i8* for vtable entry
			methodConstants = append(methodConstants, constant.NewBitCast(fn, types.NewPointer(types.I8)))
		}

		vtableInit := constant.NewStruct(vtableType,
			constant.NewBitCast(classNameConst, i8Ptr),
			parentVtable,
			constant.NewArray(vtableArrayType, methodConstants...),
		)
		vtable := g.module.NewGlobalDef(fmt.Sprintf("%s_vtable", className), vtableInit)
		g.vtables[className] = vtable

		fmt.Printf("Constructed vtable for %s with %d method(s) and parent pointer %v\n", className, n, parentVtable)
	}
	return nil
}



// Helper: returns a slice of class names sorted so that parent's come before children.
func sortClassesByInheritanceFromTable(table map[string]*ClassInfo) []string {
	// Compute depths.
	depths := make(map[string]int)
	var getDepth func(name string) int
	getDepth = func(name string) int {
		if d, ok := depths[name]; ok {
			return d
		}
		info, ok := table[name]
		if !ok || info.Parent == "" {
			depths[name] = 0
			return 0
		}
		d := 1 + getDepth(info.Parent)
		depths[name] = d
		return d
	}
	// Collect keys.
	names := make([]string, 0, len(table))
	for name := range table {
		names = append(names, name)
	}
	// Sort by depth (lower depth first).
	sort.Slice(names, func(i, j int) bool {
		return getDepth(names[i]) < getDepth(names[j])
	})
	return names
}
