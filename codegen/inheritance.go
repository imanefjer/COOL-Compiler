package codegen

import (
	"cool-compiler/ast"
	"fmt"

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

	// Create Object root class with type_name method
	g.classTable["Object"] = &ClassInfo{
		Name:       "Object",
		Parent:     "",
		Attributes: []AttributeInfo{},
		Methods:    make(map[string]MethodInfo),
	}

	// Add built-in methods to Object
	g.classTable["Object"].Methods["abort"] = MethodInfo{
		Name:  "Object_abort",
		Index: 0,
	}
	g.classTable["Object"].Methods["type_name"] = MethodInfo{
		Name:  "Object_type_name",
		Index: 1,
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
			for name, method := range parentInfo.Methods {
				if name == "copy" {
					// Rename the inherited copy method to current class's name
					method.Name = fmt.Sprintf("%s_copy", info.Name)
				}
				info.Methods[name] = method
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
				for _, attr := range info.Attributes {
					if attr.Name == f.Name.Value {
						return fmt.Errorf("attribute %s redefined in class %s", f.Name.Value, class.Name.Value)
					}
				}
				// Add local attribute without setting Offset here
				info.Attributes = append(info.Attributes, AttributeInfo{
					Name:   f.Name.Value,
					Type:   f.Type.Value,
					Offset: 0, // Corrected: Offset will be set in ComputeObjectLayouts
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

		// Ensure type_name is properly inherited if not overridden
		if _, hasTypeName := info.Methods["type_name"]; !hasTypeName {
			info.Methods["type_name"] = MethodInfo{
				Name:  "Object_type_name",
				Index: 1, // Same index as in Object
			}
		}
	}
	return nil
}

func sortClassesByInheritance(classes []*ast.Class, table ClassTable) []*ast.Class {
	// Replace recursive depth calculation with iterative
	depths := make(map[string]int)
	for _, class := range classes {
		current := class.Name.Value
		depth := 0
		for {
			info, exists := table[current]
			if !exists || info.Parent == "" {
				break
			}
			current = info.Parent
			depth++
		}
		depths[class.Name.Value] = depth
	}

	sorted := make([]*ast.Class, len(classes))
	copy(sorted, classes)
	sort.Slice(sorted, func(i, j int) bool {
		return depths[sorted[i].Name.Value] < depths[sorted[j].Name.Value]
	})
	return sorted
}

func (g *CodeGenerator) ComputeObjectLayouts() error {
	fmt.Println("\n=== Debug: Computing Object Layouts ===")
	sortedClasses := sortClassesByInheritanceFromTable(g.classTable)

	fmt.Printf("\nProcessing classes in order: %v\n", sortedClasses)

	for _, className := range sortedClasses {
        classInfo := g.classTable[className]

        if classInfo.Parent != "" {
            parentInfo := g.classTable[classInfo.Parent]

            // Correctly inherit parent's attributes and offsets
            classInfo.Attributes = make([]AttributeInfo, len(parentInfo.Attributes))
            copy(classInfo.Attributes, parentInfo.Attributes)

            nextOffset := 1 // Default if parent has no attributes
            if len(parentInfo.Attributes) > 0 {
                nextOffset = parentInfo.Attributes[len(parentInfo.Attributes)-1].Offset + 1
            }

            // Process local attributes
            for _, class := range g.program.Classes {
                if class.Name.Value == className {
                    for _, feature := range class.Features {
                        if attr, ok := feature.(*ast.Attribute); ok {
                            isInherited := false
                            // Check all inherited attributes (from all ancestors)
                            currentClass := className
                            for currentClass != "" {
                                ancestorInfo := g.classTable[currentClass]
                                for _, a := range ancestorInfo.Attributes {
                                    if a.Name == attr.Name.Value {
                                        isInherited = true
                                        break
                                    }
                                }
                                if isInherited {
                                    break
                                }
                                currentClass = ancestorInfo.Parent
                            }

                            if !isInherited {
                                classInfo.Attributes = append(classInfo.Attributes, AttributeInfo{
                                    Name:   attr.Name.Value,
                                    Type:   attr.Type.Value,
                                    Offset: nextOffset,
                                })
                                nextOffset++
                            }
                        }
                    }
                }
            }

            // Update object size
            classInfo.ObjectSize = nextOffset
        } else {
			fmt.Printf("Base class - assigning sequential offsets:\n")
			offset := 1 // vtable at 0
			newAttributes := make([]AttributeInfo, 0)

			// Find the AST class to get initialization values
			var classAST *ast.Class
			for _, class := range g.program.Classes {
				if class.Name.Value == className {
					classAST = class
					break
				}
			}

			for _, attr := range classInfo.Attributes {
				// Find initialization value if present
				var initValue string = "default"
				if classAST != nil {
					for _, feature := range classAST.Features {
						if attrFeature, ok := feature.(*ast.Attribute); ok {
							if attrFeature.Name.Value == attr.Name {
								if attrFeature.Init != nil {
									switch init := attrFeature.Init.(type) {
									case *ast.StringLiteral:
										initValue = fmt.Sprintf("\"%s\"", init.Value)
									case *ast.IntegerLiteral:
										initValue = fmt.Sprintf("%d", init.Value)
									case *ast.BooleanLiteral:
										initValue = fmt.Sprintf("%v", init.Value)
									default:
										initValue = fmt.Sprintf("complex expression: %T", init)
									}
								}
								break
							}
						}
					}
				}

				fmt.Printf("  - Setting %s (type: %s) at offset %d with initial value: %s\n",
					attr.Name, attr.Type, offset, initValue)
				attr.Offset = offset
				newAttributes = append(newAttributes, attr)
				offset++
			}
			classInfo.Attributes = newAttributes
		}

		// Update object size
		if len(classInfo.Attributes) > 0 {
			classInfo.ObjectSize = classInfo.Attributes[len(classInfo.Attributes)-1].Offset + 1
		} else {
			classInfo.ObjectSize = 1
		}

		fmt.Printf("\nFinal layout for %s:\n", className)
		fmt.Printf("Object size: %d\n", classInfo.ObjectSize)
		fmt.Printf("Attributes:\n")
		for _, attr := range classInfo.Attributes {
			// Find initialization value in AST
			initValue := "default"
			for _, class := range g.program.Classes {
				if class.Name.Value == className {
					for _, feature := range class.Features {
						if attrFeature, ok := feature.(*ast.Attribute); ok {
							if attrFeature.Name.Value == attr.Name {
								if attrFeature.Init != nil {
									switch init := attrFeature.Init.(type) {
									case *ast.StringLiteral:
										initValue = fmt.Sprintf("\"%s\"", init.Value)
									case *ast.IntegerLiteral:
										initValue = fmt.Sprintf("%d", init.Value)
									case *ast.BooleanLiteral:
										initValue = fmt.Sprintf("%v", init.Value)
									default:
										initValue = fmt.Sprintf("complex expression: %T", init)
									}
								}
								break
							}
						}
					}
				}
			}
			fmt.Printf("  - %s (type: %s) at offset %d with initial value: %s\n",
				attr.Name, attr.Type, attr.Offset, initValue)
		}
		fmt.Printf("----------------------------------------\n")
	}
	return nil
}

func (g *CodeGenerator) ConstructVTables() error {
	fmt.Println("\n=== Debug: Constructing VTables ===")
	sortedClasses := sortClassesByInheritanceFromTable(g.classTable)

	fmt.Println("\nSorted classes in inheritance order:", sortedClasses)

	for _, className := range sortedClasses {
		info := g.classTable[className]
		fmt.Printf("\nProcessing vtable for class: %s\n", className)
		fmt.Printf("Parent class: %s\n", info.Parent)
        classNameConst := g.getOrCreateStringConstant(info.Name)

		// Collect all methods including inherited ones in correct order
		methodMap := make(map[string]MethodInfo)

		// First, get all methods from the inheritance chain (bottom-up)
		currentClass := className
		fmt.Printf("\nTraversing inheritance chain for %s:\n", className)
		for currentClass != "" {
			if classInfo, exists := g.classTable[currentClass]; exists {
				fmt.Printf("- Checking methods in class: %s\n", currentClass)
				// Add methods from current class, potentially overriding parent methods
				for methodName, methodInfo := range classInfo.Methods {
					if _, exists := methodMap[methodName]; !exists {
						// Keep the original method implementation but update the name
						newMethodInfo := MethodInfo{
							Name:  fmt.Sprintf("%s_%s", className, methodName),
							Index: methodInfo.Index,
						}
						fmt.Printf("  * Adding method: %s (Index: %d)\n", newMethodInfo.Name, newMethodInfo.Index)
						methodMap[methodName] = newMethodInfo

						// Copy the implementation from the parent class if it doesn't exist
						if g.methods[newMethodInfo.Name] == nil && g.methods[methodInfo.Name] != nil {
							g.methods[newMethodInfo.Name] = g.methods[methodInfo.Name]
						}
					} else {
						fmt.Printf("  * Method already exists: %s (skipping)\n", methodName)
					}
				}
				currentClass = classInfo.Parent
			} else {
				fmt.Printf("WARNING: Class %s not found in class table\n", currentClass)
				break
			}
		}

		// Ensure type_name is available
		if _, hasTypeName := methodMap["type_name"]; !hasTypeName {
			fmt.Printf("Adding type_name method from Object class\n")
			methodMap["type_name"] = MethodInfo{
				Name:  "Object_type_name",
				Index: 1, // Standard index for type_name in Object class
			}
		}

		// Convert map to sorted slice to ensure consistent ordering
		var methods []MethodInfo
		fmt.Printf("\nFinal method list for %s:\n", className)
		for _, method := range methodMap {
			methods = append(methods, method)
			fmt.Printf("- Method: %s (Index: %d)\n", method.Name, method.Index)
		}
		sort.Slice(methods, func(i, j int) bool {
			return methods[i].Index < methods[j].Index
		})

		// Create vtable type with correct size
		n := uint64(len(methods))
		if n == 0 {
			n = 1 // Ensure at least one slot
		}

		// Get class name and parent name constants
		var parentVtable constant.Constant
		if info.Parent == "" {
			parentVtable = constant.NewNull(types.NewPointer(types.I8))
			fmt.Printf("Setting null parent vtable for root class: %s\n", info.Name)
		} else {
			parentGlobal := g.vtables[info.Parent]
			if parentGlobal == nil {
				return fmt.Errorf("parent vtable %s not found for class %s", info.Parent, info.Name)
			}
			parentVtable = constant.NewBitCast(parentGlobal, types.NewPointer(types.I8))
			fmt.Printf("Setting parent vtable pointer from %s to %s\n", info.Parent, info.Name)
		}

		// Create vtable type
		vtableArrayType := types.NewArray(n, types.NewPointer(types.I8))
		vtableType := types.NewStruct(
			types.NewPointer(types.I8), // class name
			types.NewPointer(types.I8), // parent vtable
			vtableArrayType,            // methods array
		)

		// Create method list
		methodList := make([]constant.Constant, 0, n)
		fmt.Printf("\nAdding methods to vtable for %s:\n", className)
		for _, method := range methods {
			fn := g.methods[method.Name]
			if fn == nil {
				fmt.Printf("ERROR: Method %s not found in methods map\n", method.Name)
				return fmt.Errorf("method %s not found in methods map", method.Name)
			}
			fmt.Printf("- Adding method to vtable: %s\n", method.Name)
			methodList = append(methodList, constant.NewBitCast(fn, types.NewPointer(types.I8)))
		}

		// Create vtable initializer
		vtableInit := constant.NewStruct(vtableType,
			constant.NewBitCast(classNameConst, types.NewPointer(types.I8)),
			constant.NewBitCast(parentVtable, types.NewPointer(types.I8)), // Fixed parent link
			constant.NewArray(vtableArrayType, methodList...),
		)

		// Create vtable global
		vtable := g.module.NewGlobalDef(fmt.Sprintf("%s_vtable", info.Name), vtableInit)
		g.vtables[info.Name] = vtable

		fmt.Printf("\nCompleted vtable construction for %s with %d method(s)\n",
			info.Name, n)
	}

	// Print final vtable summary
	fmt.Println("\n=== Final VTable Summary ===")
	for className, vtable := range g.vtables {
		fmt.Printf("Class %s vtable at: %s\n", className, vtable.Name())
	}

	return nil
}

// Helper: returns a slice of class names sorted so that parent's come before children.
func sortClassesByInheritanceFromTable(table map[string]*ClassInfo) []string {
	// Compute depths iteratively
	depths := make(map[string]int)
	for name := range table {
		current := name
		depth := 0
		visited := make(map[string]bool) // Track visited classes
		for {
			if visited[current] {
				// Handle cyclic dependency error
				panic(fmt.Sprintf("inheritance cycle detected at class %s", current))
			}
			visited[current] = true

			info, exists := table[current]
			if !exists || info.Parent == "" {
				break
			}
			current = info.Parent
			depth++
		}
		depths[name] = depth
	}

	// Collect keys and sort
	names := make([]string, 0, len(table))
	for name := range table {
		names = append(names, name)
	}
	sort.Slice(names, func(i, j int) bool {
		return depths[names[i]] < depths[names[j]]
	})
	return names
}
