package code_gen

// import (
//     "cool-compiler/ast"
//     "fmt"
// )

// // ----------------------------------
// //  Data structures
// // ----------------------------------

// type ClassInfo struct {
//     Name       string
//     Parent     string
//     Methods    map[string]*ast.Method
//     Attributes map[string]*ast.Attribute
// }

// type ClassTable struct {
//     Classes map[string]*ClassInfo
// }

// type VtableEntry struct {
//     MethodName string
//     Method     *ast.Method
// }

// type VtableLayout struct {
//     ClassName string
//     Entries   []VtableEntry
// }

// type VtableTable struct {
//     Layouts map[string]*VtableLayout
// }

// // ----------------------------------
// //  BuildClassTable
// // ----------------------------------

// // BuildClassTable traverses the AST and gathers each class's parent, methods, and attributes.
// func BuildClassTable(prog *ast.Program) *ClassTable {
//     table := &ClassTable{Classes: make(map[string]*ClassInfo)}

//     // Pass #1: Collect each class, defaulting parent to "Object" if missing.
//     for _, cls := range prog.Classes {
//         classInfo := &ClassInfo{
//             Name:       cls.Name.Value,
//             Parent:     "Object", // default if not explicitly stated
//             Methods:    make(map[string]*ast.Method),
//             Attributes: make(map[string]*ast.Attribute),
//         }
//         if cls.Parent != nil {
//             classInfo.Parent = cls.Parent.Value
//         }
//         // Register in the table
//         table.Classes[classInfo.Name] = classInfo
//     }

//     // Pass #2: Collect features (methods, attributes)
//     for _, cls := range prog.Classes {
//         cInfo := table.Classes[cls.Name.Value] // retrieve the info we just created
//         for _, feature := range cls.Features {
//             switch f := feature.(type) {
//             case *ast.Method:
//                 cInfo.Methods[f.Name.Value] = f
//             case *ast.Attribute:
//                 cInfo.Attributes[f.Name.Value] = f
//             default:
//                 // If there are other Feature types not covered, handle them or ignore
//             }
//         }
//     }
//     return table
// }

// // ----------------------------------
// //  BuildVtableLayouts
// // ----------------------------------

// // BuildVtableLayouts uses the ClassTable to produce final method dispatch orders for each class.
// func BuildVtableLayouts(ct *ClassTable) (*VtableTable, error) {
//     vt := &VtableTable{Layouts: make(map[string]*VtableLayout)}

//     // Sort classes so that parents come first
//     classList, err := TopologicalSort(ct)
//     if err != nil {
//         return nil, fmt.Errorf("failed to sort classes topologically: %w", err)
//     }

//     // For each class in parent-first order, build the vtable
//     for _, cName := range classList {
//         cInfo, ok := ct.Classes[cName]
//         if !ok {
//             return nil, fmt.Errorf("class %q not found in ClassTable", cName)
//         }

//         // Start with a copy of the parent's vtable entries, if the parent isn't Object
//         var parentEntries []VtableEntry
//         parentName := cInfo.Parent
//         if parentName != "" && parentName != "Object" {
//             pLayout, found := vt.Layouts[parentName]
//             if !found {
//                 return nil, fmt.Errorf("parent class %q layout not found for class %q", parentName, cName)
//             }
//             // Copy parent's method entries
//             parentEntries = append(parentEntries, pLayout.Entries...)
//         }

//         // We need to handle any child method overrides or additions
//         // Copy child methods so we can remove them as we handle them
//         childMethods := make(map[string]*ast.Method)
//         for mname, method := range cInfo.Methods {
//             childMethods[mname] = method
//         }

//         // Override: if a method is in parent's vtable, replace it
//         for i, entry := range parentEntries {
//             if overridingMethod, exists := childMethods[entry.MethodName]; exists {
//                 // Child overrides
//                 parentEntries[i] = VtableEntry{
//                     MethodName: entry.MethodName,
//                     Method:     overridingMethod,
//                 }
//                 delete(childMethods, entry.MethodName)
//             }
//         }

//         // Add any new methods that weren't in the parent
//         for mname, methodNode := range childMethods {
//             parentEntries = append(parentEntries, VtableEntry{
//                 MethodName: mname,
//                 Method:     methodNode,
//             })
//         }

//         // Store final layout
//         vt.Layouts[cName] = &VtableLayout{
//             ClassName: cName,
//             Entries:   parentEntries,
//         }
//     }

//     return vt, nil
// }

// // ----------------------------------
// //  TopologicalSort (parent -> child)
// // ----------------------------------

// // TopologicalSort returns a list of class names in parent-first order.
// // If there's a cycle or an undefined parent, we return an error.
// func TopologicalSort(ct *ClassTable) ([]string, error) {
//     // We'll do a DFS-based sort:
//     visited := make(map[string]bool)
//     tempMark := make(map[string]bool)
//     result := []string{}

//     var visit func(clsName string) error
//     visit = func(clsName string) error {
//         // If we temporarily marked it, there's a cycle
//         if tempMark[clsName] {
//             return fmt.Errorf("cycle detected with class %q", clsName)
//         }
//         // If already visited, skip
//         if visited[clsName] {
//             return nil
//         }

//         // Mark temporarily
//         tempMark[clsName] = true

//         cInfo, ok := ct.Classes[clsName]
//         if !ok {
//             // Class doesn't exist in table => error
//             return fmt.Errorf("class %q not found", clsName)
//         }

//         // Visit parent if not "Object"
//         pName := cInfo.Parent
//         if pName != "" && pName != "Object" {
//             if _, parentExists := ct.Classes[pName]; !parentExists {
//                 return fmt.Errorf("class %q has undefined parent %q", clsName, pName)
//             }
//             // Recur on parent
//             if err := visit(pName); err != nil {
//                 return err
//             }
//         }

//         // Mark permanently
//         tempMark[clsName] = false
//         visited[clsName] = true

//         // Add to result after the parent
//         result = append(result, clsName)
//         return nil
//     }

//     // Try to visit each class
//     for cName := range ct.Classes {
//         if err := visit(cName); err != nil {
//             return nil, err
//         }
//     }

//     // `result` now has classes in parent->child order
//     return result, nil
// }

import (
	"fmt"

	"cool-compiler/ast"

	"github.com/llir/llvm/ir"
)

// GenerateLLVM is the main entry point to code generation.
// Returns the completed LLVM module or an error.
func GenerateLLVM(prog *ast.Program) (*ir.Module, error) {
	fmt.Println("=== Starting LLVM Generation ===")

	// 1) Build class table
	fmt.Println("Building class table...")
	ct := BuildClassTable(prog)
	fmt.Println("Class table built successfully")

	// 2) Build vtable layouts
	fmt.Println("Building vtable layouts...")
	vt, err := BuildVtableLayouts(ct)
	if err != nil {
		return nil, fmt.Errorf("failed building vtable layouts: %w", err)
	}
	fmt.Println("Vtable layouts built successfully")

	// 3) Create a new LLVM module
	fmt.Println("Creating new LLVM module...")
	mod := ir.NewModule()

	// 3.5) Initialize built-in functions (e.g. out_string and out_int)
	initBuiltins(mod)

	// 4) Define struct types
	fmt.Println("Defining class structs...")
	if err := DefineClassStructs(mod, ct); err != nil {
		return nil, fmt.Errorf("failed defining structs: %w", err)
	}
	fmt.Println("Class structs defined successfully")

	// 5) Declare method stubs
	fmt.Println("Declaring methods...")
	if err := DeclareAllMethods(mod, ct); err != nil {
		return nil, fmt.Errorf("failed declaring methods: %w", err)
	}
	fmt.Println("Methods declared successfully")

	// 6) Define global vtables
	fmt.Println("Defining vtables...")
	if err := DefineGlobalVtables(mod, vt, ct); err != nil {
		return nil, fmt.Errorf("failed defining vtables: %w", err)
	}
	fmt.Println("Vtables defined successfully")

	// 7) Define method bodies
	fmt.Println("Defining method bodies...")
	if err := DefineMethodBodies(mod, ct); err != nil {
		return nil, fmt.Errorf("failed defining method bodies: %w", err)
	}
	fmt.Println("Method bodies defined successfully")

	// 8) Create an entrypoint if desired
	fmt.Println("Creating entrypoint...")
	if err := DefineEntryPoint(mod, ct); err != nil {
		return nil, fmt.Errorf("failed to define entrypoint: %w", err)
	}
	fmt.Println("Entrypoint created successfully")

	fmt.Println("=== LLVM Generation completed successfully ===")
	return mod, nil
}
