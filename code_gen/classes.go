// classes.go
package code_gen

import (
	"cool-compiler/ast"
	"fmt"
	"sort"
)

type ClassInfo struct {
	Name       string
	Parent     string
	Methods    map[string]*ast.Method
	Attributes map[string]*ast.Attribute
}

type ClassTable struct {
	Classes map[string]*ClassInfo
}

func BuildClassTable(prog *ast.Program) *ClassTable {
	table := &ClassTable{Classes: make(map[string]*ClassInfo)}

	// Add basic classes first
	objectInfo := &ClassInfo{
		Name:       "Object",
		Parent:     "",
		Methods:    make(map[string]*ast.Method),
		Attributes: make(map[string]*ast.Attribute),
	}

	ioInfo := &ClassInfo{
		Name:       "IO",
		Parent:     "Object",
		Methods:    make(map[string]*ast.Method),
		Attributes: make(map[string]*ast.Attribute),
	}

	stringInfo := &ClassInfo{
		Name:       "String",
		Parent:     "Object",
		Methods:    make(map[string]*ast.Method),
		Attributes: make(map[string]*ast.Attribute),
	}

	intInfo := &ClassInfo{
		Name:       "Int",
		Parent:     "Object",
		Methods:    make(map[string]*ast.Method),
		Attributes: make(map[string]*ast.Attribute),
	}

	boolInfo := &ClassInfo{
		Name:       "Bool",
		Parent:     "Object",
		Methods:    make(map[string]*ast.Method),
		Attributes: make(map[string]*ast.Attribute),
	}

	// Add basic classes to table
	table.Classes["Object"] = objectInfo
	table.Classes["IO"] = ioInfo
	table.Classes["String"] = stringInfo
	table.Classes["Int"] = intInfo
	table.Classes["Bool"] = boolInfo

	// Add user-defined classes
	for _, cls := range prog.Classes {
		ci := &ClassInfo{
			Name:       cls.Name.Value,
			Parent:     "Object",
			Methods:    make(map[string]*ast.Method),
			Attributes: make(map[string]*ast.Attribute),
		}
		if cls.Parent != nil {
			ci.Parent = cls.Parent.Value
		}
		table.Classes[ci.Name] = ci
	}

	// Add features for user-defined classes
	for _, cls := range prog.Classes {
		cInfo := table.Classes[cls.Name.Value]
		for _, feat := range cls.Features {
			switch f := feat.(type) {
			case *ast.Method:
				cInfo.Methods[f.Name.Value] = f
			case *ast.Attribute:
				cInfo.Attributes[f.Name.Value] = f
			}
		}
	}

	return table
}

// Vtable
type VtableEntry struct {
	MethodName        string
	ImplementingClass string
	Method            *ast.Method
}

type VtableLayout struct {
	ClassName string
	Entries   []VtableEntry
}

type VtableTable struct {
	Layouts map[string]*VtableLayout
}

func BuildVtableLayouts(ct *ClassTable) (*VtableTable, error) {
	vt := &VtableTable{Layouts: make(map[string]*VtableLayout)}

	sortedClasses, err := topologicalSort(ct)
	if err != nil {
		return nil, err
	}

	for _, cName := range sortedClasses {
		cInfo := ct.Classes[cName]
		if cInfo.Parent == "" {
			cInfo.Parent = "Object"
		}

		var parentEntries []VtableEntry
		if cInfo.Parent != "Object" {
			parentLayout, ok := vt.Layouts[cInfo.Parent]
			if !ok {
				return nil, fmt.Errorf("no vtable layout for parent %s", cInfo.Parent)
			}
			// Copy parent's entries to preserve ordering.
			parentEntries = append([]VtableEntry(nil), parentLayout.Entries...)
		}

		// Gather child methods to consider for override or new additions.
		childMethods := make(map[string]*ast.Method)
		for mName, m := range cInfo.Methods {
			childMethods[mName] = m
		}

		// Override parent's methods if the child redefines them.
		for i, entry := range parentEntries {
			if over, found := childMethods[entry.MethodName]; found {
				parentEntries[i] = VtableEntry{
					MethodName:        entry.MethodName,
					ImplementingClass: cName,
					Method:            over,
				}
				delete(childMethods, entry.MethodName)
			}
		}

		// For new methods (i.e. those not overriding a parent method), sort their names to get deterministic order.
		var newMethodNames []string
		for mName := range childMethods {
			newMethodNames = append(newMethodNames, mName)
		}
		sort.Strings(newMethodNames)
		for _, mName := range newMethodNames {
			parentEntries = append(parentEntries, VtableEntry{
				MethodName:        mName,
				ImplementingClass: cName,
				Method:            childMethods[mName],
			})
		}

		vt.Layouts[cName] = &VtableLayout{
			ClassName: cName,
			Entries:   parentEntries,
		}
	}

	return vt, nil
}

func topologicalSort(ct *ClassTable) ([]string, error) {
	visited := make(map[string]bool)
	temp := make(map[string]bool)
	var result []string

	var visit func(string) error
	visit = func(cName string) error {
		if temp[cName] {
			return fmt.Errorf("cycle detected at class %q", cName)
		}
		if visited[cName] {
			return nil
		}
		temp[cName] = true
		ci := ct.Classes[cName]
		if ci == nil {

			return fmt.Errorf("unknown class %s", cName)
		}
		if ci.Parent != "" && ci.Parent != "Object" {
			if err := visit(ci.Parent); err != nil {
				return err
			}
		}
		visited[cName] = true
		temp[cName] = false
		result = append(result, cName)
		return nil
	}

	for cName := range ct.Classes {
		if err := visit(cName); err != nil {
			return nil, err
		}
	}
	return result, nil
}
