package semant

import (
	"cool-compiler/ast"
	"fmt"
)

type SemanticAnalyzer struct {
	symbolTable *SymbolTable
	errors      []string
}

func NewSemanticAnalyzer() *SemanticAnalyzer {
	return &SemanticAnalyzer{
		symbolTable: NewSymbolTable(),
		errors:      []string{},
	}
}

func (sa *SemanticAnalyzer) Errors() []string {
	return sa.errors
}
func (sa *SemanticAnalyzer) topologicalSort(classes []*ast.Class) []*ast.Class {
    visited := make(map[string]bool)
    order := []*ast.Class{}

    var visit func(cls *ast.Class)
    visit = func(cls *ast.Class) {
        if visited[cls.Name.Value] {
            return
        }
        visited[cls.Name.Value] = true

        // Process parent first if it's a user-defined class
        if cls.Parent != nil {
            parentName := cls.Parent.Value
            for _, c := range classes {
                if c.Name.Value == parentName {
                    visit(c)
                    break
                }
            }
        }

        order = append(order, cls)
    }

    // Iterate over classes in original order
    for _, class := range classes {
        visit(class)
    }

    return order
}
func (sa *SemanticAnalyzer) Analyze(program *ast.Program) {
	fmt.Println("\n=== Building Class Hierarchy ===")

	for _, class := range program.Classes {
		className := class.Name.Value
		if _, isBasic := sa.symbolTable.BasicClasses[className]; isBasic {
			sa.errors = append(sa.errors,
				fmt.Sprintf("Class %s cannot be redefined", className))
			continue
		}
		if _, exists := sa.symbolTable.Classes[className]; exists {
			sa.errors = append(sa.errors,
				fmt.Sprintf("Class %s is redefined", className))
			continue
		}
		fmt.Printf("Pre-registering class: %s\n", class.Name.Value)
		sa.symbolTable.Classes[class.Name.Value] = &Symbol{
			Name:     class.Name.Value,
			Kind:     SymbolClass,
			Features: make(map[string]*Symbol),
		}
	}
	for _, class := range program.Classes {
		fmt.Printf("Setting up inheritance for class: %s\n", class.Name.Value)
		if err := sa.setupInheritance(class); err != nil {
			sa.errors = append(sa.errors, err.Error())
		}
	}

	if errs := sa.symbolTable.Inheritance.ValidateInheritance(); len(errs) > 0 {
		sa.errors = append(sa.errors, errs...)
		return
	}

	sortedClasses := sa.topologicalSort(program.Classes)

	for _, class := range sortedClasses {
		fmt.Printf("\nAnalyzing features of class: %s\n", class.Name.Value)
		sa.analyzeClass(class)
	}

	sa.validateMainClass(program)

}

func (sa *SemanticAnalyzer) setupInheritance(class *ast.Class) error {
	className := class.Name.Value

	if class.Parent != nil {
		parentName := class.Parent.Value
		// Check if parent class exists (either as a basic class or user-defined class)
		if _, exists := sa.symbolTable.Classes[parentName]; !exists {
			if _, exists := sa.symbolTable.BasicClasses[parentName]; !exists {
				return fmt.Errorf("class %s inherits from undefined class %s", className, parentName)
			}
		}

		sa.symbolTable.Classes[className].Parent = parentName
		if err := sa.symbolTable.Inheritance.AddInheritanceEdge(className, parentName); err != nil {
			return err
		}
	} else {
		// Default parent is Object
		sa.symbolTable.Classes[className].Parent = "Object"
		sa.symbolTable.Inheritance.AddInheritanceEdge(className, "Object")
	}

	return nil
}

func (sa *SemanticAnalyzer) analyzeClass(class *ast.Class) {
	className := class.Name.Value

	// Analyze each feature
	for _, feature := range class.Features {
		switch f := feature.(type) {
		case *ast.Method:
			if err := sa.symbolTable.AddMethod(className, f); err != nil {
				sa.errors = append(sa.errors, err.Error())
			}
			// Analyze method body
			sa.analyzeExpression(f.Body, className)
		case *ast.Attribute:
			if err := sa.symbolTable.AddAttribute(className, f); err != nil {
				sa.errors = append(sa.errors, err.Error())
			}
			// Analyze attribute initialization if present
			if f.Init != nil {
				sa.analyzeExpression(f.Init, className)
			}
		}
	}
}

func (sa *SemanticAnalyzer) analyzeExpression(expr ast.Expression, className string) {
	if expr == nil {
		return
	}

	switch e := expr.(type) {
	case *ast.BinaryExpression:
		sa.analyzeExpression(e.Left, className)
		sa.analyzeExpression(e.Right, className)
		leftType := sa.symbolTable.GetExpressionType(e.Left, className)
		rightType := sa.symbolTable.GetExpressionType(e.Right, className)

		// Validate types based on operator
		switch e.Operator {
		case "+", "-", "*", "/":
			if leftType != "Int" || rightType != "Int" {
				sa.errors = append(sa.errors, fmt.Sprintf("line %d: arithmetic operator %s requires Int operands",
					e.Token.Line, e.Operator))
			}
		case "<", "<=":
			if leftType != "Int" || rightType != "Int" {
				sa.errors = append(sa.errors, fmt.Sprintf("line %d: comparison operator %s requires Int operands",
					e.Token.Line, e.Operator))
			}
		case "=":
			// Special case for basic types
			basicTypes := map[string]bool{"Int": true, "String": true, "Bool": true}
			if basicTypes[leftType] || basicTypes[rightType] {
				if leftType != rightType {
					sa.errors = append(sa.errors, fmt.Sprintf("line %d: cannot compare %s and %s with =", e.Token.Line, leftType, rightType))
				}
			}
		}
	case *ast.IfExpression:
		sa.analyzeExpression(e.Condition, className)
		sa.analyzeExpression(e.Consequence, className)
		sa.analyzeExpression(e.Alternative, className)

		condType := sa.symbolTable.GetExpressionType(e.Condition, className)
		if condType != "Bool" {
			sa.errors = append(sa.errors, fmt.Sprintf("line %d: if condition must be Bool, got %s",
				e.Token.Line, condType))
		}
	case *ast.BlockExpression:
		for _, expr := range e.Expressions {
			sa.analyzeExpression(expr, className)
		}
	case *ast.CaseExpression:
		sa.analyzeExpression(e.Expression, className)

		// Track seen types to detect duplicates
		seenTypes := make(map[string]bool)
		hasObjectBranch := false

		for _, branch := range e.Cases {
			if branch.Type.Value == "Object" {
				hasObjectBranch = true
			}
			if seenTypes[branch.Type.Value] {
				sa.errors = append(sa.errors, fmt.Sprintf("line %d: duplicate branch type %s in case expression",
					branch.Token.Line, branch.Type.Value))
				continue
			}
			seenTypes[branch.Type.Value] = true

			// Validate branch type
			if !sa.symbolTable.isValidType(branch.Type.Value) {
				sa.errors = append(sa.errors, fmt.Sprintf("line %d: undefined type %s in case branch",
					branch.Token.Line, branch.Type.Value))
			}

			// Analyze branch expression
			sa.analyzeExpression(branch.Expression, className)
		}

		// Check for Object branch
		if !hasObjectBranch {
			sa.errors = append(sa.errors, fmt.Sprintf("line %d: case expression should have Object branch as default",
				e.Token.Line))
		}
	case *ast.WhileExpression:
		sa.analyzeExpression(e.Condition, className)
		sa.analyzeExpression(e.Body, className)
		condType := sa.symbolTable.GetExpressionType(e.Condition, className)
		if condType != "Bool" {
			sa.errors = append(sa.errors, fmt.Sprintf("line %d: while condition must be Bool, got %s", e.Token.Line, condType))
		}

	case *ast.NotExpression:
		sa.analyzeExpression(e.Expression, className)
		exprType := sa.symbolTable.GetExpressionType(e.Expression, className)
		if exprType != "Bool" {
			sa.errors = append(sa.errors, fmt.Sprintf("line %d: not requires Bool operand", e.Token.Line))
		}
	case *ast.LetExpression:
		// Create new scope for let bindings
		sa.symbolTable.EnterScope(SymbolLocal, className)
		defer sa.symbolTable.ExitScope()
	
		// Process all bindings, including the first one
		allBindings := make([]*ast.Binding, 0)
		if e.Name != nil {
			firstBinding := &ast.Binding{
				Name: e.Name,
				Type: e.Type,
				Init: e.Init,
			}
			allBindings = append(allBindings, firstBinding)
		}
		allBindings = append(allBindings, e.Bindings...)
	
		for _, binding := range allBindings {
			// Validate declared type
			if !sa.symbolTable.isValidType(binding.Type.Value) {
				sa.errors = append(sa.errors, fmt.Sprintf("line %d: undefined type %s in let binding",
					binding.Name.Token.Line, binding.Type.Value))
				continue
			}
	
			// Analyze initializer if present
			if binding.Init != nil {
				sa.analyzeExpression(binding.Init, className)
				initType := sa.symbolTable.GetExpressionType(binding.Init, className)
				if !sa.symbolTable.IsConformingType(initType, binding.Type.Value, className) {
					sa.errors = append(sa.errors, fmt.Sprintf("line %d: let initializer type %s does not conform to declared type %s",
						binding.Name.Token.Line, initType, binding.Type.Value))
				}
			}
	
			// Add binding to scope
			sa.symbolTable.CurrentScope.Symbols[binding.Name.Value] = &Symbol{
				Name: binding.Name.Value,
				Kind: SymbolLocal,
				Type: binding.Type.Value,
			}
		}
	
		sa.analyzeExpression(e.Body, className)
	case *ast.MethodCall:
		// dispatching on an IsVoid expression
		if _, isVoid := e.Object.(*ast.IsVoidExpression); isVoid {
			sa.errors = append(sa.errors, "dispatch on void")
			return
		}

		// Check if method exists in current class or parent classes
		if e.Object == nil {
			// This is a direct method call 
			if _, exists := sa.symbolTable.LookupMethod(className, e.Method.Value); !exists {
				sa.errors = append(sa.errors,
					fmt.Sprintf("line %d:%d: undefined method '%s' called in class %s",
						e.Method.Token.Line, e.Method.Token.Column, e.Method.Value, className))
			}
		} else {
			// This is an object method call 
			objectType := sa.symbolTable.GetExpressionType(e.Object, className)
			if _, exists := sa.symbolTable.LookupMethod(objectType, e.Method.Value); !exists {
				sa.errors = append(sa.errors,
					fmt.Sprintf("line %d:%d: undefined method '%s' called on object of type %s",
						e.Method.Token.Line, e.Method.Token.Column, e.Method.Value, objectType))
			}
		}
		// Analyze method arguments
		for _, arg := range e.Arguments {
			sa.analyzeExpression(arg, className)
		}
	}
	// Add other expression types as needed
}

func (sa *SemanticAnalyzer) validateMainClass(program *ast.Program) {
	fmt.Println("\n=== Validating Main Class ===")

	// Look for Main class
	var mainClass *ast.Class
	for _, class := range program.Classes {
		if class.Name.Value == "Main" {
			mainClass = class
			break
		}
	}

	if mainClass == nil {
		sa.errors = append(sa.errors, "Program does not contain a 'Main' class")
		return
	}

	// Check main method is defined in Main (not inherited)
	var mainMethod *ast.Method
	for _, feature := range mainClass.Features {
		if method, ok := feature.(*ast.Method); ok && method.Name.Value == "main" {
			mainMethod = method
			break
		}
	}

	if mainMethod == nil {
		sa.errors = append(sa.errors, "Class 'Main' must define a 'main' method")
		return
	}

	// Validate no parameters
	if len(mainMethod.Parameters) != 0 {
		sa.errors = append(sa.errors, "Method 'main' must have 0 parameters")
	}

}

