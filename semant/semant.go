package semant

import (
	"cool-compiler/ast"
	"cool-compiler/lexer"
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

    // First pass: register all methods and attributes
    for _, feature := range class.Features {
        switch f := feature.(type) {
        case *ast.Method:
            if err := sa.symbolTable.AddMethod(className, f); err != nil {
                sa.errors = append(sa.errors, err.Error())
            }
        case *ast.Attribute:
            if err := sa.symbolTable.AddAttribute(className, f); err != nil {
                sa.errors = append(sa.errors, err.Error())
            }
        }
    }

    // Second pass: analyze method bodies and attribute initializers
    for _, feature := range class.Features {
        switch f := feature.(type) {
        case *ast.Method:
            // Create a new scope for the method
            sa.symbolTable.EnterScope(SymbolMethod, className)
            
            // Add parameters to scope
            for _, param := range f.Parameters {
                // Check for duplicate parameters
                if _, exists := sa.symbolTable.CurrentScope.Symbols[param.Name.Value]; exists {
                    sa.errors = append(sa.errors, fmt.Sprintf("line %d:%d: duplicate parameter name '%s' in method '%s'",
                        param.Name.Token.Line, param.Name.Token.Column, param.Name.Value, f.Name.Value))
                    continue
                }
                
                // Check for 'self' as parameter name
                if param.Name.Value == "self" {
                    sa.errors = append(sa.errors, fmt.Sprintf("line %d:%d: 'self' cannot be used as a parameter name",
                        param.Name.Token.Line, param.Name.Token.Column))
                    continue
                }
                
                // Add parameter to scope
                sa.symbolTable.CurrentScope.Symbols[param.Name.Value] = &Symbol{
                    Name:  param.Name.Value,
                    Kind:  SymbolLocal,
                    Type:  param.Type.Value,
                    Token: param.Name.Token,
                }
                fmt.Printf("DEBUG: Added parameter '%s' with type '%s' to method '%s'\n", 
                    param.Name.Value, param.Type.Value, f.Name.Value)
            }
            
            // Analyze method body
            sa.analyzeExpression(f.Body, className)
            
            // Ensure the body's type conforms to the return type
            bodyType := sa.symbolTable.GetExpressionType(f.Body, className)
            returnType := f.ReturnType.Value
            
            if !sa.symbolTable.IsConformingType(bodyType, returnType, className) {
                sa.errors = append(sa.errors, fmt.Sprintf("line %d:%d: method body type '%s' does not conform to declared return type '%s'",
                    f.Token.Line, f.Token.Column, bodyType, returnType))
            }
            
            // Exit method scope
            sa.symbolTable.ExitScope()
            
        case *ast.Attribute:
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
	case *ast.Assignment:
		// Analyze the right-hand side expression
		sa.analyzeExpression(e.Expression, className)
		
		// Check if variable being assigned to exists
		if e.Name.Value != "self" { // Cannot assign to self
			if _, exists := sa.symbolTable.LookupSymbol(e.Name.Value); !exists {
				// Also check if it's a class attribute
				if _, exists := sa.symbolTable.LookupAttribute(className, e.Name.Value); !exists {
					// Truly undefined
					sa.errors = append(sa.errors, 
						fmt.Sprintf("line %d:%d: assignment to undefined variable '%s'", 
						e.Name.Token.Line, e.Name.Token.Column, e.Name.Value))
				}
			}
		} else {
			// Cannot assign to self
			sa.errors = append(sa.errors, 
				fmt.Sprintf("line %d:%d: cannot assign to 'self'", 
				e.Name.Token.Line, e.Name.Token.Column))
		}
		
		// Type check the assignment
		exprType := sa.symbolTable.GetExpressionType(e.Expression, className)
		targetType := sa.symbolTable.GetExpressionType(e.Name, className)
		
		if !sa.symbolTable.IsConformingType(exprType, targetType, className) {
			sa.errors = append(sa.errors, 
				fmt.Sprintf("line %d:%d: type %s of assigned expression does not conform to declared type %s of identifier %s", 
				e.Token.Line, e.Token.Column, exprType, targetType, e.Name.Value))
		}
	case *ast.ObjectIdentifier:
		// Skip "self" keyword
		if e.Value == "self" {
			return
		}
		
		// Check if the identifier is defined in any scope
		if _, exists := sa.symbolTable.LookupSymbol(e.Value); !exists {
			// Also check if it's a class attribute
			if _, exists := sa.symbolTable.LookupAttribute(className, e.Value); !exists {
				// Identifier is truly undefined
				sa.errors = append(sa.errors, 
					fmt.Sprintf("line %d:%d: undefined variable '%s'", 
					e.Token.Line, e.Token.Column, e.Value))
			}
		}
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
			// Validate binding name is not 'self'
			if binding.Name.Value == "self" {
				sa.errors = append(sa.errors, fmt.Sprintf("line %d:%d: 'self' cannot be bound in a let expression",
					binding.Name.Token.Line, binding.Name.Token.Column))
				continue
			}
	
			// Validate declared type
			if !sa.symbolTable.isValidType(binding.Type.Value) {
				sa.errors = append(sa.errors, fmt.Sprintf("line %d:%d: undefined type '%s' in let binding",
					binding.Type.Token.Line, binding.Type.Token.Column, binding.Type.Value))
				continue
			}
	
			// Analyze initializer if present
			if binding.Init != nil {
				// Check for undefined variables in the initializer
				sa.analyzeExpression(binding.Init, className)
				
				initType := sa.symbolTable.GetExpressionType(binding.Init, className)
				declaredType := binding.Type.Value
				
				if !sa.symbolTable.IsConformingType(initType, declaredType, className) {
					sa.errors = append(sa.errors, fmt.Sprintf("line %d:%d: let initializer type '%s' does not conform to declared type '%s'",
						binding.Type.Token.Line, binding.Type.Token.Column, initType, declaredType))
				}
			}
	
			// Add binding to scope - even if there are errors, to prevent cascading errors
			sa.symbolTable.CurrentScope.Symbols[binding.Name.Value] = &Symbol{
				Name:  binding.Name.Value,
				Kind:  SymbolLocal,
				Type:  binding.Type.Value,
				Token: binding.Name.Token,
			}
			
			fmt.Printf("DEBUG: Added let binding '%s' with type '%s' to scope\n", 
				binding.Name.Value, binding.Type.Value)
		}
	
		// Analyze the body of the let expression
		sa.analyzeExpression(e.Body, className)
	// This is the fixed method call case for the analyzeExpression function in semant.go
case *ast.MethodCall:
    // First, analyze all arguments regardless of whether the method exists
    for _, arg := range e.Arguments {
        sa.analyzeExpression(arg, className)
    }
    
    // Track whether we found the method to avoid duplicate errors
    methodFound := false
    
    if e.Object != nil {
        // This is a dispatched call (obj.method())
        sa.analyzeExpression(e.Object, className)
        
        // Check for null dispatch
        if _, isVoid := e.Object.(*ast.IsVoidExpression); isVoid {
            sa.errors = append(sa.errors, fmt.Sprintf("line %d:%d: dispatch on void",
                e.Method.Token.Line, e.Method.Token.Column))
            return
        }
        
        // Get the object type for the dispatch
        objectType := sa.symbolTable.GetExpressionType(e.Object, className)
        fmt.Printf("DEBUG: Method call on object of type: %s\n", objectType)
        
        // For static dispatch (@Type)
        if e.Type != nil {
            // Check if the type exists
            if !sa.symbolTable.isValidType(e.Type.Value) {
                sa.errors = append(sa.errors, fmt.Sprintf("line %d:%d: undefined type '%s' in static dispatch",
                    e.Type.Token.Line, e.Type.Token.Column, e.Type.Value))
            } else {
                // Verify object conforms to static type
                if !sa.symbolTable.IsConformingType(objectType, e.Type.Value, className) {
                    sa.errors = append(sa.errors, fmt.Sprintf("line %d:%d: expression type '%s' does not conform to declared static dispatch type '%s'",
                        e.Type.Token.Line, e.Type.Token.Column, objectType, e.Type.Value))
                }
                
                // Use the static type for method lookup
                method, exists := sa.symbolTable.LookupMethod(e.Type.Value, e.Method.Value)
                if exists {
                    methodFound = true
                    sa.validateMethodArguments(method, e.Arguments, className, e.Method.Token)
                }
            }
        } else {
            // Regular dynamic dispatch
            method, exists := sa.symbolTable.LookupMethod(objectType, e.Method.Value)
            if exists {
                methodFound = true
                sa.validateMethodArguments(method, e.Arguments, className, e.Method.Token)
            }
        }
        
        // Report undefined method
        if !methodFound {
            dispatchType := objectType
            if e.Type != nil {
                dispatchType = e.Type.Value
            }
            sa.errors = append(sa.errors,
                fmt.Sprintf("line %d:%d: undefined method '%s' called on object of type '%s'",
                    e.Method.Token.Line, e.Method.Token.Column, e.Method.Value, dispatchType))
        }
    } else {
        // This is a direct method call (implicit self)
        method, exists := sa.symbolTable.LookupMethod(className, e.Method.Value)
        if exists {
            methodFound = true
            sa.validateMethodArguments(method, e.Arguments, className, e.Method.Token)
        } else {
            // Check if it's defined in any built-in class
            for _, builtInClass := range []string{"Object", "String", "Int", "Bool"} {
                if method, exists := sa.symbolTable.LookupMethod(builtInClass, e.Method.Value); exists {
                    // Methods from built-in classes should be directly callable in certain cases (like IO methods)
                    if builtInClass == "IO" && sa.symbolTable.IsConformingType(className, "IO", className) {
                        methodFound = true
                        sa.validateMethodArguments(method, e.Arguments, className, e.Method.Token)
                        break
                    }
                }
            }
            
            // Check the inheritance chain one last time for clarity
            current := className
            for current != "" && !methodFound {
                if classSymbol, exists := sa.symbolTable.Classes[current]; exists {
                    for featureName, feature := range classSymbol.Features {
                        if featureName == e.Method.Value && feature.Kind == SymbolMethod {
                            methodFound = true
                            break
                        }
                    }
                    current = classSymbol.Parent
                } else {
                    break
                }
            }
        }
        
        // Report undefined method
        if !methodFound {
            sa.errors = append(sa.errors,
                fmt.Sprintf("line %d:%d: undefined method '%s' called in class '%s'",
                    e.Method.Token.Line, e.Method.Token.Column, e.Method.Value, className))
        }
    }
	// Add this case to the switch statement in analyzeExpression
case *ast.FunctionCall:
    fmt.Printf("DEBUG: Analyzing function call to method: %s\n", e.Function.Value)
    
    // Analyze all arguments
    for _, arg := range e.Arguments {
        sa.analyzeExpression(arg, className)
    }
    
    // Check if the method exists in the current class or any ancestor
    methodFound := false
    
    // First check in the current class
    if method, exists := sa.symbolTable.LookupMethod(className, e.Function.Value); exists {
        methodFound = true
        // Validate argument types
        sa.validateMethodArguments(method, e.Arguments, className, e.Function.Token)
    }
    
    // If not found, and it might be an IO method, check if the class inherits from IO
    if !methodFound && (e.Function.Value == "out_string" || e.Function.Value == "out_int" || 
                        e.Function.Value == "in_string" || e.Function.Value == "in_int") {
        if sa.symbolTable.IsConformingType(className, "IO", className) {
            if method, exists := sa.symbolTable.LookupMethod("IO", e.Function.Value); exists {
                methodFound = true
                sa.validateMethodArguments(method, e.Arguments, className, e.Function.Token)
            }
        }
    }
    
    // If still not found, report the error
    if !methodFound {
        sa.errors = append(sa.errors, 
            fmt.Sprintf("line %d:%d: undefined method '%s' called in class '%s'",
                e.Function.Token.Line, e.Function.Token.Column, e.Function.Value, className))
    }
	}
	
}
func (sa *SemanticAnalyzer) validateMethodArguments(method *Symbol, args []ast.Expression, 
    className string, token lexer.Token) {
    
    // Check arity
    if len(args) != len(method.Parameters) {
        sa.errors = append(sa.errors, fmt.Sprintf("line %d:%d: wrong number of arguments for method '%s': expected %d, got %d",
            token.Line, token.Column, method.Name, len(method.Parameters), len(args)))
        return
    }
    
    // Check argument types
    for i, arg := range args {
        argType := sa.symbolTable.GetExpressionType(arg, className)
        paramType := method.Parameters[i].Type.Value
        
        if !sa.symbolTable.IsConformingType(argType, paramType, className) {
            sa.errors = append(sa.errors, fmt.Sprintf("line %d:%d: argument #%d type '%s' does not conform to parameter type '%s'",
                token.Line, token.Column, i+1, argType, paramType))
        }
    }
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

