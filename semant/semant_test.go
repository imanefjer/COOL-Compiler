package semant

import (
	"cool-compiler/ast"
	"cool-compiler/lexer"
	"fmt"
	"strings"
	"testing"
)

func TestSemanticAnalysis(t *testing.T) {
	dummyToken := lexer.Token{Type: lexer.OBJECTID, Literal: "dummy"}

	t.Run("Class Validation", func(t *testing.T) {
		t.Run("Basic Inheritance Error", func(t *testing.T) {
			classes := []*ast.Class{
				{
					Name:   &ast.ObjectIdentifier{Value: "Bad", Token: dummyToken},
					Parent: &ast.TypeIdentifier{Value: "Int", Token: dummyToken},
					Features: []ast.Feature{
						&ast.Method{
							Name:       &ast.ObjectIdentifier{Value: "main", Token: dummyToken},
							ReturnType: &ast.TypeIdentifier{Value: "Object", Token: dummyToken},
						},
					},
				},
			}
			sa := NewSemanticAnalyzer()
			sa.Analyze(&ast.Program{Classes: classes})
			assertErrorsContain(t, sa.Errors(), "cannot inherit from Int")
		})
	})

	t.Run("Method Validation", func(t *testing.T) {
		t.Run("Invalid Method Override", func(t *testing.T) {
			classes := []*ast.Class{
				{
					Name: &ast.ObjectIdentifier{Value: "Main", Token: dummyToken},
					Features: []ast.Feature{
						&ast.Method{
							Name:       &ast.ObjectIdentifier{Value: "main", Token: dummyToken},
							ReturnType: &ast.TypeIdentifier{Value: "Object", Token: dummyToken},
							Body:       &ast.ObjectIdentifier{Value: "self", Token: dummyToken},
						},
					},
				},
				{
					Name: &ast.ObjectIdentifier{Value: "Parent", Token: dummyToken},
					Features: []ast.Feature{
						&ast.Method{
							Name: &ast.ObjectIdentifier{Value: "test", Token: dummyToken},
							Parameters: []*ast.Formal{
								{Name: &ast.ObjectIdentifier{Value: "x", Token: dummyToken},
									Type: &ast.TypeIdentifier{Value: "Int", Token: dummyToken}},
							},
							ReturnType: &ast.TypeIdentifier{Value: "Int", Token: dummyToken},
							Body:       &ast.IntegerLiteral{Value: 0, Token: dummyToken},
						},
					},
				},
				{
					Name:   &ast.ObjectIdentifier{Value: "Child", Token: dummyToken},
					Parent: &ast.TypeIdentifier{Value: "Parent", Token: dummyToken},
					Features: []ast.Feature{
						&ast.Method{
							Name: &ast.ObjectIdentifier{Value: "test", Token: dummyToken},
							Parameters: []*ast.Formal{
								{Name: &ast.ObjectIdentifier{Value: "x", Token: dummyToken},
									Type: &ast.TypeIdentifier{Value: "String", Token: dummyToken}},
							},
							ReturnType: &ast.TypeIdentifier{Value: "Int", Token: dummyToken},
							Body:       &ast.IntegerLiteral{Value: 0, Token: dummyToken},
						},
					},
				},
			}
			sa := NewSemanticAnalyzer()
			sa.Analyze(&ast.Program{Classes: classes})
			assertErrorsContain(t, sa.Errors(), "parameter #1 type mismatch in overridden method 'test': expected 'Int', got 'String'")
		})
	})

	t.Run("Expression Validation", func(t *testing.T) {
		t.Run("Invalid Arithmetic Operation", func(t *testing.T) {
			expr := &ast.BinaryExpression{
				Token:    dummyToken,
				Left:     &ast.BooleanLiteral{Value: true, Token: dummyToken},
				Operator: "+",
				Right:    &ast.IntegerLiteral{Value: 5, Token: dummyToken},
			}
			sa := NewSemanticAnalyzer()
			sa.analyzeExpression(expr, "Main")
			assertErrorsContain(t, sa.Errors(), "Int operands")
		})

		t.Run("Valid Let Expression", func(t *testing.T) {
			letExpr := &ast.LetExpression{
				Token: dummyToken,
				Bindings: []*ast.Binding{
					{
						Name: &ast.ObjectIdentifier{Value: "x", Token: dummyToken},
						Type: &ast.TypeIdentifier{Value: "Int", Token: dummyToken},
						Init: &ast.IntegerLiteral{Value: 5, Token: dummyToken},
					},
				},
				Body: &ast.ObjectIdentifier{Value: "x", Token: dummyToken},
			}
			sa := NewSemanticAnalyzer()
			sa.analyzeExpression(letExpr, "Main")
			assertNoErrors(t, sa.Errors())
		})
	})

	t.Run("Type Conformance", func(t *testing.T) {
		t.Run("Invalid Assignment Type", func(t *testing.T) {
			cls := &ast.Class{
				Name: &ast.ObjectIdentifier{Value: "Test", Token: dummyToken},
				Features: []ast.Feature{
					&ast.Attribute{
						Name: &ast.ObjectIdentifier{Value: "count", Token: dummyToken},
						Type: &ast.TypeIdentifier{Value: "Int", Token: dummyToken},
						Init: &ast.StringLiteral{Value: "hello", Token: dummyToken},
					},
				},
			}
			sa := NewSemanticAnalyzer()
			sa.Analyze(&ast.Program{Classes: []*ast.Class{cls}})
			assertErrorsContain(t, sa.Errors(), "does not conform")
		})
	})

	t.Run("Case Expression Validation", func(t *testing.T) {
		t.Run("Missing Object Branch", func(t *testing.T) {
			caseExpr := &ast.CaseExpression{
				Expression: &ast.IntegerLiteral{Value: 5, Token: dummyToken},
				Cases: []*ast.Case{
					{
						Name: &ast.ObjectIdentifier{Value: "x", Token: dummyToken},
						Type: &ast.TypeIdentifier{Value: "Int", Token: dummyToken},
					},
				},
			}
			sa := NewSemanticAnalyzer()
			sa.analyzeExpression(caseExpr, "Main")
			assertErrorsContain(t, sa.Errors(), "Object branch")
		})
	})

	t.Run("Method Dispatch", func(t *testing.T) {
		t.Run("Undefined Method Call", func(t *testing.T) {
			methodCall := &ast.MethodCall{
				Method: &ast.ObjectIdentifier{Value: "undefined", Token: dummyToken},
				Object: &ast.NewExpression{
					Type: &ast.TypeIdentifier{Value: "Main", Token: dummyToken},
				},
			}
			sa := NewSemanticAnalyzer()
			sa.analyzeExpression(methodCall, "Main")
			assertErrorsContain(t, sa.Errors(), "undefined method")
		})
	})

	t.Run("SELF_TYPE Handling", func(t *testing.T) {
		t.Run("Valid SELF_TYPE Return", func(t *testing.T) {
			classes := []*ast.Class{
				{
					Name: &ast.ObjectIdentifier{Value: "Main", Token: dummyToken},
					Features: []ast.Feature{
						&ast.Method{
							Name:       &ast.ObjectIdentifier{Value: "main", Token: dummyToken},
							ReturnType: &ast.TypeIdentifier{Value: "Object", Token: dummyToken},
						},
					},
				},
				{
					Name: &ast.ObjectIdentifier{Value: "Test", Token: dummyToken},
					Features: []ast.Feature{
						&ast.Method{
							Name:       &ast.ObjectIdentifier{Value: "copy", Token: dummyToken},
							ReturnType: &ast.TypeIdentifier{Value: "SELF_TYPE", Token: dummyToken},
							Body:       &ast.ObjectIdentifier{Value: "self", Token: dummyToken},
						},
					},
				},
			}
			sa := NewSemanticAnalyzer()
			sa.Analyze(&ast.Program{Classes: classes})
			assertNoErrors(t, sa.Errors())
		})
	})
	t.Run("Static Dispatch", func(t *testing.T) {
		t.Run("Valid Parent Method Dispatch", func(t *testing.T) {
			classes := []*ast.Class{
				{
					Name: &ast.ObjectIdentifier{Value: "Main", Token: dummyToken},
					Features: []ast.Feature{
						&ast.Method{
							Name:       &ast.ObjectIdentifier{Value: "main", Token: dummyToken},
							ReturnType: &ast.TypeIdentifier{Value: "Object", Token: dummyToken},
						},
					},
				},
				{
					Name: &ast.ObjectIdentifier{Value: "Parent", Token: dummyToken},
					Features: []ast.Feature{
						&ast.Method{
							Name:       &ast.ObjectIdentifier{Value: "create", Token: dummyToken},
							ReturnType: &ast.TypeIdentifier{Value: "Parent", Token: dummyToken},
							Body: &ast.NewExpression{
								Type: &ast.TypeIdentifier{Value: "Parent", Token: dummyToken},
							},
						},
					},
				},
				{
					Name:   &ast.ObjectIdentifier{Value: "Child", Token: dummyToken},
					Parent: &ast.TypeIdentifier{Value: "Parent", Token: dummyToken},
					Features: []ast.Feature{
						&ast.Method{
							Name:       &ast.ObjectIdentifier{Value: "create", Token: dummyToken},
							ReturnType: &ast.TypeIdentifier{Value: "Child", Token: dummyToken},
							Body: &ast.NewExpression{
								Type: &ast.TypeIdentifier{Value: "Child", Token: dummyToken},
							},
						},
					},
				},
			}

			methodCall := &ast.MethodCall{
				Object: &ast.NewExpression{Type: &ast.TypeIdentifier{Value: "Child", Token: dummyToken}},
				Type:   &ast.TypeIdentifier{Value: "Parent", Token: dummyToken},
				Method: &ast.ObjectIdentifier{Value: "create", Token: dummyToken},
			}

			sa := NewSemanticAnalyzer()
			sa.Analyze(&ast.Program{Classes: classes})
			sa.analyzeExpression(methodCall, "Child")
			assertNoErrors(t, sa.Errors())
		})
	})

	t.Run("Control Flow Structures", func(t *testing.T) {
		t.Run("If Expression Type Validation", func(t *testing.T) {
			ifExpr := &ast.IfExpression{
				Condition:   &ast.IntegerLiteral{Value: 5},
				Consequence: &ast.IntegerLiteral{Value: 10},
				Alternative: &ast.IntegerLiteral{Value: 20},
			}
			sa := NewSemanticAnalyzer()
			sa.analyzeExpression(ifExpr, "Main")
			assertErrorsContain(t, sa.Errors(), "must be Bool")
		})

		t.Run("While Loop Validation", func(t *testing.T) {
			whileExpr := &ast.WhileExpression{
				Condition: &ast.BooleanLiteral{Value: true},
				Body:      &ast.IntegerLiteral{Value: 42},
			}
			sa := NewSemanticAnalyzer()
			sa.analyzeExpression(whileExpr, "Main")
			assertNoErrors(t, sa.Errors())
		})
	})

	t.Run("Attribute Initialization", func(t *testing.T) {
		t.Run("Self in Initialization", func(t *testing.T) {
			classes := []*ast.Class{
				{
					Name: &ast.ObjectIdentifier{Value: "Main", Token: dummyToken},
					Features: []ast.Feature{
						&ast.Method{
							Name:       &ast.ObjectIdentifier{Value: "main", Token: dummyToken},
							ReturnType: &ast.TypeIdentifier{Value: "Object", Token: dummyToken},
						},
					},
				},
				{
					Name: &ast.ObjectIdentifier{Value: "Test", Token: dummyToken},
					Features: []ast.Feature{
						&ast.Attribute{
							Name: &ast.ObjectIdentifier{Value: "copy_attr", Token: dummyToken},
							Type: &ast.TypeIdentifier{Value: "Test", Token: dummyToken},
							Init: &ast.MethodCall{
								Object: &ast.ObjectIdentifier{Value: "self", Token: dummyToken},
								Method: &ast.ObjectIdentifier{Value: "copy", Token: dummyToken},
							},
						},
					},
				},
			}
			sa := NewSemanticAnalyzer()
			sa.Analyze(&ast.Program{Classes: classes})
			assertNoErrors(t, sa.Errors())
		})
	})

	t.Run("Void Handling", func(t *testing.T) {
		t.Run("Dispatch on Void", func(t *testing.T) {
			methodCall := &ast.MethodCall{
				Object: &ast.IsVoidExpression{
					Expression: &ast.NewExpression{Type: &ast.TypeIdentifier{Value: "Object"}},
				},
				Method: &ast.ObjectIdentifier{Value: "abort"},
			}
			sa := NewSemanticAnalyzer()
			sa.analyzeExpression(methodCall, "Main")
			assertErrorsContain(t, sa.Errors(), "dispatch on void")
		})
	})

	t.Run("Default Values", func(t *testing.T) {
		t.Run("Attribute Default Initialization", func(t *testing.T) {
			cls := &ast.Class{
				Name: &ast.ObjectIdentifier{Value: "Test"},
				Features: []ast.Feature{
					&ast.Attribute{
						Name: &ast.ObjectIdentifier{Value: "count"},
						Type: &ast.TypeIdentifier{Value: "Int"},
						// No Init specified
					},
				},
			}
			sa := NewSemanticAnalyzer()
			sa.Analyze(&ast.Program{Classes: []*ast.Class{cls}})

			classSym := sa.symbolTable.Classes["Test"]
			attrSym := classSym.Features["count"]
			if attrSym.IsInitialized {
				t.Error("Attribute should use default initialization")
			}
		})
	})

	t.Run("Self Constraints", func(t *testing.T) {
		t.Run("Self as Attribute Name", func(t *testing.T) {
			cls := &ast.Class{
				Name: &ast.ObjectIdentifier{Value: "Bad"},
				Features: []ast.Feature{
					&ast.Attribute{
						Name: &ast.ObjectIdentifier{Value: "self"}, // Invalid
						Type: &ast.TypeIdentifier{Value: "Object"},
					},
				},
			}
			sa := NewSemanticAnalyzer()
			sa.Analyze(&ast.Program{Classes: []*ast.Class{cls}})
			assertErrorsContain(t, sa.Errors(), "cannot be named 'self'")
		})
	})

	t.Run("Inheritance Cycle Detection", func(t *testing.T) {
		t.Run("Direct Self-Inheritance", func(t *testing.T) {
			classes := []*ast.Class{
				{
					Name:   &ast.ObjectIdentifier{Value: "Bad", Token: dummyToken},
					Parent: &ast.TypeIdentifier{Value: "Bad", Token: dummyToken}, // Class inherits from itself
					Features: []ast.Feature{
						&ast.Method{
							Name:       &ast.ObjectIdentifier{Value: "main", Token: dummyToken},
							ReturnType: &ast.TypeIdentifier{Value: "Object", Token: dummyToken},
						},
					},
				},
			}
			sa := NewSemanticAnalyzer()
			sa.Analyze(&ast.Program{Classes: classes})
			assertErrorsContain(t, sa.Errors(), "inheritance cycle detected")
		})

		t.Run("Indirect Cycle Through Multiple Classes", func(t *testing.T) {
			classes := []*ast.Class{
				{
					Name:   &ast.ObjectIdentifier{Value: "A", Token: dummyToken},
					Parent: &ast.TypeIdentifier{Value: "B", Token: dummyToken},
				},
				{
					Name:   &ast.ObjectIdentifier{Value: "B", Token: dummyToken},
					Parent: &ast.TypeIdentifier{Value: "C", Token: dummyToken},
				},
				{
					Name:   &ast.ObjectIdentifier{Value: "C", Token: dummyToken},
					Parent: &ast.TypeIdentifier{Value: "A", Token: dummyToken}, // Creates cycle A -> B -> C -> A
				},
			}
			sa := NewSemanticAnalyzer()
			sa.Analyze(&ast.Program{Classes: classes})
			assertErrorsContain(t, sa.Errors(), "inheritance cycle detected")
		})

		t.Run("Multiple Independent Cycles", func(t *testing.T) {
			classes := []*ast.Class{
				// First cycle
				{
					Name:   &ast.ObjectIdentifier{Value: "A", Token: dummyToken},
					Parent: &ast.TypeIdentifier{Value: "B", Token: dummyToken},
				},
				{
					Name:   &ast.ObjectIdentifier{Value: "B", Token: dummyToken},
					Parent: &ast.TypeIdentifier{Value: "A", Token: dummyToken},
				},
				// Second cycle
				{
					Name:   &ast.ObjectIdentifier{Value: "X", Token: dummyToken},
					Parent: &ast.TypeIdentifier{Value: "Y", Token: dummyToken},
				},
				{
					Name:   &ast.ObjectIdentifier{Value: "Y", Token: dummyToken},
					Parent: &ast.TypeIdentifier{Value: "X", Token: dummyToken},
				},
			}
			sa := NewSemanticAnalyzer()
			sa.Analyze(&ast.Program{Classes: classes})

			errors := sa.Errors()
			cycleCount := 0
			for _, err := range errors {
				if strings.Contains(err, "inheritance cycle detected") {
					cycleCount++
				}
			}
			if cycleCount < 2 {
				t.Errorf("Expected at least 2 cycle detection errors, got %d", cycleCount)
			}
		})

		t.Run("Valid Complex Inheritance", func(t *testing.T) {
			classes := []*ast.Class{
				// Add Main class with required main method
				{
					Name: &ast.ObjectIdentifier{Value: "Main", Token: dummyToken},
					Features: []ast.Feature{
						&ast.Method{
							Name:       &ast.ObjectIdentifier{Value: "main", Token: dummyToken},
							ReturnType: &ast.TypeIdentifier{Value: "Object", Token: dummyToken},
							Body:       &ast.ObjectIdentifier{Value: "self", Token: dummyToken},
						},
					},
				},
				{
					Name:   &ast.ObjectIdentifier{Value: "D", Token: dummyToken},
					Parent: &ast.TypeIdentifier{Value: "C", Token: dummyToken},
				},
				{
					Name:   &ast.ObjectIdentifier{Value: "C", Token: dummyToken},
					Parent: &ast.TypeIdentifier{Value: "B", Token: dummyToken},
				},
				{
					Name:   &ast.ObjectIdentifier{Value: "B", Token: dummyToken},
					Parent: &ast.TypeIdentifier{Value: "A", Token: dummyToken},
				},
				{
					Name: &ast.ObjectIdentifier{Value: "A", Token: dummyToken},
					// No parent specified, should default to Object
				},
			}
			sa := NewSemanticAnalyzer()
			sa.Analyze(&ast.Program{Classes: classes})
			assertNoErrors(t, sa.Errors())
		})

		t.Run("Inheritance from Undefined Class", func(t *testing.T) {
			classes := []*ast.Class{
				{
					Name:   &ast.ObjectIdentifier{Value: "Bad", Token: dummyToken},
					Parent: &ast.TypeIdentifier{Value: "NonExistent", Token: dummyToken},
				},
			}
			sa := NewSemanticAnalyzer()
			sa.Analyze(&ast.Program{Classes: classes})
			assertErrorsContain(t, sa.Errors(), "inherits from undefined class")
		})
	})

	t.Run("Class Definition Validation", func(t *testing.T) {
		t.Run("Duplicate Class Definitions", func(t *testing.T) {
			classes := []*ast.Class{
				// Main class
				{
					Name: &ast.ObjectIdentifier{Value: "Main", Token: dummyToken},
					Features: []ast.Feature{
						&ast.Method{
							Name:       &ast.ObjectIdentifier{Value: "main", Token: dummyToken},
							ReturnType: &ast.TypeIdentifier{Value: "Object", Token: dummyToken},
							Body:       &ast.ObjectIdentifier{Value: "self", Token: dummyToken},
						},
					},
				},
				// First definition of class A
				{
					Name: &ast.ObjectIdentifier{Value: "A", Token: dummyToken},
					Features: []ast.Feature{
						&ast.Method{
							Name:       &ast.ObjectIdentifier{Value: "foo", Token: dummyToken},
							ReturnType: &ast.TypeIdentifier{Value: "Int", Token: dummyToken},
							Body:       &ast.IntegerLiteral{Value: 1, Token: dummyToken},
						},
					},
				},
				// Duplicate definition of class A
				{
					Name: &ast.ObjectIdentifier{Value: "A", Token: dummyToken},
					Features: []ast.Feature{
						&ast.Method{
							Name:       &ast.ObjectIdentifier{Value: "bar", Token: dummyToken},
							ReturnType: &ast.TypeIdentifier{Value: "Int", Token: dummyToken},
							Body:       &ast.IntegerLiteral{Value: 2, Token: dummyToken},
						},
					},
				},
				// Attempt to redefine a basic class
				{
					Name: &ast.ObjectIdentifier{Value: "Int", Token: dummyToken},
					Features: []ast.Feature{
						&ast.Method{
							Name:       &ast.ObjectIdentifier{Value: "test", Token: dummyToken},
							ReturnType: &ast.TypeIdentifier{Value: "Int", Token: dummyToken},
							Body:       &ast.IntegerLiteral{Value: 0, Token: dummyToken},
						},
					},
				},
			}
			sa := NewSemanticAnalyzer()
			sa.Analyze(&ast.Program{Classes: classes})

			errors := sa.Errors()

			// Check for class A redefinition error
			assertErrorsContain(t, errors, "Class A is redefined")

			// Check for basic class Int redefinition error
			assertErrorsContain(t, errors, "Class Int cannot be redefined")

			// Count the number of redefinition errors
			redefinitionErrors := 0
			for _, err := range errors {
				if strings.Contains(err, "redefined") || strings.Contains(err, "cannot be redefined") {
					redefinitionErrors++
				}
			}

			if redefinitionErrors != 2 {
				t.Errorf("Expected exactly 2 redefinition errors, got %d", redefinitionErrors)
			}
		})
	})
}

func assertErrorsContain(t *testing.T, errors []string, substr string) {
	t.Helper()
	found := false
	fmt.Printf("Looking for error containing: %q\n", substr)
	fmt.Printf("Got errors:\n")
	for _, err := range errors {
		fmt.Printf("  - %s\n", err)
		if strings.Contains(err, substr) {
			found = true
		}
	}
	if !found {
		t.Errorf("Expected error containing %q, got: %v", substr, errors)
	}
}

func assertNoErrors(t *testing.T, errors []string) {
	t.Helper()
	if len(errors) > 0 {
		t.Errorf("Expected no errors, got: %v", errors)
	}
}
