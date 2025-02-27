package parser

import (
	"cool-compiler/ast"
	"cool-compiler/lexer"
	"fmt"
	"strings"
	"testing"
)

// Helper function to create a parser from input string
func newParser(input string) *Parser {
	l := lexer.NewLexer(strings.NewReader(input))
	return New(l)
}

// Helper function to check parser errors
func checkParserErrors(t *testing.T, p *Parser) {
	errors := p.Errors()
	if len(errors) == 0 {
		return
	}

	t.Errorf("parser has %d errors", len(errors))
	for _, msg := range errors {
		t.Errorf("parser error: %q", msg)
	}
	t.FailNow()
}

// Helper function to assert number of classes
func assertClassCount(t *testing.T, program *ast.Program, expected int) {
	if len(program.Classes) != expected {
		t.Fatalf("program.Classes does not contain %d classes. got=%d",
			expected, len(program.Classes))
	}
}

// Helper function to assert class name
func assertClassName(t *testing.T, class *ast.Class, expected string) {
	if class.Name.Value != expected {
		t.Errorf("class.Name.Value not '%s'. got=%s", expected, class.Name.Value)
	}
}

// Helper function to assert parent class
func assertParentClass(t *testing.T, class *ast.Class, expected string) {
	if class.Parent.Value != expected {
		t.Errorf("class.Parent.Value not '%s'. got=%s", expected, class.Parent.Value)
	}
}

// Test basic class parsing
func TestBasicClassParsing(t *testing.T) {
	tests := []struct {
		input          string
		expectedClass  string
		expectedParent string
	}{
		{"class A {};", "A", "Object"},
		{"class B inherits A {};", "B", "A"},
	}

	for _, tt := range tests {
		p := newParser(tt.input)
		program := p.ParseProgram()
		checkParserErrors(t, p)

		assertClassCount(t, program, 1)
		class := program.Classes[0]
		assertClassName(t, class, tt.expectedClass)
		assertParentClass(t, class, tt.expectedParent)
	}
}

// Test class feature parsing (methods and attributes)
func TestClassFeatureParsing(t *testing.T) {
	input := `
		class Test {
			x: Int;
			y: String <- "hello";
			method(): Int { 42 };
		};
	`

	p := newParser(input)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	assertClassCount(t, program, 1)
	class := program.Classes[0]

	if len(class.Features) != 3 {
		t.Fatalf("class.Features does not contain 3 features. got=%d",
			len(class.Features))
	}

	// Test attribute without initialization
	attr1, ok := class.Features[0].(*ast.Attribute)
	if !ok {
		t.Fatalf("class.Features[0] is not ast.Attribute. got=%T",
			class.Features[0])
	}
	if attr1.Name.Value != "x" || attr1.Type.Value != "Int" {
		t.Errorf("attribute x incorrect. got name=%s, type=%s",
			attr1.Name.Value, attr1.Type.Value)
	}

	// Test attribute with initialization
	attr2, ok := class.Features[1].(*ast.Attribute)
	if !ok {
		t.Fatalf("class.Features[1] is not ast.Attribute. got=%T",
			class.Features[1])
	}
	if attr2.Name.Value != "y" || attr2.Type.Value != "String" {
		t.Errorf("attribute y incorrect. got name=%s, type=%s",
			attr2.Name.Value, attr2.Type.Value)
	}

	// Test method
	method, ok := class.Features[2].(*ast.Method)
	if !ok {
		t.Fatalf("class.Features[2] is not ast.Method. got=%T",
			class.Features[2])
	}
	if method.Name.Value != "method" || method.ReturnType.Value != "Int" {
		t.Errorf("method incorrect. got name=%s, return type=%s",
			method.Name.Value, method.ReturnType.Value)
	}
}

// Test method parameter parsing
func TestMethodParameterParsing(t *testing.T) {
	input := `
		class Test {
			method(x: Int, y: String, z: Bool): Int { 42 };
		};
	`

	p := newParser(input)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	method := program.Classes[0].Features[0].(*ast.Method)
	if len(method.Parameters) != 3 {
		t.Fatalf("method.Parameters does not contain 3 parameters. got=%d",
			len(method.Parameters))
	}

	expectedParams := []struct {
		name string
		typ  string
	}{
		{"x", "Int"},
		{"y", "String"},
		{"z", "Bool"},
	}

	for i, expected := range expectedParams {
		param := method.Parameters[i]
		if param.Name.Value != expected.name || param.Type.Value != expected.typ {
			t.Errorf("parameter %d incorrect. expected name=%s, type=%s, got name=%s, type=%s",
				i, expected.name, expected.typ, param.Name.Value, param.Type.Value)
		}
	}
}

// Test expression parsing
func TestExpressionParsing(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{
			`class Test {
				method(): Int { 42 };
			};`,
			"*ast.IntegerLiteral",
		},
		{
			`class Test {
				method(): Bool { true };
			};`,
			"*ast.BooleanLiteral",
		},
		{
			`class Test {
				method(): String { "hello" };
			};`,
			"*ast.StringLiteral",
		},
	}

	for _, tt := range tests {
		p := newParser(tt.input)
		program := p.ParseProgram()
		checkParserErrors(t, p)

		method := program.Classes[0].Features[0].(*ast.Method)
		if method.Body == nil {
			t.Fatalf("method.Body is nil")
		}

		actual := fmt.Sprintf("%T", method.Body)
		if actual != tt.expected {
			t.Errorf("expected=%q, got=%q", tt.expected, actual)
		}
	}
}

// Test complex expressions and control structures
func TestComplexExpressionParsing(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		// If expression
		{
			`class Test {
				method(): Int {
					if num1 < num2 then num1 else num2 fi
				};
			};`,
			"*ast.IfExpression",
		},
		// While loop
		{
			`class Test {
				method(): Int {
					while num1 < num2 loop num1 + num2 pool
				};
			};`,
			"*ast.WhileExpression",
		},
		// Case expression
		{
			`class Test {
				method(): Object {
					case num1 + num2 of
						x: Int => x;
						y: String => y;
						z: Bool => z;
					esac
				};
			};`,
			"*ast.CaseExpression",
		},
		// Let expression
		{
			`class Test {
				method(): Int {
					let x: Int <- 5,
						y: Int <- 10,
						z: String <- "test" in
						x + y
				};
			};`,
			"*ast.LetExpression",
		},
		// Method call with static dispatch
		{
			`class Test {
				method(): Int {
					num1@Sum.sum(num2, 3, 4)
				};
			};`,
			"*ast.MethodCall",
		},
		// Block expression
		{
			`class Test {
				method(): Int {
					{
						num1 <- 5;
						num2 <- 10;
						num1 + num2;
					}
				};
			};`,
			"*ast.BlockExpression",
		},
	}

	for _, tt := range tests {
		p := newParser(tt.input)
		program := p.ParseProgram()
		checkParserErrors(t, p)

		method := program.Classes[0].Features[0].(*ast.Method)
		if method.Body == nil {
			t.Fatalf("method.Body is nil")
		}

		actual := fmt.Sprintf("%T", method.Body)
		if actual != tt.expected {
			t.Errorf("expected=%q, got=%q", tt.expected, actual)
		}
	}
}

// Test operator precedence
func TestOperatorPrecedence(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		// Arithmetic operators
		{
			`class Test {
				method(): Int { num1 + num2 * num3 };
			};`,
			"*ast.InfixExpression",
		},
		// Comparison operators
		{
			`class Test {
				method(): Bool { num1 <= num2 = num3 };
			};`,
			"*ast.InfixExpression",
		},
	}

	for _, tt := range tests {
		p := newParser(tt.input)
		program := p.ParseProgram()
		checkParserErrors(t, p)

		method := program.Classes[0].Features[0].(*ast.Method)
		if method.Body == nil {
			t.Fatalf("method.Body is nil")
		}

		actual := fmt.Sprintf("%T", method.Body)
		if actual != tt.expected {
			t.Errorf("expected=%q, got=%q", tt.expected, actual)
		}
	}
}

// Test error cases
func TestParserErrors(t *testing.T) {
	tests := []struct {
		input          string
		expectedErrors int
		expectedErrMsg []string
	}{
		{
			// Missing semicolon after class
			`class Test {}`,
			1,
			[]string{"Expected next token to be SEMI"},
		},
		{
			// Invalid class declaration
			`class {};`,
			1,
			[]string{"Expected next token to be TYPEID"},
		},
		{
			// Invalid method declaration
			`class Test {
				method(: Int { 42 };
			};`,
			2,
			[]string{
				"Expected parameter name",
				"Expected next token to be SEMI",
			},
		},
		{
			// Invalid let binding
			`class Test {
				method(): Int {
					let Int <- 5 in x
				};
			};`,
			2,
			[]string{
				"Expected next token to be OBJECTID",
				"Expected next token to be SEMI",
			},
		},
		{
			// Invalid method parameter syntax
			`class Test {
				method(): Int (x:) { 42 };
			};`,
			2,
			[]string{
				"Expected next token to be LBRACE",
				"Expected next token to be SEMI",
			},
		},
	}

	for _, tt := range tests {
		p := newParser(tt.input)
		p.ParseProgram()
		errors := p.Errors()

		if len(errors) != tt.expectedErrors {
			t.Errorf("expected %d errors, got %d for input:\n%s\nErrors: %v",
				tt.expectedErrors, len(errors), tt.input, errors)
			continue
		}

		// Check each expected error message
		for i, expectedMsg := range tt.expectedErrMsg {
			if i < len(errors) && !strings.Contains(errors[i], expectedMsg) {
				t.Errorf("expected error message to contain %q, got %q\nfor input:\n%s",
					expectedMsg, errors[i], tt.input)
			}
		}
	}
}

// Test method calls and dispatch
func TestMethodCallParsing(t *testing.T) {
	tests := []struct {
		input   string
		checkFn func(*testing.T, ast.Expression)
	}{
		{
			// Simple method call
			`class Test {
				method(): Int { sum(1, 2, 3) };
			};`,
			func(t *testing.T, exp ast.Expression) {
				call, ok := exp.(*ast.FunctionCall)
				if !ok {
					t.Fatalf("exp not *ast.FunctionCall. got=%T", exp)
				}
				if len(call.Arguments) != 3 {
					t.Errorf("wrong number of arguments. got=%d", len(call.Arguments))
				}
			},
		},
		{
			// Method call with static dispatch
			`class Test {
				method(): Int { obj@Type.method(x, y) };
			};`,
			func(t *testing.T, exp ast.Expression) {
				call, ok := exp.(*ast.MethodCall)
				if !ok {
					t.Fatalf("exp not *ast.MethodCall. got=%T", exp)
				}
				if call.Type == nil {
					t.Error("static dispatch type is nil")
				}
				if len(call.Arguments) != 2 {
					t.Errorf("wrong number of arguments. got=%d", len(call.Arguments))
				}
			},
		},
	}

	for _, tt := range tests {
		p := newParser(tt.input)
		program := p.ParseProgram()
		checkParserErrors(t, p)

		method := program.Classes[0].Features[0].(*ast.Method)
		if method.Body == nil {
			t.Fatalf("method.Body is nil")
		}

		tt.checkFn(t, method.Body)
	}
}
