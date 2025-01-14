package parser

import (
	"cool-compiler/ast"
	"cool-compiler/lexer"
	"strings"
	"testing"
)

func newParserFromInput(input string) *Parser {
	l := lexer.NewLexer(strings.NewReader(input))
	return New(l)
}

func checkParserErrors(t *testing.T, p *Parser, i int) {
	errors := p.Errors()
	if len(errors) > 0 {
		t.Errorf("parser has %d errors for test case %d", len(errors), i)
		for _, msg := range errors {
			t.Errorf("parser error: %q", msg)
		}
		t.FailNow()
	}
}

func TestClassParser(t *testing.T) {
	tests := []struct {
		input          string
		expectedName   string
		expectedParent string
	}{
		{
			input:          "class Main {};",
			expectedName:   "Main",
			expectedParent: "",
		},
		{
			input:          "class A {age:Integer<-30;};",
			expectedName:   "A",
			expectedParent: "",
		},
		{
			input:          "class B {func(): Void {};};",
			expectedName:   "B",
			expectedParent: "",
		},
		{
			input:          "class B inherits A {func(): Void {};};",
			expectedName:   "B",
			expectedParent: "A",
		},
	}

	for i, tt := range tests {
		parser := newParserFromInput(tt.input)
		class := parser.parseClass()

		checkParserErrors(t, parser, i)

		if class.Name.Value != tt.expectedName {
			t.Fatalf("[%q]: expected class name to be %q got %q", tt.input, tt.expectedName, class.Name.Value)
		}

		if class.Parent != nil {
			if class.Parent.Value != tt.expectedParent {
				t.Fatalf("[%q]: expected class parent to be %q got %q", tt.input, tt.expectedParent, class.Parent.Value)
			}
		} else if tt.expectedParent != "" {
			t.Fatalf("[%q]: expected class parent to be %q got nil", tt.input, tt.expectedParent)
		}
	}
}

func TestFormalParsing(t *testing.T) {
	tests := []struct {
		input         string
		expectedNames []string
		expectedTypes []string
	}{
		{
			input:         "var1:Integer",
			expectedNames: []string{"var1"},
			expectedTypes: []string{"Integer"},
		},
		{
			input:         "var1:Integer,var2:Boolean,var3:String",
			expectedNames: []string{"var1", "var2", "var3"},
			expectedTypes: []string{"Integer", "Boolean", "String"},
		},
	}

	for _, tt := range tests {
		parser := newParserFromInput(tt.input)
		formals := parser.parseFormals()

		if len(parser.errors) > 0 {
			for _, err := range parser.errors {
				t.Errorf("Parsing Error %s\n", err)
			}
			t.Fatalf("[%q]: Found errors while parsing", tt.input)
		}

		if len(formals) != len(tt.expectedNames) {
			t.Fatalf("[%q]: expected %d formals got %d: %v", tt.input, len(tt.expectedNames), len(formals), formals)
		}

		for i, formal := range formals {
			if formal.Name.Value != tt.expectedNames[i] {
				t.Fatalf("[%q]: expected formal name to be %q got %q", tt.input, tt.expectedNames[i], formal.Name.Value)
			}
			if formal.TypeDecl.Value != tt.expectedTypes[i] {
				t.Fatalf("[%q]: expected formal type to be %q got %q", tt.input, tt.expectedNames[i], formal.Name.Value)
			}
		}
	}
}

func TestMethodParsing(t *testing.T) {
	tests := []struct {
		input               string
		expectedMethodName  string
		expectedFormalNames []string
		expectedFormalTypes []string
		expectedMethodType  string
	}{
		{
			input:               "Main(): Void {};",
			expectedMethodName:  "Main",
			expectedFormalNames: []string{},
			expectedFormalTypes: []string{},
			expectedMethodType:  "Void",
		},
		{
			input:               "Sum(a:Integer,b:Integer): Integer {};",
			expectedMethodName:  "Sum",
			expectedFormalNames: []string{"a", "b"},
			expectedFormalTypes: []string{"Integer", "Integer"},
			expectedMethodType:  "Integer",
		},
	}

	for i, tt := range tests {
		parser := newParserFromInput(tt.input)
		method := parser.parseMethod()
		checkParserErrors(t, parser, i)

		if method.Name.Value != tt.expectedMethodName {
			t.Fatalf("[%q]: Expected method name to be %q found %q", tt.input, tt.expectedMethodName, method.Name.Value)
		}

		for i, formal := range method.Formals {
			if formal.Name.Value != tt.expectedFormalNames[i] {
				t.Fatalf("[%q]: Expected formal name to be %q found %q", tt.input, tt.expectedFormalNames[i], formal.Name.Value)
			}
			if formal.TypeDecl.Value != tt.expectedFormalTypes[i] {
				t.Fatalf("[%q]: Expected formal type to be %q found %q", tt.input, tt.expectedFormalTypes[i], formal.TypeDecl.Value)
			}
		}

		if method.TypeDecl.Value != tt.expectedMethodType {
			t.Fatalf("[%q]: Expected method type to be %q found %q", tt.input, tt.expectedMethodType, method.TypeDecl.Value)
		}
	}
}

func TestAttributeParsing(t *testing.T) {
	tests := []struct {
		input              string
		expectedName       string
		expectedType       string
		expectedExpression ast.Expression
	}{
		{
			input:        "firstName:String",
			expectedName: "firstName",
			expectedType: "String",
		},
		{
			input:        "age:Integer<-0",
			expectedName: "age",
			expectedType: "Integer",
		},
	}

	for i, tt := range tests {
		parser := newParserFromInput(tt.input)
		attribute := parser.parseAttribute()

		checkParserErrors(t, parser, i)
		if attribute.Name.Value != tt.expectedName {
			t.Fatalf("[%q]: Expected attribute name to be %q got %q", tt.input, tt.expectedName, attribute.Name.Value)
		}
		if attribute.TypeDecl.Value != tt.expectedType {
			t.Fatalf("[%q]: Expected attribute type to be %q got %q", tt.input, tt.expectedType, attribute.TypeDecl.Value)
		}
	}
}

func TestExpressionParssing(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{"5", "5"},
		{`"hello world"`, `"hello world"`},
		{"true", "true"},
		{"false", "false"},
		{"x", "x"},
		{"not true", "(not true)"},
		{"1 + 2", "(1 + 2)"},
		{"1 < 2", "(1 < 2)"},
		{"1 <= 2", "(1 <= 2)"},
		{"~1", "(~ 1)"},
		{"1 = 2", "(1 = 2)"},
		{"1 * 2", "(1 * 2)"},
		{"isvoid 1", "isvoid 1"},
		{"1 / 2", "(1 / 2)"},
		// TODO: Implement parenthesis parsing
		// {"(1 + 2)", "(1 + 2)"},
		{"new Object", "new Object"},
		{"x <- 5", "(x <- 5)"},
		{"if true then 1 else 2 fi", "if true then 1 else 2 fi"},
		{"while true loop 1 pool", "while true loop 1 pool"},
	}

	for i, tt := range tests {
		p := newParserFromInput(tt.input)
		checkParserErrors(t, p, i)

		expression := p.parseExpression(START)
		actual := SerializeExpression(expression)
		if actual != tt.expected {
			t.Errorf("test [%d] expected expression to be '%s', got '%s'", i, tt.expected, actual)
		}
	}

}
