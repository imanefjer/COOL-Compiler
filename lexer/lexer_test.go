package lexer

import (
	"strings"
	"testing"
)

type lexerTest struct {
	input             string
	expectedTokenType []TokenType
	expectedLiteral   []string
}
func TestNextToken(t *testing.T) {
	tests := []struct {
		input             string
		expectedTokenType []TokenType
		expectedLiteral   []string
	}{
		{
			"class Main {};",
			[]TokenType{CLASS, TYPEID, LBRACE, RBRACE, SEMI, EOF},
			[]string{"class", "Main", "{", "}", ";", ""},
		},
		{
			"x <- true; -- One line comment\nx <- false;",
			[]TokenType{OBJECTID, ASSIGN, BOOL_CONST, SEMI, OBJECTID, ASSIGN, BOOL_CONST, SEMI, EOF},
			[]string{"x", "<-", "true", ";", "x", "<-", "false", ";", ""},
		},
		{
			"_a <- 0; b   <- _a <= \"1\\n\";",
			[]TokenType{OBJECTID, ASSIGN, INT_CONST, SEMI, OBJECTID, ASSIGN, OBJECTID, LE, STR_CONST, SEMI, EOF},
			[]string{"_a", "<-", "0", ";", "b", "<-", "_a", "<=", "1\n", ";", ""},
		},
		{
			"{true\n1\n\"some string\"\n}",
			[]TokenType{LBRACE, BOOL_CONST, INT_CONST, STR_CONST, RBRACE, EOF},
			[]string{"{", "true", "1", "some string", "}", ""},
		},
		{
			"{true\n1\n\"some string\"}",
			[]TokenType{LBRACE, BOOL_CONST, INT_CONST, STR_CONST, RBRACE, EOF},
			[]string{"{", "true", "1", "some string", "}", ""},
		},
		{
			"let a:A in true",
			[]TokenType{LET, OBJECTID, COLON, TYPEID, IN, BOOL_CONST, EOF},
			[]string{"let", "a", ":", "A", "in", "true", ""},
		},
		{
			"case a of b:B => false esac",
			[]TokenType{CASE, OBJECTID, OF, OBJECTID, COLON, TYPEID, DARROW, BOOL_CONST, ESAC, EOF},
			[]string{"case", "a", "of", "b", ":", "B", "=>", "false", "esac", ""},
		},
	}

	for _, tt := range tests {
		l := NewLexer(strings.NewReader(tt.input))
		for i, expTType := range tt.expectedTokenType {
			tok := l.NextToken()

			if tok.Type != expTType {
				t.Fatalf("[%q]: Wrong token type %d-th Token. expected=%s, got %s", tt.input, i, expTType, tok.Type)
			}

			if tok.Literal != tt.expectedLiteral[i] {
				t.Fatalf("[%q]: Wrong literal at test %d-it Token. expected=%q, got %q", tt.input, i, tt.expectedLiteral[i], tok.Literal)
			}
		}
	}
}

func TestClassDeclaration(t *testing.T) {
	test := lexerTest{
		input: "class List {",
		expectedTokenType: []TokenType{
			CLASS, TYPEID, LBRACE,
		},
		expectedLiteral: []string{
			"class", "List", "{",
		},
	}
	runLexerTest(t, test)
}

func TestAttributeDeclarations(t *testing.T) {
	tests := []lexerTest{
		{
			input: "    item: String;",
			expectedTokenType: []TokenType{
				OBJECTID, COLON, TYPEID, SEMI,
			},
			expectedLiteral: []string{
				"item", ":", "String", ";",
			},
		},
		{
			input: "    next: List;",
			expectedTokenType: []TokenType{
				OBJECTID, COLON, TYPEID, SEMI,
			},
			expectedLiteral: []string{
				"next", ":", "List", ";",
			},
		},
	}
	for _, test := range tests {
		runLexerTest(t, test)
	}
}

func TestMethodHeader(t *testing.T) {
	test := lexerTest{
		input: "    init(i: String, n: List): List {",
		expectedTokenType: []TokenType{
			OBJECTID, LPAREN,
			OBJECTID, COLON, TYPEID, COMMA,
			OBJECTID, COLON, TYPEID,
			RPAREN, COLON, TYPEID, LBRACE,
		},
		expectedLiteral: []string{
			"init", "(",
			"i", ":", "String", ",",
			"n", ":", "List",
			")", ":", "List", "{",
		},
	}
	runLexerTest(t, test)
}

func TestMethodBody(t *testing.T) {
	tests := []lexerTest{
		{
			input: "        {",
			expectedTokenType: []TokenType{
				LBRACE,
			},
			expectedLiteral: []string{
				"{",
			},
		},
		{
			input: "            item <- i;",
			expectedTokenType: []TokenType{
				OBJECTID, ASSIGN, OBJECTID, SEMI,
			},
			expectedLiteral: []string{
				"item", "<-", "i", ";",
			},
		},
		{
			input: "            next <- n;",
			expectedTokenType: []TokenType{
				OBJECTID, ASSIGN, OBJECTID, SEMI,
			},
			expectedLiteral: []string{
				"next", "<-", "n", ";",
			},
		},
		{
			input: "            self;",
			expectedTokenType: []TokenType{
				OBJECTID, SEMI,
			},
			expectedLiteral: []string{
				"self", ";",
			},
		},
		{
			input: "        }",
			expectedTokenType: []TokenType{
				RBRACE,
			},
			expectedLiteral: []string{
				"}",
			},
		},
	}
	for _, test := range tests {
		runLexerTest(t, test)
	}
}

func TestPrintMethod(t *testing.T) {
	tests := []lexerTest{
		{
			input: "    print(): Object {",
			expectedTokenType: []TokenType{
				OBJECTID, LPAREN, RPAREN, COLON, TYPEID, LBRACE,
			},
			expectedLiteral: []string{
				"print", "(", ")", ":", "Object", "{",
			},
		},
		{
			input: "            out_string(item);",
			expectedTokenType: []TokenType{
				OBJECTID, LPAREN, OBJECTID, RPAREN, SEMI,
			},
			expectedLiteral: []string{
				"out_string", "(", "item", ")", ";",
			},
		},
	}
	for _, test := range tests {
		runLexerTest(t, test)
	}
}

func TestIfStatement(t *testing.T) {
	test := lexerTest{
		input: "            if not (isvoid next) then",
		expectedTokenType: []TokenType{
			IF, NOT, LPAREN, ISVOID, OBJECTID, RPAREN, THEN,
		},
		expectedLiteral: []string{
			"if", "not", "(", "isvoid", "next", ")", "then",
		},
	}
	runLexerTest(t, test)
}

func TestMethodCall(t *testing.T) {
	test := lexerTest{
		input: "                next.print()",
		expectedTokenType: []TokenType{
			OBJECTID, DOT, OBJECTID, LPAREN, RPAREN,
		},
		expectedLiteral: []string{
			"next", ".", "print", "(", ")",
		},
	}
	runLexerTest(t, test)
}

func TestElseBlock(t *testing.T) {
	tests := []lexerTest{
		{
			input: "            else",
			expectedTokenType: []TokenType{
				ELSE,
			},
			expectedLiteral: []string{
				"else",
			},
		},
		{
			input: "                true",
			expectedTokenType: []TokenType{
				BOOL_CONST,
			},
			expectedLiteral: []string{
				"true",
			},
		},
		{
			input: "            fi;",
			expectedTokenType: []TokenType{
				FI, SEMI,
			},
			expectedLiteral: []string{
				"fi", ";",
			},
		},
	}
	for _, test := range tests {
		runLexerTest(t, test)
	}
}

func TestLetExpression(t *testing.T) {
	tests := []lexerTest{
		{
			input: "        let list: List <- new List,",
			expectedTokenType: []TokenType{
				LET, OBJECTID, COLON, TYPEID, ASSIGN, NEW, TYPEID, COMMA,
			},
			expectedLiteral: []string{
				"let", "list", ":", "List", "<-", "new", "List", ",",
			},
		},
		{
			input: "            list2: List <- new List",
			expectedTokenType: []TokenType{
				OBJECTID, COLON, TYPEID, ASSIGN, NEW, TYPEID,
			},
			expectedLiteral: []string{
				"list2", ":", "List", "<-", "new", "List",
			},
		},
	}
	for _, test := range tests {
		runLexerTest(t, test)
	}
}

func TestStringLiterals(t *testing.T) {
	tests := []lexerTest{
		{
			input: `"Hello"`,
			expectedTokenType: []TokenType{
				STR_CONST,
			},
			expectedLiteral: []string{
				"Hello",
			},
		},
		{
			input: `"World"`,
			expectedTokenType: []TokenType{
				STR_CONST,
			},
			expectedLiteral: []string{
				"World",
			},
		},
	}
	for _, test := range tests {
		runLexerTest(t, test)
	}
}

func TestComments(t *testing.T) {
	tests := []lexerTest{
		{
			input: "-- This is a comment\nx",
			expectedTokenType: []TokenType{
				OBJECTID,
			},
			expectedLiteral: []string{
				"x",
			},
		},
		{
			input: "(* This is a\nmulti-line\ncomment *)\ny",
			expectedTokenType: []TokenType{
				OBJECTID,
			},
			expectedLiteral: []string{
				"y",
			},
		},
		{
			input: "x (* nested (* comment *) here *) y",
			expectedTokenType: []TokenType{
				OBJECTID, OBJECTID,
			},
			expectedLiteral: []string{
				"x", "y",
			},
		},
		{
			input: "(* Unterminated comment",
			expectedTokenType: []TokenType{
				EOF,
			},
			expectedLiteral: []string{
				"",
			},
		},
	}

	for _, test := range tests {
		runLexerTest(t, test)
	}
}


//  test for string escape sequences
func TestStringEscapeSequences(t *testing.T) {
	tests := []lexerTest{
		{
			input: `"String with \b backspace"`,
			expectedTokenType: []TokenType{
				STR_CONST,
			},
			expectedLiteral: []string{
				"String with \b backspace",
			},
		},
		{
			input: `"String with \t tab"`,
			expectedTokenType: []TokenType{
				STR_CONST,
			},
			expectedLiteral: []string{
				"String with \t tab",
			},
		},
		{
			input: `"String with \n newline"`,
			expectedTokenType: []TokenType{
				STR_CONST,
			},
			expectedLiteral: []string{
				"String with \n newline",
			},
		},
		{
			input: `"String with \f form feed"`,
			expectedTokenType: []TokenType{
				STR_CONST,
			},
			expectedLiteral: []string{
				"String with \f form feed",
			},
		},
		{
			input: `"String with \\ backslash"`,
			expectedTokenType: []TokenType{
				STR_CONST,
			},
			expectedLiteral: []string{
				"String with \\ backslash",
			},
		},
		{
			input: `"String with \" quote"`,
			expectedTokenType: []TokenType{
				STR_CONST,
			},
			expectedLiteral: []string{
				"String with \" quote",
			},
		},
	}

	for _, test := range tests {
		runLexerTest(t, test)
	}
}

//  test for nested comments
func TestNestedComments(t *testing.T) {
	tests := []lexerTest{
		{
			input:             "(* outer (* inner *) outer *)",
			expectedTokenType: []TokenType{},
			expectedLiteral:   []string{},
		},
		{
			input:             "(* level1 (* level2 (* level3 *) level2 *) level1 *)",
			expectedTokenType: []TokenType{},
			expectedLiteral:   []string{},
		},
		{
			input: "x (* comment *) y",
			expectedTokenType: []TokenType{
				OBJECTID, OBJECTID,
			},
			expectedLiteral: []string{
				"x", "y",
			},
		},
	}

	for _, test := range tests {
		runLexerTest(t, test)
	}
}

//  test for case insensitive keywords
func TestCaseInsensitiveKeywords(t *testing.T) {
	tests := []lexerTest{
		{
			input: "CLASS Class class CLASS",
			expectedTokenType: []TokenType{
				CLASS, CLASS, CLASS, CLASS,
			},
			expectedLiteral: []string{
				"CLASS", "Class", "class", "CLASS",
			},
		},
		{
			input: "IF if If iF",
			expectedTokenType: []TokenType{
				IF, IF, IF, IF,
			},
			expectedLiteral: []string{
				"IF", "if", "If", "iF",
			},
		},
		{
			input: "true TRUE TrUe",
			expectedTokenType: []TokenType{
				BOOL_CONST, BOOL_CONST, BOOL_CONST,
			},
			expectedLiteral: []string{
				"true", "TRUE", "TrUe",
			},
		},
	}

	for _, test := range tests {
		runLexerTest(t, test)
	}
}

// Helper function to run lexer tests
func runLexerTest(t *testing.T, test lexerTest) {
	l := NewLexer(strings.NewReader(test.input))

	for i, expectedType := range test.expectedTokenType {
		tok := l.NextToken()

		if tok.Type != expectedType {
			t.Errorf("test [%q] - wrong token type. expected=%q, got=%q",
				test.input, expectedType, tok.Type)
		}

		if tok.Literal != test.expectedLiteral[i] {
			t.Errorf("test [%q] - wrong literal. expected=%q, got=%q",
				test.input, test.expectedLiteral[i], tok.Literal)
		}
	}

	// Check that there are no extra tokens
	tok := l.NextToken()
	if tok.Type != EOF && len(test.expectedTokenType) > 0 {
		t.Errorf("test [%q] - extra token found: %q", test.input, tok)
	}
}
