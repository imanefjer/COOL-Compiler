package lexer

import (
	"strings"
	"testing"
)

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
			"x <- true;// One line comment\nx <- false;",
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
