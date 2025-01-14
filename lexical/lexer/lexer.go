package lexer

import (
	"bufio"
	"fmt"
	"io"
	"strings"
	"unicode"
)

type TokenType int

// The list of token types
const (
	EOF TokenType = iota
	ERROR
	LBRACE
	RBRACE
	CLASS
	TYPEID
	SEMI
	OBJECTID
	LPAREN
	RPAREN
	COLON
	DARROW
	LET
	IN
	BOOL_CONST
	ASSIGN
	INT_CONST
	LE
	CASE
	OF
	ESAC
	STR_CONST
)

func (tt TokenType) String() string {
	return [...]string{"EOF", "ERROR", "LBRACE", "RBRACE", "CLASS", "TYPEID", "SEMI", "OBJECTID", "LPAREN", "RPAREN", "COLON", "DARROW", "LET", "IN", "BOOL_CONST", "ASSIGN", "INT_CONST", "LE", "CASE", "OF", "ESAC", "STR_CONST"}[tt]

}

// Token represents a lexical token with its type, value, and position.
type Token struct {
	Type    TokenType
	Literal string
}

// Lexer is the lexical analyzer.
type Lexer struct {
	reader *bufio.Reader
	line   int
	column int
	char   rune
}

// NewLexer creates a new lexer from an io.Reader
func NewLexer(reader io.Reader) *Lexer {
	l := &Lexer{
		reader: bufio.NewReader(reader),
		line:   1,
		column: 0,
		char:   ' ',
	}
	return l
}

// readChar reads the next character from the input.
func (l *Lexer) readChar() {
	var err error
	l.char, _, err = l.reader.ReadRune()
	if err != nil {
		l.char = 0 // EOF
	}

	l.column++
	if l.char == '\n' {
		l.line++
		l.column = 0
	}
}

// peekChar returns the next character without advancing the stream.
func (l *Lexer) peekChar() rune {
	char, _, err := l.reader.ReadRune()
	if err != nil {
		return 0
	}
	l.reader.UnreadRune()
	return char
}

// skipWhiteSpace whitespace characters.
func (l *Lexer) skipWhiteSpace() {
	for unicode.IsSpace(l.char) {
		l.readChar()
	}
}

func (l *Lexer) readNumber() string {
	var sb strings.Builder
	for unicode.IsDigit(l.char) {
		sb.WriteRune(l.char)
		l.readChar()
	}
	return sb.String()
}
func (l *Lexer) readString() string {
	var sb strings.Builder
	l.readChar()
	for l.char != '"' {
		sb.WriteRune(l.char)
		l.readChar()
	}
	l.readChar()
	return sb.String()
}

func (l *Lexer) readIdentifier() string {
	var sb strings.Builder
	for unicode.IsLetter(l.char) || unicode.IsDigit(l.char) || l.char == '_' {
		sb.WriteRune(l.char)
		l.readChar()
	}
	return sb.String()
}

// adding iteration for the lexer
func (l *Lexer) NextToken() Token {
	l.skipWhiteSpace()

	tok := Token{}

	switch {
	case l.char == '{':
		tok.Type = LBRACE
		tok.Literal = "{"
		l.readChar()
	case l.char == '}':
		tok.Type = RBRACE
		tok.Literal = "}"
		l.readChar()
	case l.char == ';':
		tok.Type = SEMI
		tok.Literal = ";"
		l.readChar()
	case l.char == '(':
		tok.Type = LPAREN
		tok.Literal = "("
		l.readChar()
	case l.char == ')':
		tok.Type = RPAREN
		tok.Literal = ")"
		l.readChar()
	case l.char == ':':
		tok.Type = COLON
		tok.Literal = ":"
		l.readChar()
	case l.char == '=' && l.peekChar() == '>':
		tok.Type = DARROW
		tok.Literal = "=>"
		l.readChar()
		l.readChar()
	case l.char == '<' && l.peekChar() == '-':
		tok.Type = ASSIGN
		tok.Literal = "<-"
		l.readChar()
		l.readChar()
	case l.char == '/' && l.peekChar() == '/':

		for l.char != '\n' {
			l.readChar()
		}
		l.NextToken()
	case l.char == '<' && l.peekChar() == '=':
		tok.Type = LE
		tok.Literal = "<="
	case unicode.IsDigit(l.char):
		number := l.readNumber()
		tok.Type = INT_CONST
		tok.Literal = number
	case l.char == '_':
		identifier := l.readIdentifier()
		tok.Type = OBJECTID
		tok.Literal = identifier

	case unicode.IsLetter(l.char):
		identifier := l.readIdentifier()
		var upper = unicode.IsUpper(rune(identifier[0]))
		switch identifier {
		case "class":
			tok.Type = CLASS
		case "let":
			tok.Type = LET
		case "in":
			tok.Type = IN
		case "true", "false":
			tok.Type = BOOL_CONST
		case "case":
			tok.Type = CASE
		case "of":
			tok.Type = OF
		case "esac":
			tok.Type = ESAC
		default:
			if upper {
				tok.Type = TYPEID
			} else {
				tok.Type = OBJECTID
			}
		}
		tok.Literal = identifier

	case unicode.IsDigit(l.char):
		tok.Type = OBJECTID
		tok.Literal = l.readNumber()

	case l.char == '\n':
		l.readChar()
	case l.char == '"':
		tok.Type = STR_CONST
		tok.Literal = l.readString()

	case l.char == '/':
		fmt.Println("Comment")

		l.readChar()
	case l.char == 0:
		tok.Type = EOF
		tok.Literal = ""
	default:
		tok.Type = ERROR
		tok.Literal = fmt.Sprintf("Unexpected character: %c", l.char)
		l.readChar()
	}

	return tok
}
