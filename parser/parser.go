package parser

import (
	"cool-compiler/ast"
	"cool-compiler/lexer"
	"fmt"
	"strconv"
)

const (
	_ int = iota
	LOWEST
	ASSIGN  // <- (lowest precedence)
	NOT     // not
	COMPARE // <=, <, = (non-associative)
	SUM     // +, -
	PRODUCT // *, /
	ISVOID  // isvoid
	NEG     // ~
	AT      // @
	DOT     // . (highest precedence)
)

var precedences = map[lexer.TokenType]int{
	lexer.ASSIGN: ASSIGN,  // <-
	lexer.NOT:    NOT,     // not
	lexer.EQ:     COMPARE, // =
	lexer.LE:     COMPARE, // <=
	lexer.LT:     COMPARE, // <
	lexer.PLUS:   SUM,     // +
	lexer.MINUS:  SUM,     // -
	lexer.TIMES:  PRODUCT, // *
	lexer.DIVIDE: PRODUCT, // /
	lexer.ISVOID: ISVOID,  // isvoid
	lexer.NEG:    NEG,     // ~
	lexer.AT:     AT,      // @
	lexer.DOT:    DOT,     // .
}

type (
	prefixParseFn func() ast.Expression
	infixParseFn  func(ast.Expression) ast.Expression
)

type Parser struct {
	l              *lexer.Lexer
	curToken       lexer.Token
	peekToken      lexer.Token
	errors         []string
	prefixParseFns map[lexer.TokenType]prefixParseFn
	infixParseFns  map[lexer.TokenType]infixParseFn
}

func New(l *lexer.Lexer) *Parser {
	p := &Parser{
		l:              l,
		errors:         []string{},
		prefixParseFns: make(map[lexer.TokenType]prefixParseFn),
		infixParseFns:  make(map[lexer.TokenType]infixParseFn),
	}

	p.nextToken()
	p.nextToken()

	p.registerPrefix(lexer.INT_CONST, p.parseIntegerExpression)
	p.registerPrefix(lexer.STR_CONST, p.parseStringExpression)
	p.registerPrefix(lexer.BOOL_CONST, p.parseBoolExpression)
	p.registerPrefix(lexer.OBJECTID, p.parseObjectIdentifier)
	p.registerPrefix(lexer.LPAREN, p.parseGroupedExpression)
	p.registerPrefix(lexer.IF, p.parseIfExpression)
	p.registerPrefix(lexer.WHILE, p.parseWhileExpression)
	p.registerPrefix(lexer.LET, p.parseLetExpression)
	p.registerPrefix(lexer.CASE, p.parseCaseExpression)
	p.registerPrefix(lexer.NEW, p.parseNewExpression)
	p.registerPrefix(lexer.ISVOID, p.parseIsvoidExpression)
	p.registerPrefix(lexer.NOT, p.parseNotExpression)
	p.registerPrefix(lexer.NEG, p.parseNEGExpression)
	p.registerPrefix(lexer.LBRACE, p.parseBlockExpression)

	p.registerInfix(lexer.PLUS, p.parseInfixExpression)
	p.registerInfix(lexer.MINUS, p.parseInfixExpression)
	p.registerInfix(lexer.TIMES, p.parseInfixExpression)
	p.registerInfix(lexer.DIVIDE, p.parseInfixExpression)
	p.registerInfix(lexer.LT, p.parseInfixExpression)
	p.registerInfix(lexer.LE, p.parseInfixExpression)
	p.registerInfix(lexer.EQ, p.parseInfixExpression)
	p.registerInfix(lexer.ASSIGN, p.parseAssignment)

	// Register method call parsing for the dot operator
	p.registerInfix(lexer.DOT, p.parseMethodCall)

	// Register TYPEID and OBJECTID for function calls
	p.registerPrefix(lexer.OBJECTID, p.parseObjectIdentifier)

	// Register NEG and NOT operators
	p.registerPrefix(lexer.NEG, p.parseNEGExpression)
	p.registerPrefix(lexer.NOT, p.parseNotExpression)

	return p
}

func (p *Parser) Errors() []string {
	return p.errors
}

func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

func (p *Parser) curTokenIs(t lexer.TokenType) bool {
	return p.curToken.Type == t
}

func (p *Parser) peekTokenIs(t lexer.TokenType) bool {
	return p.peekToken.Type == t
}

func (p *Parser) expectAndPeek(t lexer.TokenType) bool {
	if p.peekTokenIs(t) {
		p.nextToken()
		return true
	}
	p.peekError(t)
	return false
}

func (p *Parser) expectCurrent(t lexer.TokenType) bool {
	if p.curTokenIs(t) {
		p.nextToken()
		return true
	}
	p.currentError(t)
	return false
}

func (p *Parser) peekError(t lexer.TokenType) {
	p.errors = append(p.errors, fmt.Sprintf("Expected next token to be %v, got %v line %d col %d", t, p.peekToken.Type, p.peekToken.Line, p.peekToken.Column))
}

func (p *Parser) currentError(t lexer.TokenType) {
	p.errors = append(p.errors, fmt.Sprintf("Expected current token to be %v, got %v line %d col %d", t, p.curToken.Type, p.peekToken.Line, p.peekToken.Column))
}

func (p *Parser) ParseProgram() *ast.Program {
	fmt.Println("\n=== Starting ParseProgram ===")
	prog := &ast.Program{}
	prog.Classes = []*ast.Class{}

	// Parse classes until EOF
	for p.curToken.Type != lexer.EOF {
		fmt.Printf("ParseProgram: Current token: %s (Literal: %s)\n", p.curToken.Type, p.curToken.Literal)
		class := p.ParseClass()
		if class == nil {
			fmt.Println("ParseProgram: Failed to parse class")
			break
		}
		prog.Classes = append(prog.Classes, class)
		fmt.Printf("ParseProgram: Successfully parsed class: %s\n", class.Name.Value)

		// After each class, expect a semicolon
		fmt.Printf("ParseProgram: Expecting semicolon, got: %s\n", p.peekToken.Type)
		if !p.expectAndPeek(lexer.SEMI) {
			fmt.Println("ParseProgram: Missing semicolon after class")
			break
		}
		p.nextToken() // Move past semicolon to start of next class (if any)
		fmt.Printf("ParseProgram: Next token after semicolon: %s\n", p.curToken.Type)
	}

	// fmt.Println("=== Completed ParseProgram ===\n")
	return prog
}

func (p *Parser) ParseClass() *ast.Class {
	fmt.Println("\n=== Starting ParseClass ===")
	fmt.Printf("ParseClass: Current token: %s (Literal: %s)\n", p.curToken.Type, p.curToken.Literal)

	// Verify we're at a class token
	if !p.curTokenIs(lexer.CLASS) {
		fmt.Printf("ParseClass: Expected CLASS token, got %s\n", p.curToken.Type)
		p.errors = append(p.errors, fmt.Sprintf("Expected class, got %s", p.curToken.Type))
		return nil
	}

	c := &ast.Class{Token: p.curToken}

	// Get class name
	fmt.Printf("ParseClass: Looking for class name, next token: %s\n", p.peekToken.Type)
	if !p.expectAndPeek(lexer.TYPEID) {
		fmt.Println("ParseClass: Failed to get class name")
		return nil
	}
	c.Name = &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal}
	fmt.Printf("ParseClass: Found class name: %s\n", c.Name.Value)

	// Handle inheritance (optional)
	fmt.Printf("ParseClass: Checking for inheritance, next token: %s\n", p.peekToken.Type)
	if p.peekTokenIs(lexer.INHERITS) {
		fmt.Println("ParseClass: Found inheritance")
		p.nextToken()
		if !p.expectAndPeek(lexer.TYPEID) {
			fmt.Println("ParseClass: Failed to get parent class name")
			return nil
		}
		c.Parent = &ast.TypeIdentifier{Token: p.curToken, Value: p.curToken.Literal}
		fmt.Printf("ParseClass: Parent class is: %s\n", c.Parent.Value)
	} else {
		c.Parent = &ast.TypeIdentifier{
			Token: lexer.Token{Type: lexer.TYPEID, Literal: "Object"},
			Value: "Object",
		}
		fmt.Println("ParseClass: No inheritance specified, defaulting to Object")
	}

	// Expect and consume opening brace
	fmt.Printf("ParseClass: Looking for opening brace, next token: %s\n", p.peekToken.Type)
	if !p.expectAndPeek(lexer.LBRACE) {
		fmt.Println("ParseClass: Missing opening brace")
		return nil
	}

	c.Features = []ast.Feature{}

	// Parse features until closing brace
	p.nextToken() // Move to first feature or closing brace
	fmt.Printf("ParseClass: Starting feature parsing, current token: %s\n", p.curToken.Type)

	for !p.curTokenIs(lexer.RBRACE) && !p.curTokenIs(lexer.EOF) {
		fmt.Printf("ParseClass: Attempting to parse feature, current token: %s\n", p.curToken.Type)
		feature := p.parseFeature()
		if feature == nil {
			fmt.Println("ParseClass: Failed to parse feature")
			return nil
		}
		c.Features = append(c.Features, feature)
		fmt.Printf("ParseClass: Successfully parsed feature of type: %T\n", feature)

		// Each feature must end with a semicolon
		fmt.Printf("ParseClass: Looking for semicolon after feature, next token: %s\n", p.peekToken.Type)
		if !p.expectAndPeek(lexer.SEMI) {
			fmt.Println("ParseClass: Missing semicolon after feature")
			return nil
		}
		p.nextToken() // Move to next feature or closing brace
		fmt.Printf("ParseClass: Next token after feature semicolon: %s\n", p.curToken.Type)
	}

	// Verify we found the closing brace
	if !p.curTokenIs(lexer.RBRACE) {
		fmt.Println("ParseClass: Missing closing brace")
		p.errors = append(p.errors, "Expected closing brace")
		return nil
	}

	// fmt.Println("=== Completed ParseClass Successfully ===\n")
	return c
}

// Add error recovery helper
func (p *Parser) skipUntil(tokens ...lexer.TokenType) {
	fmt.Println("error recovery")
	fmt.Println(p.curToken.Type)
	for !p.curTokenIs(lexer.EOF) {
		for _, t := range tokens {
			if p.curTokenIs(t) {
				return
			}
		}
		p.nextToken()
	}
}

func (p *Parser) parseFeature() ast.Feature {
	fmt.Println("\n=== Starting parseFeature ===")
	fmt.Printf("parseFeature: Current token: %s (Literal: %s)\n", p.curToken.Type, p.curToken.Literal)

	// Check if it's a method (has parentheses) or attribute
	if p.peekTokenIs(lexer.LPAREN) {
		fmt.Println("parseFeature: Detected method declaration")
		feature := p.parseMethod()
		if feature != nil {
			fmt.Printf("parseFeature: Successfully parsed method: %s\n", feature.Name.Value)
		} else {
			fmt.Println("parseFeature: Failed to parse method")
		}
		fmt.Printf("parseFeature: Current token after method: %s\n", p.curToken.Type)
		fmt.Printf("parseFeature: Next token after method: %s\n", p.peekToken.Type)
		return feature
	}

	fmt.Println("parseFeature: Detected attribute declaration")
	feature := p.parseAttribute()
	if feature != nil {
		fmt.Printf("parseFeature: Successfully parsed attribute: %s\n", feature.Name.Value)
	} else {
		fmt.Println("parseFeature: Failed to parse attribute")
	}
	fmt.Printf("parseFeature: Current token after attribute: %s\n", p.curToken.Type)
	fmt.Printf("parseFeature: Next token after attribute: %s\n", p.peekToken.Type)
	// fmt.Println("=== Completed parseFeature ===\n")
	return feature
}

func (p *Parser) parseMethod() *ast.Method {
	fmt.Println("\n=== Starting parseMethod ===")
	m := &ast.Method{Token: p.curToken}

	// Parse method name
	if !p.curTokenIs(lexer.OBJECTID) {
		p.errors = append(p.errors, fmt.Sprintf("Expected method name to be OBJECTID, got %s", p.curToken.Type))
		fmt.Println("parseMethod: Failed - expected OBJECTID for method name")
		return nil
	}
	m.Name = &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal}
	fmt.Printf("parseMethod: Found method name: %s\n", m.Name.Value)

	// Parse parameters
	if !p.expectAndPeek(lexer.LPAREN) {
		p.errors = append(p.errors, fmt.Sprintf("Expected '(' after method name, got %s at line %d, col %d",
			p.curToken.Type, p.curToken.Line, p.curToken.Column))
		fmt.Println("parseMethod: Failed - expected left parenthesis")
		return nil
	}
	p.nextToken() // Move past the left paren
	fmt.Printf("parseMethod: After LPAREN, current token: %s\n", p.curToken.Type)

	// Parse formal parameters
	m.Parameters = []*ast.Formal{} // Initialize empty slice

	// First check if we have an empty parameter list
	if p.curTokenIs(lexer.RPAREN) {
		fmt.Println("parseMethod: Empty parameter list")
	} else {
		for {
			fmt.Printf("parseMethod: Parsing formal parameter, current token: %s\n", p.curToken.Type)

			formal := p.parseFormal()
			if formal == nil {
				fmt.Println("parseMethod: Failed to parse formal parameter")
				return nil
			}
			m.Parameters = append(m.Parameters, formal)
			fmt.Printf("parseMethod: Successfully parsed parameter: %s\n", formal.Name.Value)

			// Check for comma or right paren
			if p.peekTokenIs(lexer.RPAREN) {
				fmt.Println("parseMethod: Found right parenthesis, ending parameter list")
				break
			}

			// Must be a comma if not right paren
			if !p.expectAndPeek(lexer.COMMA) {
				p.errors = append(p.errors, fmt.Sprintf("Expected ',' or ')' in parameter list, got %s at line %d, col %d",
					p.peekToken.Type, p.peekToken.Line, p.peekToken.Column))
				fmt.Println("parseMethod: Failed - expected comma between parameters")
				return nil
			}
			p.nextToken() // move to next parameter
			fmt.Printf("parseMethod: After comma, current token: %s\n", p.curToken.Type)
		}
		if !p.expectAndPeek(lexer.RPAREN) {
			fmt.Println("parseMethod: Failed - missing closing parenthesis")
			return nil
		}
	}

	fmt.Printf("parseMethod: After RPAREN, current token: %s\n", p.curToken.Type)

	// Parse return type
	fmt.Println("parseMethod: Parsing return type")
	if !p.expectAndPeek(lexer.COLON) {
		fmt.Println("parseMethod: Failed - expected colon before return type")
		return nil
	}

	if !p.expectAndPeek(lexer.TYPEID) {
		fmt.Println("parseMethod: Failed - expected return type")
		return nil
	}
	m.ReturnType = &ast.TypeIdentifier{Token: p.curToken, Value: p.curToken.Literal}
	fmt.Printf("parseMethod: Found return type: %s\n", m.ReturnType.Value)

	// Parse method body
	fmt.Println("parseMethod: Parsing method body")
	if !p.expectAndPeek(lexer.LBRACE) {
		fmt.Println("parseMethod: Failed - expected opening brace")
		return nil
	}

	p.nextToken() // Move past LBRACE
	fmt.Printf("parseMethod: About to parse method body, current token: %s\n", p.curToken.Type)

	// Parse the body expression
	m.Body = p.parseExpression(LOWEST)
	if m.Body == nil {
		fmt.Println("parseMethod: Failed to parse method body")
		return nil
	}
	fmt.Printf("parseMethod: Successfully parsed method body of type: %T\n", m.Body)

	// For method bodies, we expect a closing brace without requiring a semicolon
	if !p.expectAndPeek(lexer.RBRACE) {
		fmt.Printf("parseMethod: Expected closing brace for method, got %s\n", p.peekToken.Type)
		return nil
	}
	fmt.Println("parseMethod: Found method closing brace")

	fmt.Printf("parseMethod: Current token after method: %s\n", p.curToken.Type)
	fmt.Printf("parseMethod: Next token after method: %s\n", p.peekToken.Type)
	// fmt.Println("=== Completed parseMethod Successfully ===\n")
	return m
}

func (p *Parser) parseFormal() *ast.Formal {
	fmt.Println("\n=== Starting parseFormal ===")
	f := &ast.Formal{Token: p.curToken}

	// Parse parameter name
	fmt.Printf("parseFormal: Current token (name): %s (Literal: %s)\n", p.curToken.Type, p.curToken.Literal)
	if !p.curTokenIs(lexer.OBJECTID) {
		p.errors = append(p.errors, fmt.Sprintf("Expected parameter name, got %s", p.curToken.Type))
		fmt.Println("parseFormal: Failed - expected OBJECTID for parameter name")
		return nil
	}
	f.Name = &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal}
	fmt.Printf("parseFormal: Found parameter name: %s\n", f.Name.Value)

	// Parse colon
	fmt.Printf("parseFormal: Looking for colon, next token: %s\n", p.peekToken.Type)
	if !p.expectAndPeek(lexer.COLON) {
		fmt.Println("parseFormal: Failed - expected colon")
		return nil
	}

	// Parse type
	p.nextToken() // Move to the type token
	fmt.Printf("parseFormal: Looking for type, current token: %s (Literal: %s)\n", p.curToken.Type, p.curToken.Literal)
	if !p.curTokenIs(lexer.TYPEID) {
		p.errors = append(p.errors, fmt.Sprintf("Expected type name, got %s", p.curToken.Type))
		fmt.Println("parseFormal: Failed - expected TYPEID for parameter type")
		return nil
	}
	f.Type = &ast.TypeIdentifier{Token: p.curToken, Value: p.curToken.Literal}
	fmt.Printf("parseFormal: Found parameter type: %s\n", f.Type.Value)

	// fmt.Println("=== Completed parseFormal Successfully ===\n")
	return f
}

func (p *Parser) parseAttribute() *ast.Attribute {
	fmt.Println("\n=== Starting parseAttribute ===")
	a := &ast.Attribute{Token: p.curToken}

	// Parse name (should be current token - OBJECTID)
	fmt.Printf("parseAttribute: Current token (name): %s (Literal: %s)\n", p.curToken.Type, p.curToken.Literal)
	if !p.curTokenIs(lexer.OBJECTID) {
		p.errors = append(p.errors, fmt.Sprintf("Expected attribute name to be OBJECTID, got %s", p.curToken.Type))
		return nil
	}
	a.Name = &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal}
	fmt.Printf("parseAttribute: Found attribute name: %s\n", a.Name.Value)

	// Parse colon
	fmt.Printf("parseAttribute: Looking for colon, next token: %s\n", p.peekToken.Type)
	if !p.expectAndPeek(lexer.COLON) {
		return nil
	}

	// Parse type
	fmt.Printf("parseAttribute: Looking for type, next token: %s\n", p.peekToken.Type)
	if !p.expectAndPeek(lexer.TYPEID) {
		return nil
	}
	a.Type = &ast.TypeIdentifier{Token: p.curToken, Value: p.curToken.Literal}
	fmt.Printf("parseAttribute: Found type: %s\n", a.Type.Value)

	// Parse initialization if present
	if p.peekTokenIs(lexer.ASSIGN) {
		fmt.Println("parseAttribute: Found initialization arrow")
		p.nextToken() // move to ASSIGN token
		p.nextToken() // move to the expression start
		a.Init = p.parseExpression(LOWEST)
		if a.Init == nil {
			fmt.Println("parseAttribute: Failed to parse initialization expression")
			return nil
		}
		fmt.Println("parseAttribute: Successfully parsed initialization")
	}

	// fmt.Println("=== Completed parseAttribute Successfully ===\n")
	return a
}

// parseExpression implements Pratt parsing to handle operator precedence
// while building the expression AST
func (p *Parser) parseExpression(precedence int) ast.Expression {
	fmt.Printf("\n=== Starting parseExpression with precedence %d ===\n", precedence)
	fmt.Printf("Current token: %s (Literal: %s)\n", p.curToken.Type, p.curToken.Literal)

	prefix := p.prefixParseFns[p.curToken.Type]
	if prefix == nil {
		p.noPrefixParseFnError(p.curToken.Type)
		fmt.Printf("No prefix parse function for %s\n", p.curToken.Type)
		return nil
	}

	leftExp := prefix()
	fmt.Printf("After prefix parse: %T\n", leftExp)

	// If we just parsed a block expression, we should stop here
	if _, isBlock := leftExp.(*ast.BlockExpression); isBlock {
		fmt.Println("parseExpression: Found block expression, stopping further parsing")
		return leftExp
	}

	// Continue parsing infix expressions until we hit a lower precedence
	for precedence < p.peekPrecedence() {
		infix := p.infixParseFns[p.peekToken.Type]
		if infix == nil {
			fmt.Printf("No infix parse function for %s\n", p.peekToken.Type)
			return leftExp
		}

		fmt.Printf("Found infix operator: %s\n", p.peekToken.Type)
		p.nextToken()
		leftExp = infix(leftExp)

		if leftExp == nil {
			fmt.Println("parseExpression: Failed to parse infix expression")
			return nil
		}
	}

	fmt.Printf("=== Completed parseExpression, result type: %T ===\n", leftExp)
	fmt.Printf("Current token after expression: %s\n", p.curToken.Type)
	fmt.Printf("Next token after expression: %s\n", p.peekToken.Type)
	return leftExp
}

// (expr)
func (p *Parser) parseGroupedExpression() ast.Expression {
	fmt.Println("\n=== Starting parseGroupedExpression ===")
	fmt.Printf("parseGroupedExpression: Current token: %s\n", p.curToken.Type)

	p.nextToken() // Move past the opening parenthesis
	fmt.Printf("parseGroupedExpression: After LPAREN, current token: %s\n", p.curToken.Type)

	exp := p.parseExpression(LOWEST)
	if exp == nil {
		fmt.Println("parseGroupedExpression: Failed to parse inner expression")
		return nil
	}
	fmt.Printf("parseGroupedExpression: Parsed inner expression of type: %T\n", exp)

	if !p.expectAndPeek(lexer.RPAREN) {
		fmt.Println("parseGroupedExpression: Missing closing parenthesis")
		return nil
	}
	fmt.Printf("parseGroupedExpression: Found closing parenthesis, next token: %s\n", p.peekToken.Type)

	// After a grouped expression, check for method call
	if p.peekTokenIs(lexer.DOT) || p.peekTokenIs(lexer.AT) {
		fmt.Printf("parseGroupedExpression: Found %s, starting method call\n", p.peekToken.Type)
		p.nextToken() // Move to the dot or @ token
		return p.parseMethodCall(exp)
	}

	fmt.Printf("parseGroupedExpression: Completed, current token: %s\n", p.curToken.Type)
	return exp
}

func (p *Parser) parseInfixExpression(left ast.Expression) ast.Expression {
	exp := &ast.InfixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
		Left:     left,
	}

	precedence := p.curPrecedence()
	p.nextToken()
	exp.Right = p.parseExpression(precedence)

	return exp
}

// parseBlockExpression handles Cool blocks: { expr; expr; ... }
// A block must contain at least one expression
func (p *Parser) parseBlockExpression() ast.Expression {
	fmt.Println("\n=== Starting parseBlockExpression ===")
	be := &ast.BlockExpression{Token: p.curToken}
	be.Expressions = []ast.Expression{}

	if !p.curTokenIs(lexer.LBRACE) {
		fmt.Printf("parseBlockExpression: Expected LBRACE, got %s\n", p.curToken.Type)
		return nil
	}

	p.nextToken() // Move past LBRACE
	fmt.Printf("parseBlockExpression: After LBRACE, current token is: %s\n", p.curToken.Type)

	// Parse expressions until we hit RBRACE
	for !p.curTokenIs(lexer.RBRACE) {
		fmt.Printf("parseBlockExpression: Parsing expression, current token: %s\n", p.curToken.Type)
		expr := p.parseExpression(LOWEST)
		if expr == nil {
			fmt.Printf("parseBlockExpression: Failed to parse expression at token %s\n", p.curToken.Type)
			return nil
		}
		be.Expressions = append(be.Expressions, expr)
		fmt.Printf("parseBlockExpression: Successfully parsed expression of type: %T\n", expr)

		// Must have semicolon between expressions (but not after the last one)
		if p.peekTokenIs(lexer.RBRACE) {
			fmt.Println("parseBlockExpression: Found RBRACE, stopping")
			break
		}

		if !p.expectAndPeek(lexer.SEMI) {
			fmt.Printf("parseBlockExpression: Expected semicolon between expressions, got %s\n", p.peekToken.Type)
			return nil
		}
		p.nextToken() // Move past semicolon
	}

	// Don't advance past the RBRACE - let the caller handle it
	fmt.Printf("parseBlockExpression: Successfully parsed block with %d expressions\n", len(be.Expressions))
	fmt.Printf("parseBlockExpression: Current token: %s (at RBRACE)\n", p.curToken.Type)
	fmt.Printf("parseBlockExpression: Next token: %s\n", p.peekToken.Type)
	// fmt.Println("=== Completed parseBlockExpression Successfully ===\n")
	return be
}

// | if expr then expr else expr fi
func (p *Parser) parseIfExpression() ast.Expression {
	fmt.Println("\n=== Starting parseIfExpression ===")
	ife := &ast.IfExpression{Token: p.curToken}

	// Current token is 'if', move to the start of condition
	p.nextToken()
	fmt.Printf("parseIfExpression: Starting condition parse at token: %s\n", p.curToken.Type)

	// Parse condition
	ife.Condition = p.parseExpression(LOWEST)
	if ife.Condition == nil {
		fmt.Println("Failed to parse if condition")
		return nil
	}

	// Parse 'then'
	if !p.expectAndPeek(lexer.THEN) {
		return nil
	}

	// Move to the consequence expression
	p.nextToken()
	ife.Consequence = p.parseExpression(LOWEST)
	if ife.Consequence == nil {
		fmt.Println("Failed to parse if consequence")
		return nil
	}

	// Parse 'else' if present
	if p.peekTokenIs(lexer.ELSE) {
		p.nextToken() // consume 'else'
		p.nextToken() // move to start of alternative expression
		ife.Alternative = p.parseExpression(LOWEST)
		if ife.Alternative == nil {
			fmt.Println("Failed to parse if alternative")
			return nil
		}
	}

	if !p.expectAndPeek(lexer.FI) {
		return nil
	}

	fmt.Printf("parseIfExpression: Successfully parsed if expression\n")
	return ife
}

// | while expr loop expr pool
func (p *Parser) parseWhileExpression() ast.Expression {
	fmt.Println("\n=== Starting parseWhileExpression ===")
	we := &ast.WhileExpression{Token: p.curToken}

	// Current token is 'while', move to the start of condition
	p.nextToken()
	fmt.Printf("parseWhileExpression: Starting condition parse at token: %s\n", p.curToken.Type)

	// Parse condition
	we.Condition = p.parseExpression(LOWEST)
	if we.Condition == nil {
		fmt.Println("Failed to parse while condition")
		return nil
	}

	// Parse 'loop'
	if !p.expectAndPeek(lexer.LOOP) {
		return nil
	}

	// Move to the body expression
	p.nextToken()
	we.Body = p.parseExpression(LOWEST)
	if we.Body == nil {
		fmt.Println("Failed to parse while body")
		return nil
	}

	// Parse 'pool'
	if !p.expectAndPeek(lexer.POOL) {
		return nil
	}

	fmt.Printf("parseWhileExpression: Successfully parsed while expression\n")
	return we
}

// | let ID : TYPE [ <- expr ] [[,ID : TYPE [ <- expr ]]]∗ in expr
func (p *Parser) parseLetExpression() ast.Expression {
	fmt.Println("\n=== Starting parseLetExpression ===")
	le := &ast.LetExpression{Token: p.curToken}

	// Move past 'let'
	if !p.expectAndPeek(lexer.OBJECTID) {
		fmt.Println("parseLetExpression: Failed to get identifier")
		return nil
	}

	// Parse first binding (required)
	le.Name = &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal}

	if !p.expectAndPeek(lexer.COLON) {
		fmt.Println("parseLetExpression: Failed to get colon")
		return nil
	}

	if !p.expectAndPeek(lexer.TYPEID) {
		fmt.Println("parseLetExpression: Failed to get type")
		return nil
	}
	le.Type = &ast.TypeIdentifier{Token: p.curToken, Value: p.curToken.Literal}

	// Parse optional initialization
	if p.peekTokenIs(lexer.ASSIGN) {
		p.nextToken() // consume <-
		p.nextToken() // move to expression
		le.Init = p.parseExpression(LOWEST)
	}

	// Parse additional bindings
	le.Bindings = []*ast.Binding{}
	for p.peekTokenIs(lexer.COMMA) {
		p.nextToken() // consume comma

		if !p.expectAndPeek(lexer.OBJECTID) {
			fmt.Println("parseLetExpression: Failed to get binding identifier")
			return nil
		}

		binding := &ast.Binding{
			Name: &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal},
		}

		if !p.expectAndPeek(lexer.COLON) {
			fmt.Println("parseLetExpression: Failed to get binding colon")
			return nil
		}

		if !p.expectAndPeek(lexer.TYPEID) {
			fmt.Println("parseLetExpression: Failed to get binding type")
			return nil
		}
		binding.Type = &ast.TypeIdentifier{Token: p.curToken, Value: p.curToken.Literal}

		if p.peekTokenIs(lexer.ASSIGN) {
			p.nextToken() // consume <-
			p.nextToken() // move to expression
			binding.Init = p.parseExpression(LOWEST)
		}

		le.Bindings = append(le.Bindings, binding)
	}

	// Parse 'in' expression
	if !p.expectAndPeek(lexer.IN) {
		fmt.Println("parseLetExpression: Failed to get IN keyword")
		return nil
	}

	p.nextToken() // move past 'in'
	le.Body = p.parseExpression(LOWEST)
	if le.Body == nil {
		fmt.Println("parseLetExpression: Failed to parse body expression")
		return nil
	}

	fmt.Printf("parseLetExpression: Successfully parsed let expression with %d bindings\n", len(le.Bindings))
	return le
}

// | case expr of [[ID : TYPE => expr; ]]+esac
func (p *Parser) parseCaseExpression() ast.Expression {
	fmt.Println("\n=== Starting parseCaseExpression ===")
	ce := &ast.CaseExpression{Token: p.curToken}

	// Move past 'case'
	p.nextToken()

	// Parse the expression being cased on
	fmt.Printf("Parsing case condition, current token: %s\n", p.curToken.Type)
	ce.Expression = p.parseExpression(LOWEST)
	if ce.Expression == nil {
		fmt.Println("Failed to parse case condition")
		return nil
	}

	// Expect 'of' keyword
	if !p.expectAndPeek(lexer.OF) {
		return nil
	}

	ce.Cases = []*ast.Case{}

	// Parse cases until we hit 'esac'
	p.nextToken() // move to first case
	for !p.curTokenIs(lexer.ESAC) && !p.curTokenIs(lexer.EOF) {
		fmt.Printf("Parsing case branch, current token: %s\n", p.curToken.Type)

		caseExpr := p.parseCase()
		if caseExpr == nil {
			fmt.Println("Failed to parse case branch")
			return nil
		}
		ce.Cases = append(ce.Cases, caseExpr)

		// Each case must end with semicolon
		if !p.expectAndPeek(lexer.SEMI) {
			return nil
		}
		p.nextToken() // move to next case or esac
	}

	if !p.curTokenIs(lexer.ESAC) {
		p.errors = append(p.errors, "Expected 'esac' at end of case expression")
		return nil
	}

	// Verify we have at least one case
	if len(ce.Cases) == 0 {
		p.errors = append(p.errors, "Case expression must contain at least one case")
		return nil
	}

	return ce
}

func (p *Parser) parseCase() *ast.Case {
	fmt.Printf("\n=== Starting parseCase, current token: %s ===\n", p.curToken.Type)
	c := &ast.Case{Token: p.curToken}

	// Parse identifier
	if !p.curTokenIs(lexer.OBJECTID) {
		p.errors = append(p.errors, fmt.Sprintf("Expected identifier in case branch, got %s", p.curToken.Type))
		return nil
	}
	c.Name = &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal}

	// Parse type
	if !p.expectAndPeek(lexer.COLON) {
		return nil
	}
	if !p.expectAndPeek(lexer.TYPEID) {
		return nil
	}
	c.Type = &ast.TypeIdentifier{Token: p.curToken, Value: p.curToken.Literal}

	// Parse arrow and expression
	if !p.expectAndPeek(lexer.DARROW) {
		return nil
	}

	p.nextToken() // move past DARROW to expression
	fmt.Printf("Parsing case branch expression, current token: %s\n", p.curToken.Type)
	c.Expression = p.parseExpression(LOWEST)
	if c.Expression == nil {
		fmt.Println("Failed to parse case branch expression")
		return nil
	}

	fmt.Println("=== Completed parseCase Successfully ===")
	return c
}

// | new TYPE
func (p *Parser) parseNewExpression() ast.Expression {
	fmt.Println("\n=== Starting parseNewExpression ===")
	ne := &ast.NewExpression{Token: p.curToken}

	// Move past 'new'
	p.nextToken()

	if !p.curTokenIs(lexer.TYPEID) {
		p.errors = append(p.errors, fmt.Sprintf("expected type identifier, got %s", p.curToken.Type))
		return nil
	}

	ne.Type = &ast.TypeIdentifier{Token: p.curToken, Value: p.curToken.Literal}
	return ne
}

// | isvoid expr
func (p *Parser) parseIsvoidExpression() ast.Expression {
	fmt.Println("\n=== Starting parseIsvoidExpression ===")
	ie := &ast.IsVoidExpression{Token: p.curToken}

	// Move past 'isvoid'
	p.nextToken()

	ie.Expression = p.parseExpression(LOWEST)
	if ie.Expression == nil {
		fmt.Println("Failed to parse isvoid expression")
		return nil
	}

	return ie
}

// | not expr
func (p *Parser) parseNotExpression() ast.Expression {
	fmt.Println("\n=== Starting parseNotExpression ===")
	fmt.Printf("parseNotExpression: Current token: %s\n", p.curToken.Type)

	ne := &ast.NotExpression{Token: p.curToken}

	// Move past the 'not' token
	p.nextToken()
	fmt.Printf("parseNotExpression: After NOT token, current token: %s\n", p.curToken.Type)

	ne.Expression = p.parseExpression(NOT) // Use NOT precedence
	if ne.Expression == nil {
		fmt.Println("parseNotExpression: Failed to parse expression after not")
		return nil
	}

	fmt.Printf("parseNotExpression: Successfully parsed not expression of type: %T\n", ne.Expression)
	return ne
}

// | true | false
func (p *Parser) parseBoolExpression() ast.Expression {
	be := &ast.BooleanLiteral{Token: p.curToken}
	be.Value = p.curToken.Literal == "true"
	return be
}

// | INT
func (p *Parser) parseIntegerExpression() ast.Expression {
	ie := &ast.IntegerLiteral{Token: p.curToken}
	value, err := strconv.Atoi(p.curToken.Literal)
	if err != nil {
		p.errors = append(p.errors, fmt.Sprintf("could not parse integer: %v", p.curToken.Literal))
		return nil
	}
	ie.Value = value
	return ie
}

// | STRING
func (p *Parser) parseStringExpression() ast.Expression {
	se := &ast.StringLiteral{Token: p.curToken}
	se.Value = p.curToken.Literal
	return se
}

// | ID
func (p *Parser) parseObjectIdentifier() ast.Expression {
	fmt.Println("\n=== Starting parseObjectIdentifier ===")
	oi := &ast.ObjectIdentifier{Token: p.curToken}
	oi.Value = p.curToken.Literal

	// Check the next token
	fmt.Printf("parseObjectIdentifier: Current token: %s, next token: %s\n", p.curToken.Type, p.peekToken.Type)

	// Handle static dispatch (@) or method calls (.)
	if p.peekTokenIs(lexer.AT) || p.peekTokenIs(lexer.DOT) {
		fmt.Printf("parseObjectIdentifier: Found %s, parsing method call\n", p.peekToken.Type)
		p.nextToken() // Move to the @ or . token
		return p.parseMethodCall(oi)
	}

	// Handle function calls
	if p.peekTokenIs(lexer.LPAREN) {
		fc := &ast.FunctionCall{
			Token:    p.curToken,
			Function: oi,
		}
		p.nextToken() // Move past the LPAREN
		fc.Arguments = p.parseExpressionList(lexer.RPAREN)
		return fc
	}

	// Otherwise, it's just an identifier
	fmt.Println("parseObjectIdentifier: Simple identifier")
	return oi
}

// | ID(expr, expr, ...)
func (p *Parser) parseMethodCall(object ast.Expression) ast.Expression {
	fmt.Println("\n=== Starting parseMethodCall ===")
	fmt.Printf("parseMethodCall: Current token: %s, next token: %s\n", p.curToken.Type, p.peekToken.Type)

	exp := &ast.MethodCall{
		Token:  p.curToken,
		Object: object,
	}

	// Handle static dispatch with @
	if p.curTokenIs(lexer.AT) {
		fmt.Println("parseMethodCall: Processing @ operator")
		if !p.expectAndPeek(lexer.TYPEID) {
			fmt.Println("parseMethodCall: Failed to get type after @")
			return nil
		}
		exp.Type = &ast.TypeIdentifier{Token: p.curToken, Value: p.curToken.Literal}
		fmt.Printf("parseMethodCall: Found type annotation: %s\n", exp.Type.Value)

		// After type annotation, expect dot
		if !p.expectAndPeek(lexer.DOT) {
			fmt.Println("parseMethodCall: Missing dot after type annotation")
			return nil
		}
	} else if !p.curTokenIs(lexer.DOT) {
		fmt.Printf("parseMethodCall: Expected DOT or AT, got %s\n", p.curToken.Type)
		return nil
	}

	// Parse method name
	if !p.expectAndPeek(lexer.OBJECTID) {
		fmt.Println("parseMethodCall: Failed to get method name")
		return nil
	}
	exp.Method = &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal}
	fmt.Printf("parseMethodCall: Found method name: %s\n", exp.Method.Value)

	// Parse arguments
	if !p.expectAndPeek(lexer.LPAREN) {
		fmt.Println("parseMethodCall: Missing opening parenthesis")
		return nil
	}
	exp.Arguments = p.parseExpressionList(lexer.RPAREN)
	fmt.Printf("parseMethodCall: Parsed %d arguments\n", len(exp.Arguments))

	fmt.Printf("parseMethodCall: Successfully completed, current token: %s\n", p.curToken.Type)
	return exp
}

func (p *Parser) parseExpressionList(end lexer.TokenType) []ast.Expression {
	var exps []ast.Expression

	if p.peekTokenIs(end) {
		p.nextToken()
		return exps
	}

	p.nextToken()
	exps = append(exps, p.parseExpression(LOWEST))

	for p.peekTokenIs(lexer.COMMA) {
		p.nextToken()
		p.nextToken()
		exps = append(exps, p.parseExpression(LOWEST))
	}

	if !p.expectAndPeek(end) {
		return nil
	}

	return exps
}

func (p *Parser) parseAssignment(left ast.Expression) ast.Expression {
	fmt.Println("\n=== Starting parseAssignment ===")

	// Verify that left is an ObjectIdentifier
	identifier, ok := left.(*ast.ObjectIdentifier)
	if !ok {
		p.errors = append(p.errors, "Left side of assignment must be an identifier")
		fmt.Println("parseAssignment: Failed - left side is not an identifier")
		return nil
	}

	a := &ast.Assignment{
		Token: p.curToken,
		Name:  identifier,
	}
	fmt.Printf("parseAssignment: Assigning to identifier: %s\n", identifier.Value)

	// Move past the assignment operator
	precedence := p.curPrecedence()
	p.nextToken()

	fmt.Printf("parseAssignment: Parsing right side expression, current token: %s\n", p.curToken.Type)
	a.Expression = p.parseExpression(precedence)
	if a.Expression == nil {
		fmt.Println("parseAssignment: Failed to parse right side expression")
		return nil
	}

	fmt.Printf("parseAssignment: Successfully parsed assignment to %s\n", identifier.Value)
	// fmt.Println("=== Completed parseAssignment Successfully ===\n")
	return a
}

func (p *Parser) parseNEGExpression() ast.Expression {
	fmt.Println("\n=== Starting parseNEGExpression ===")
	fmt.Printf("parseNEGExpression: Current token: %s\n", p.curToken.Type)

	ne := &ast.NegExpression{Token: p.curToken}

	// Move past the ~ token
	p.nextToken()
	fmt.Printf("parseNEGExpression: After NEG token, current token: %s\n", p.curToken.Type)

	ne.Expression = p.parseExpression(NEG) // Use NEG precedence
	if ne.Expression == nil {
		fmt.Println("parseNEGExpression: Failed to parse expression after ~")
		return nil
	}

	fmt.Printf("parseNEGExpression: Successfully parsed negation of type: %T\n", ne.Expression)
	return ne
}

func (p *Parser) peekPrecedence() int {
	if p, ok := precedences[p.peekToken.Type]; ok {
		return p
	}
	return LOWEST
}

func (p *Parser) curPrecedence() int {
	if p, ok := precedences[p.curToken.Type]; ok {
		return p
	}
	return LOWEST
}

func (p *Parser) registerPrefix(tokenType lexer.TokenType, fn prefixParseFn) {
	p.prefixParseFns[tokenType] = fn
}

func (p *Parser) registerInfix(tokenType lexer.TokenType, fn infixParseFn) {
	p.infixParseFns[tokenType] = fn
}

func (p *Parser) noPrefixParseFnError(t lexer.TokenType) {
	msg := fmt.Sprintf("no prefix parse function for %s found at line %d, col %d",
		t, p.curToken.Line, p.curToken.Column)
	fmt.Println("Parser Error:", msg)
	p.errors = append(p.errors, msg)
}

// First, add this function to parse function calls
func (p *Parser) parseFunctionCall() ast.Expression {
	fmt.Println("\n=== Starting parseFunctionCall ===")

	// Create function call expression
	fc := &ast.FunctionCall{
		Token:    p.curToken,
		Function: &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal},
	}

	// Expect opening parenthesis
	if !p.expectAndPeek(lexer.LPAREN) {
		return nil
	}

	// Parse arguments
	fc.Arguments = p.parseExpressionList(lexer.RPAREN)

	fmt.Printf("parseFunctionCall: Successfully parsed function call to %s\n", fc.Function.Value)
	return fc
}
