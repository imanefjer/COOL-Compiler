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
	prog := &ast.Program{}
	prog.Classes = []*ast.Class{}

	// Parse classes until EOF
	for p.curToken.Type != lexer.EOF {
		class := p.ParseClass()
		if class == nil {
			break
		}
		prog.Classes = append(prog.Classes, class)

		// After each class, expect a semicolon
		if !p.expectAndPeek(lexer.SEMI) {
			break
		}
		p.nextToken() // Move past semicolon to start of next class (if any)
	}

	return prog
}

func (p *Parser) ParseClass() *ast.Class {
	
	// Verify we're at a class token
	if !p.curTokenIs(lexer.CLASS) {
		p.errors = append(p.errors, fmt.Sprintf("Expected class, got %s", p.curToken.Type))
		return nil
	}

	c := &ast.Class{Token: p.curToken}

	// Get class name
	if !p.expectAndPeek(lexer.TYPEID) {
		return nil
	}
	c.Name = &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal}

	// Handle inheritance (optional)
	if p.peekTokenIs(lexer.INHERITS) {
		p.nextToken()
		if !p.expectAndPeek(lexer.TYPEID) {
			return nil
		}
		c.Parent = &ast.TypeIdentifier{Token: p.curToken, Value: p.curToken.Literal}
	} else {
		c.Parent = &ast.TypeIdentifier{
			Token: lexer.Token{Type: lexer.TYPEID, Literal: "Object"},
			Value: "Object",
		}
	}

	// Expect and consume opening brace
	if !p.expectAndPeek(lexer.LBRACE) {
		return nil
	}

	c.Features = []ast.Feature{}

	// Parse features until closing brace
	p.nextToken() // Move to first feature or closing brace

	for !p.curTokenIs(lexer.RBRACE) && !p.curTokenIs(lexer.EOF) {
		feature := p.parseFeature()
		if feature == nil {
			return nil
		}
		c.Features = append(c.Features, feature)

		// Each feature must end with a semicolon
		if !p.expectAndPeek(lexer.SEMI) {
			return nil
		}
		p.nextToken() // Move to next feature or closing brace
	}

	// Verify we found the closing brace
	if !p.curTokenIs(lexer.RBRACE) {
		p.errors = append(p.errors, "Expected closing brace")
		return nil
	}

	return c
}

// Add error recovery helper
func (p *Parser) skipUntil(tokens ...lexer.TokenType) {
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

	// Check if it's a method (has parentheses) or attribute
	if p.peekTokenIs(lexer.LPAREN) {
		feature := p.parseMethod()

		return feature
	}

	feature := p.parseAttribute()
	
	return feature
}

func (p *Parser) parseMethod() *ast.Method {
	m := &ast.Method{Token: p.curToken}

	// Parse method name
	if !p.curTokenIs(lexer.OBJECTID) {
		p.errors = append(p.errors, fmt.Sprintf("Expected method name to be OBJECTID, got %s", p.curToken.Type))
		return nil
	}
	m.Name = &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal}

	// Parse parameters
	if !p.expectAndPeek(lexer.LPAREN) {
		p.errors = append(p.errors, fmt.Sprintf("Expected '(' after method name, got %s at line %d, col %d",
			p.curToken.Type, p.curToken.Line, p.curToken.Column))
		return nil
	}
	p.nextToken() // Move past the left paren

	// Parse formal parameters
	m.Parameters = []*ast.Formal{} // Initialize empty slice

	// First check if we have an empty parameter list
	if !p.curTokenIs(lexer.RPAREN) {
		for {

			formal := p.parseFormal()
			if formal == nil {
				return nil
			}
			m.Parameters = append(m.Parameters, formal)

			// Check for comma or right paren
			if p.peekTokenIs(lexer.RPAREN) {
				break
			}

			// Must be a comma if not right paren
			if !p.expectAndPeek(lexer.COMMA) {
				p.errors = append(p.errors, fmt.Sprintf("Expected ',' or ')' in parameter list, got %s at line %d, col %d",
					p.peekToken.Type, p.peekToken.Line, p.peekToken.Column))
				return nil
			}
			p.nextToken() // move to next parameter
		}
		if !p.expectAndPeek(lexer.RPAREN) {
			return nil
		}
	}

	if !p.expectAndPeek(lexer.COLON) {
		return nil
	}

	if !p.expectAndPeek(lexer.TYPEID) {
		return nil
	}
	m.ReturnType = &ast.TypeIdentifier{Token: p.curToken, Value: p.curToken.Literal}

	// Parse method body
	if !p.expectAndPeek(lexer.LBRACE) {
		return nil
	}

	p.nextToken() // Move past LBRACE

	// Parse the body expression
	m.Body = p.parseExpression(LOWEST)
	if m.Body == nil {
		return nil
	}

	// For method bodies, we expect a closing brace without requiring a semicolon
	if !p.expectAndPeek(lexer.RBRACE) {
		return nil
	}

	return m
}

func (p *Parser) parseFormal() *ast.Formal {
	f := &ast.Formal{Token: p.curToken}

	// Parse parameter name
	if !p.curTokenIs(lexer.OBJECTID) {
		p.errors = append(p.errors, fmt.Sprintf("Expected parameter name, got %s", p.curToken.Type))
		return nil
	}
	f.Name = &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal}

	// Parse colon
	if !p.expectAndPeek(lexer.COLON) {
		return nil
	}

	// Parse type
	p.nextToken() // Move to the type token
	if !p.curTokenIs(lexer.TYPEID) {
		p.errors = append(p.errors, fmt.Sprintf("Expected type name, got %s", p.curToken.Type))
		return nil
	}
	f.Type = &ast.TypeIdentifier{Token: p.curToken, Value: p.curToken.Literal}

	return f
}

func (p *Parser) parseAttribute() *ast.Attribute {
	a := &ast.Attribute{Token: p.curToken}

	// Parse name (should be current token - OBJECTID)
	if !p.curTokenIs(lexer.OBJECTID) {
		p.errors = append(p.errors, fmt.Sprintf("Expected attribute name to be OBJECTID, got %s", p.curToken.Type))
		return nil
	}
	a.Name = &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal}

	// Parse colon
	if !p.expectAndPeek(lexer.COLON) {
		return nil
	}

	// Parse type
	if !p.expectAndPeek(lexer.TYPEID) {
		return nil
	}
	a.Type = &ast.TypeIdentifier{Token: p.curToken, Value: p.curToken.Literal}

	// Parse initialization if present
	if p.peekTokenIs(lexer.ASSIGN) {
		p.nextToken() // move to ASSIGN token
		p.nextToken() // move to the expression start
		a.Init = p.parseExpression(LOWEST)
		if a.Init == nil {
			return nil
		}
	}

	return a
}

// parseExpression implements Pratt parsing to handle operator precedence
// while building the expression AST
func (p *Parser) parseExpression(precedence int) ast.Expression {
	
	prefix := p.prefixParseFns[p.curToken.Type]
	if prefix == nil {
		p.noPrefixParseFnError(p.curToken.Type)
		return nil
	}

	leftExp := prefix()

	// If we just parsed a block expression, we should stop here
	if _, isBlock := leftExp.(*ast.BlockExpression); isBlock {
		return leftExp
	}

	// Continue parsing infix expressions until we hit a lower precedence
	for precedence < p.peekPrecedence() {
		infix := p.infixParseFns[p.peekToken.Type]
		if infix == nil {
			return leftExp
		}

		p.nextToken()
		leftExp = infix(leftExp)

		if leftExp == nil {
			return nil
		}
	}

	
	return leftExp
}

// (expr)
func (p *Parser) parseGroupedExpression() ast.Expression {
	
	p.nextToken() // Move past the opening parenthesis

	exp := p.parseExpression(LOWEST)
	if exp == nil {
		return nil
	}

	if !p.expectAndPeek(lexer.RPAREN) {
		return nil
	}

	// After a grouped expression, check for method call
	if p.peekTokenIs(lexer.DOT) || p.peekTokenIs(lexer.AT) {
		p.nextToken() // Move to the dot or @ token
		return p.parseMethodCall(exp)
	}

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
	be := &ast.BlockExpression{Token: p.curToken}
	be.Expressions = []ast.Expression{}

	if !p.curTokenIs(lexer.LBRACE) {
		return nil
	}

	p.nextToken() // Move past LBRACE

	// Parse expressions until we hit RBRACE
	for !p.curTokenIs(lexer.RBRACE) {
		expr := p.parseExpression(LOWEST)
		if expr == nil {
			return nil
		}
		be.Expressions = append(be.Expressions, expr)

		// Must have semicolon between expressions (but not after the last one)
		if p.peekTokenIs(lexer.RBRACE) {
			break
		}

		if !p.expectAndPeek(lexer.SEMI) {
			return nil
		}
		p.nextToken() // Move past semicolon
	}
	return be
}

// | if expr then expr else expr fi
func (p *Parser) parseIfExpression() ast.Expression {
	ife := &ast.IfExpression{Token: p.curToken}

	// Current token is 'if', move to the start of condition
	p.nextToken()

	// Parse condition
	ife.Condition = p.parseExpression(LOWEST)
	if ife.Condition == nil {
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
		return nil
	}

	// Parse 'else' if present
	if p.peekTokenIs(lexer.ELSE) {
		p.nextToken() // consume 'else'
		p.nextToken() // move to start of alternative expression
		ife.Alternative = p.parseExpression(LOWEST)
		if ife.Alternative == nil {
			return nil
		}
	}

	if !p.expectAndPeek(lexer.FI) {
		return nil
	}
	return ife
}

// | while expr loop expr pool
func (p *Parser) parseWhileExpression() ast.Expression {
	we := &ast.WhileExpression{Token: p.curToken}

	// Current token is 'while', move to the start of condition
	p.nextToken()

	// Parse condition
	we.Condition = p.parseExpression(LOWEST)
	if we.Condition == nil {
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
		return nil
	}

	// Parse 'pool'
	if !p.expectAndPeek(lexer.POOL) {
		return nil
	}
	return we
}

// | let ID : TYPE [ <- expr ] [[,ID : TYPE [ <- expr ]]]∗ in expr
func (p *Parser) parseLetExpression() ast.Expression {
	le := &ast.LetExpression{Token: p.curToken}

	// Move past 'let'
	if !p.expectAndPeek(lexer.OBJECTID) {
		return nil
	}

	// Parse first binding (required)
	le.Name = &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal}

	if !p.expectAndPeek(lexer.COLON) {
		return nil
	}

	if !p.expectAndPeek(lexer.TYPEID) {
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
			return nil
		}

		binding := &ast.Binding{
			Name: &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal},
		}

		if !p.expectAndPeek(lexer.COLON) {
			return nil
		}

		if !p.expectAndPeek(lexer.TYPEID) {
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
		return nil
	}

	p.nextToken() // move past 'in'
	le.Body = p.parseExpression(LOWEST)
	if le.Body == nil {
		return nil
	}

	return le
}

// | case expr of [[ID : TYPE => expr; ]]+esac
func (p *Parser) parseCaseExpression() ast.Expression {
	ce := &ast.CaseExpression{Token: p.curToken}

	// Move past 'case'
	p.nextToken()

	// Parse the expression being cased on
	ce.Expression = p.parseExpression(LOWEST)
	if ce.Expression == nil {
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

		caseExpr := p.parseCase()
		if caseExpr == nil {
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
		return nil
	}

	// Verify we have at least one case
	if len(ce.Cases) == 0 {
		return nil
	}

	return ce
}

func (p *Parser) parseCase() *ast.Case {
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
	c.Expression = p.parseExpression(LOWEST)
	if c.Expression == nil {
		return nil
	}

	return c
}

// | new TYPE
func (p *Parser) parseNewExpression() ast.Expression {
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
	ie := &ast.IsVoidExpression{Token: p.curToken}

	// Move past 'isvoid'
	p.nextToken()

	ie.Expression = p.parseExpression(LOWEST)
	if ie.Expression == nil {
		return nil
	}

	return ie
}

// | not expr
func (p *Parser) parseNotExpression() ast.Expression {

	ne := &ast.NotExpression{Token: p.curToken}

	// Move past the 'not' token
	p.nextToken()

	ne.Expression = p.parseExpression(NOT) // Use NOT precedence
	if ne.Expression == nil {
		return nil
	}

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
	oi := &ast.ObjectIdentifier{Token: p.curToken}
	oi.Value = p.curToken.Literal


	// Handle static dispatch (@) or method calls (.)
	if p.peekTokenIs(lexer.AT) || p.peekTokenIs(lexer.DOT) {
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

	return oi
}

// | ID(expr, expr, ...)
func (p *Parser) parseMethodCall(object ast.Expression) ast.Expression {

	exp := &ast.MethodCall{
		Token:  p.curToken,
		Object: object,
	}

	// Handle static dispatch with @
	if p.curTokenIs(lexer.AT) {
		if !p.expectAndPeek(lexer.TYPEID) {
			return nil
		}
		exp.Type = &ast.TypeIdentifier{Token: p.curToken, Value: p.curToken.Literal}

		// After type annotation, expect dot
		if !p.expectAndPeek(lexer.DOT) {
			return nil
		}
	} else if !p.curTokenIs(lexer.DOT) {
		return nil
	}

	// Parse method name
	if !p.expectAndPeek(lexer.OBJECTID) {
		return nil
	}
	exp.Method = &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal}

	// Parse arguments
	if !p.expectAndPeek(lexer.LPAREN) {
		return nil
	}
	exp.Arguments = p.parseExpressionList(lexer.RPAREN)

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

	// Verify that left is an ObjectIdentifier
	identifier, ok := left.(*ast.ObjectIdentifier)
	if !ok {
		p.errors = append(p.errors, "Left side of assignment must be an identifier")
		return nil
	}

	a := &ast.Assignment{
		Token: p.curToken,
		Name:  identifier,
	}

	// Move past the assignment operator
	precedence := p.curPrecedence()
	p.nextToken()

	a.Expression = p.parseExpression(precedence)
	if a.Expression == nil {
		return nil
	}

	return a
}

func (p *Parser) parseNEGExpression() ast.Expression {
	

	ne := &ast.NegExpression{Token: p.curToken}

	// Move past the ~ token
	p.nextToken()

	ne.Expression = p.parseExpression(NEG) // Use NEG precedence
	if ne.Expression == nil {
		return nil
	}

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
	p.errors = append(p.errors, msg)
}
