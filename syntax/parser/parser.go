package parser

import (
	"cool-compiler/ast"
	"cool-compiler/lexer"
	"fmt"
	"strconv"
)

// TODO: respect the convention
const (
	_ int = iota
	LOWEST
	ASSIGN      // <-
	EQUALS      // =
	LESSGREATER // <=, <
	SUM         // +, -
	PRODUCT     // *, /
	PREFIX      // -X or !X
	CALL        // myFunction(X)
)

var precedences = map[lexer.TokenType]int{
	lexer.ASSIGN: ASSIGN,
	lexer.EQ:     EQUALS,
	lexer.LE:     LESSGREATER,
	lexer.LT:     LESSGREATER,
	lexer.PLUS:   SUM,
	lexer.MINUS:  SUM,
	lexer.TIMES:  PRODUCT,
	lexer.DIVIDE: PRODUCT,
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

	p.registerInfix(lexer.PLUS, p.parseInfixExpression)
	p.registerInfix(lexer.MINUS, p.parseInfixExpression)
	p.registerInfix(lexer.TIMES, p.parseInfixExpression)
	p.registerInfix(lexer.DIVIDE, p.parseInfixExpression)
	p.registerInfix(lexer.LT, p.parseInfixExpression)
	p.registerInfix(lexer.LE, p.parseInfixExpression)
	p.registerInfix(lexer.EQ, p.parseInfixExpression)
	p.registerInfix(lexer.ASSIGN, p.parseAssignment)

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
	for p.curToken.Type != lexer.EOF && p.curToken.Type != lexer.ERROR {
		c := p.ParseClass()

		if !p.expectAndPeek(lexer.SEMI) {
			continue
		}
		prog.Classes = append(prog.Classes, c)
	}
	return prog
}

func (p *Parser) ParseClass() *ast.Class {
	c := &ast.Class{Token: p.curToken}
	if !p.expectCurrent(lexer.CLASS) {
		return nil
	}

	if !p.curTokenIs(lexer.TYPEID) {
		// Add errors
		return nil
	}
	c.Name = &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal}
	if !p.expectAndPeek(lexer.LBRACE) {
		return nil
	}

	for !p.peekTokenIs(lexer.RBRACE) {
		p.nextToken()
		c.Features = append(c.Features, p.parseFeature())
		if !p.expectAndPeek(lexer.SEMI) {
			return nil
		}
	}

	if !p.expectAndPeek(lexer.RBRACE) {
		return nil
	}

	return c
}

func (p *Parser) parseFeature() ast.Feature {
	if p.peekTokenIs(lexer.LPAREN) {
		return p.parseMethod()
	}
	return p.parseAttribute()
}

func (p *Parser) parseMethod() *ast.Method {
	m := &ast.Method{Token: p.curToken}
	if !p.expectCurrent(lexer.OBJECTID) {
		return nil
	}
	m.Name = &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal}
	if !p.expectAndPeek(lexer.LPAREN) {
		return nil
	}
	for !p.peekTokenIs(lexer.RPAREN) {
		p.nextToken()
		p.parseFromal()
		if !p.expectAndPeek(lexer.COMMA) {
			break
		}
	}
	if !p.expectAndPeek(lexer.RPAREN) {
		return nil
	}
	if !p.expectAndPeek(lexer.COLON) {
		return nil
	}
	if !p.expectCurrent(lexer.TYPEID) {
		return nil
	}
	m.Type = &ast.TypeIdentifier{Token: p.curToken, Value: p.curToken.Literal}
	if !p.expectAndPeek(lexer.LBRACE) {
		return nil
	}
	p.parseExpression(LOWEST)
	if !p.expectAndPeek(lexer.RBRACE) {
		return nil
	}

	return m
}

func (p *Parser) parseFromal() *ast.Formal {
	f := &ast.Formal{Token: p.curToken}
	if !p.expectCurrent(lexer.OBJECTID) {
		return nil
	}
	f.Name = &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal}
	if !p.expectAndPeek(lexer.COLON) {
		return nil
	}
	if !p.expectCurrent(lexer.TYPEID) {
		return nil
	}
	f.Type = &ast.TypeIdentifier{Token: p.curToken, Value: p.curToken.Literal}
	return f
}

func (p *Parser) parseAttribute() *ast.Attribute {
	a := &ast.Attribute{Token: p.curToken}
	if !p.expectCurrent(lexer.OBJECTID) {
		return nil
	}
	a.Name = &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal}
	if !p.expectAndPeek(lexer.COLON) {
		return nil
	}
	if !p.expectCurrent(lexer.TYPEID) {
		return nil
	}
	a.Type = &ast.TypeIdentifier{Token: p.curToken, Value: p.curToken.Literal}
	if p.peekTokenIs(lexer.ASSIGN) {
		p.nextToken()
		p.parseExpression(LOWEST)
	}
	return a
}

func (p *Parser) parseExpression(precedence int) ast.Expression {
	prefix := p.prefixParseFns[p.curToken.Type]
	if prefix == nil {
		p.noPrefixParseFnError(p.curToken.Type)
		return nil
	}
	leftExp := prefix()

	for !p.peekTokenIs(lexer.SEMI) && precedence < p.peekPrecedence() {
		infix := p.infixParseFns[p.peekToken.Type]
		if infix == nil {
			return leftExp
		}

		p.nextToken()

		leftExp = infix(leftExp)
	}

	return leftExp
}

func (p *Parser) parseGroupedExpression() ast.Expression {
	p.nextToken()
	exp := p.parseExpression(LOWEST)
	if !p.expectAndPeek(lexer.RPAREN) {
		return nil
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

func (p *Parser) parseBlockExpression() *ast.BlockExpression {
	be := &ast.BlockExpression{Token: p.curToken}
	if !p.expectCurrent(lexer.LBRACE) {
		return nil
	}
	for !p.peekTokenIs(lexer.RBRACE) {
		p.nextToken()
		be.Expressions = append(be.Expressions, p.parseExpression(LOWEST))
		if !p.expectAndPeek(lexer.SEMI) {
			break
		}
	}
	if !p.expectAndPeek(lexer.RBRACE) {
		return nil
	}
	return be
}

func (p *Parser) parseIfExpression() *ast.IfExpression {
	ife := &ast.IfExpression{Token: p.curToken}
	if !p.expectCurrent(lexer.IF) {
		return nil
	}
	p.nextToken()
	ife.Condition = p.parseExpression(LOWEST)
	if !p.expectAndPeek(lexer.THEN) {
		return nil
	}
	p.nextToken()
	ife.Consequence = p.parseExpression(LOWEST)
	if p.peekTokenIs(lexer.ELSE) {
		p.nextToken()
		ife.Alternative = p.parseExpression(LOWEST)
	}
	if !p.expectAndPeek(lexer.FI) {
		return nil
	}
	return ife
}

func (p *Parser) parseWhileExpression() *ast.WhileExpression {
	we := &ast.WhileExpression{Token: p.curToken}
	if !p.expectCurrent(lexer.WHILE) {
		return nil
	}
	p.nextToken()
	we.Condition = p.parseExpression(LOWEST)
	if !p.expectAndPeek(lexer.LOOP) {
		return nil
	}
	p.nextToken()
	we.Body = p.parseExpression(LOWEST)
	if !p.expectAndPeek(lexer.POOL) {
		return nil
	}
	return we
}

func (p *Parser) parseLetExpression() *ast.LetExpression {
	le := &ast.LetExpression{Token: p.curToken}
	if !p.expectCurrent(lexer.LET) {
		return nil
	}
	if !p.expectCurrent(lexer.OBJECTID) {
		return nil
	}
	le.Name = &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal}
	if !p.expectAndPeek(lexer.COLON) {
		return nil
	}
	if !p.expectCurrent(lexer.TYPEID) {
		return nil
	}
	le.Type = &ast.TypeIdentifier{Token: p.curToken, Value: p.curToken.Literal}
	if p.peekTokenIs(lexer.ASSIGN) {
		p.nextToken()
		le.Init = p.parseExpression(LOWEST)
	}

	for !p.peekTokenIs(lexer.IN) {
		if !p.expectAndPeek(lexer.COMMA) {
			break
		}
		p.nextToken()
		le.Formals = append(le.Formals, p.parseFromal())
		if p.peekTokenIs(lexer.ASSIGN) {
			p.nextToken()
			p.parseExpression(LOWEST)
		}
	}
	if !p.expectAndPeek(lexer.IN) {
		return nil
	}
	p.nextToken()
	le.Body = p.parseExpression(LOWEST)
	return le
}

func (p *Parser) parseCaseExpression() *ast.CaseExpression {
	if !p.expectCurrent(lexer.CASE) {
		return nil
	}
	ce := &ast.CaseExpression{Token: p.curToken}
	p.nextToken()
	ce.Expression = p.parseExpression(LOWEST)
	if !p.expectAndPeek(lexer.OF) {
		return nil
	}
	if !p.expectCurrent(lexer.OBJECTID) {
		return nil
	}

	ce.Cases = append(ce.Cases, p.parseCase())
	if !p.expectAndPeek(lexer.SEMI) {
		return nil
	}
	for !p.peekTokenIs(lexer.ESAC) {
		if !p.expectAndPeek(lexer.SEMI) {
			break
		}
		p.nextToken()
		ce.Cases = append(ce.Cases, p.parseCase())
	}
	if !p.expectAndPeek(lexer.ESAC) {
		return nil
	}
	return ce
}

func (p *Parser) parseCase() *ast.Case {
	c := &ast.Case{Token: p.curToken}
	if !p.expectCurrent(lexer.OBJECTID) {
		return nil
	}
	c.Name = &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal}
	if !p.expectAndPeek(lexer.COLON) {
		return nil
	}
	if !p.expectCurrent(lexer.TYPEID) {
		return nil
	}
	c.Type = &ast.TypeIdentifier{Token: p.curToken, Value: p.curToken.Literal}
	if !p.expectAndPeek(lexer.DARROW) {
		return nil
	}
	p.nextToken()
	c.Expression = p.parseExpression(LOWEST)
	return c
}

func (p *Parser) parseNewExpression() *ast.NewExpression {
	ne := &ast.NewExpression{Token: p.curToken}
	if !p.expectCurrent(lexer.NEW) {
		return nil
	}
	if !p.expectCurrent(lexer.TYPEID) {
		return nil
	}
	ne.Type = &ast.TypeIdentifier{Token: p.curToken, Value: p.curToken.Literal}
	return ne
}

func (p *Parser) parseIsvoidExpression() *ast.IsVoidExpression {
	ie := &ast.IsVoidExpression{Token: p.curToken}
	if !p.expectCurrent(lexer.ISVOID) {
		return nil
	}
	p.nextToken()
	ie.Expression = p.parseExpression(LOWEST)
	return ie
}

func (p *Parser) parseNotExpression() *ast.NotExpression {
	ne := &ast.NotExpression{Token: p.curToken}
	if !p.expectCurrent(lexer.NOT) {
		return nil
	}
	p.nextToken()
	ne.Expression = p.parseExpression(LOWEST)
	return ne
}

func (p *Parser) parseBoolExpression() *ast.BooleanLiteral {
	be := &ast.BooleanLiteral{Token: p.curToken}
	be.Value = p.curToken.Literal == "true"
	return be
}

func (p *Parser) parseIntegerExpression() *ast.IntegerLiteral {
	ie := &ast.IntegerLiteral{Token: p.curToken}
	value, err := strconv.Atoi(p.curToken.Literal)
	if err != nil {
		p.errors = append(p.errors, fmt.Sprintf("could not parse integer: %v", p.curToken.Literal))
		return nil
	}
	ie.Value = value
	return ie
}

func (p *Parser) parseStringExpression() *ast.StringLiteral {
	se := &ast.StringLiteral{Token: p.curToken}
	se.Value = p.curToken.Literal
	return se
}

func (p *Parser) parseObjectIdentifier() *ast.ObjectIdentifier {
	oi := &ast.ObjectIdentifier{Token: p.curToken}
	oi.Value = p.curToken.Literal
	return oi
}

func (p *Parser) parseMethodCall() *ast.MethodCall {
	mc := &ast.MethodCall{Token: p.curToken}
	mc.Object = &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal}
	if !p.expectAndPeek(lexer.LPAREN) {
		return nil
	}
	for !p.peekTokenIs(lexer.RPAREN) {
		p.nextToken()
		mc.Arguments = append(mc.Arguments, p.parseExpression(LOWEST))
		if !p.expectAndPeek(lexer.COMMA) {
			break
		}
	}
	if !p.expectAndPeek(lexer.RPAREN) {
		return nil
	}
	return mc
}

func (p *Parser) parseAssignment(left ast.Expression) ast.Expression {
	a := &ast.Assignment{Token: p.curToken}
	a.Name = &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal}
	if !p.expectAndPeek(lexer.ASSIGN) {
		return nil
	}
	p.nextToken()
	a.Expression = p.parseExpression(LOWEST)
	return a
}

func (p *Parser) parseNEGExpression() *ast.NegExpression {
	ne := &ast.NegExpression{Token: p.curToken}
	if !p.expectCurrent(lexer.NEG) {
		return nil
	}
	p.nextToken()
	ne.Expression = p.parseExpression(LOWEST)
	return ne
}

func (p *Parser) parseLTExpression(left ast.Expression) ast.Expression {
	lt := &ast.LTExpression{Token: p.curToken}
	if !p.expectCurrent(lexer.LT) {
		return nil
	}
	p.nextToken()
	lt.Left = left
	lt.Right = p.parseExpression(LOWEST)
	return lt
}

func (p *Parser) parseLEExpression(left ast.Expression) ast.Expression {
	le := &ast.LEExpression{Token: p.curToken}
	if !p.expectCurrent(lexer.LE) {
		return nil
	}
	p.nextToken()
	le.Left = left
	le.Right = p.parseExpression(LOWEST)
	return le
}

func (p *Parser) parseEQExpression(left ast.Expression) ast.Expression {
	eq := &ast.EQExpression{Token: p.curToken}
	if !p.expectCurrent(lexer.EQ) {
		return nil
	}
	p.nextToken()
	eq.Left = left
	eq.Right = p.parseExpression(LOWEST)
	return eq
}

func (p *Parser) parsePlusExpression(left ast.Expression) ast.Expression {
	pe := &ast.PlusExpression{Token: p.curToken}
	if !p.expectCurrent(lexer.PLUS) {
		return nil
	}
	p.nextToken()
	pe.Left = left
	pe.Right = p.parseExpression(LOWEST)
	return pe
}

func (p *Parser) parseMinusExpression(left ast.Expression) ast.Expression {
	me := &ast.MinusExpression{Token: p.curToken}
	if !p.expectCurrent(lexer.MINUS) {
		return nil
	}
	p.nextToken()
	me.Left = left
	me.Right = p.parseExpression(LOWEST)
	return me
}

func (p *Parser) parseTimesExpression(left ast.Expression) ast.Expression {
	te := &ast.TimesExpression{Token: p.curToken}
	if !p.expectCurrent(lexer.TIMES) {
		return nil
	}
	p.nextToken()
	te.Left = left
	te.Right = p.parseExpression(LOWEST)
	return te
}

func (p *Parser) parseDivideExpression(left ast.Expression) ast.Expression {
	de := &ast.DivideExpression{Token: p.curToken}
	if !p.expectCurrent(lexer.DIVIDE) {
		return nil
	}
	p.nextToken()
	de.Left = left
	de.Right = p.parseExpression(LOWEST)
	return de
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
	p.errors = append(p.errors, fmt.Sprintf("no prefix parse function for %s found", t))
}
