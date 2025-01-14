package parser

import (
	"cool-compiler/ast"
	"cool-compiler/lexer"
	"fmt"
	"strconv"
)

type Parser struct {
	l         *lexer.Lexer
	curToken  lexer.Token
	peekToken lexer.Token
	errors    []string
}

func New(l *lexer.Lexer) *Parser {
	p := &Parser{
		l:      l,
		errors: []string{},
	}

	p.nextToken()
	p.nextToken()
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

	c.Name = p.curToken.Literal
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
	p.parseExpression()
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
		p.parseExpression()
	}
	return a
}

func (p *Parser) parseExpression() ast.Expression {

	if p.curTokenIs(lexer.LBRACE) {
		return p.parseBlockExpression()
	}
	if p.curTokenIs(lexer.IF) {
		return p.parseIfExpression()
	}
	if p.curTokenIs(lexer.WHILE) {
		return p.parseWhileExpression()
	}
	if p.curTokenIs(lexer.LET) {
		return p.parseLetExpression()
	}
	if p.curTokenIs(lexer.CASE) {
		return p.parseCaseExpression()
	}
	if p.curTokenIs(lexer.NEW) {
		return p.parseNewExpression()
	}
	if p.curTokenIs(lexer.ISVOID) {
		return p.parseIsvoidExpression()
	}
	if p.curTokenIs(lexer.NOT) {
		return p.parseNotExpression()
	}
	if p.curTokenIs(lexer.BOOL_CONST) {
		return p.parseBoolExpression()
	}
	if p.curTokenIs(lexer.INT_CONST) {
		return p.parseIntegerExpression()
	}
	if p.curTokenIs(lexer.STR_CONST) {
		return p.parseStringExpression()
	}
	if p.curTokenIs(lexer.OBJECTID) {
		if p.peekTokenIs(lexer.LPAREN) {
			return p.parseMethodCall()
		} else if p.peekTokenIs(lexer.ASSIGN) {
			return p.parseAssignment()
		} else {
			return p.parseObjectIdentifier()
		}
	}
	if p.curTokenIs(lexer.LPAREN) {
		return p.parseExpression()
	}
	if p.curTokenIs(lexer.NEG) {
		return p.parseNEGExpression()
	}
	if p.curTokenIs(lexer.LT) {
		return p.parseLTExpression()
	}
	if p.curTokenIs(lexer.LE) {
		return p.parseLEExpression()
	}
	if p.curTokenIs(lexer.EQ) {
		return p.parseEQExpression()
	}
	if p.curTokenIs(lexer.PLUS) {
		return p.parsePlusExpression()
	}
	if p.curTokenIs(lexer.MINUS) {
		return p.parseMinusExpression()
	}
	if p.curTokenIs(lexer.TIMES) {
		return p.parseTimesExpression()
	}
	if p.curTokenIs(lexer.DIVIDE) {
		return p.parseDivideExpression()
	}
	return nil
}

func (p *Parser) parseBlockExpression() *ast.BlockExpression {
	be := &ast.BlockExpression{Token: p.curToken}
	if !p.expectCurrent(lexer.LBRACE) {
		return nil
	}
	for !p.peekTokenIs(lexer.RBRACE) {
		p.nextToken()
		be.Expressions = append(be.Expressions, p.parseExpression())
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
	ife.Condition = p.parseExpression()
	if !p.expectAndPeek(lexer.THEN) {
		return nil
	}
	p.nextToken()
	ife.Consequence = p.parseExpression()
	if p.peekTokenIs(lexer.ELSE) {
		p.nextToken()
		ife.Alternative = p.parseExpression()
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
	we.Condition = p.parseExpression()
	if !p.expectAndPeek(lexer.LOOP) {
		return nil
	}
	p.nextToken()
	we.Body = p.parseExpression()
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
		le.Init = p.parseExpression()
	}

	for !p.peekTokenIs(lexer.IN) {
		if !p.expectAndPeek(lexer.COMMA) {
			break
		}
		p.nextToken()
		le.Formals = append(le.Formals, p.parseFromal())
		if p.peekTokenIs(lexer.ASSIGN) {
			p.nextToken()
			p.parseExpression()
		}
	}
	if !p.expectAndPeek(lexer.IN) {
		return nil
	}
	p.nextToken()
	le.Body = p.parseExpression()
	return le
}

func (p *Parser) parseCaseExpression() *ast.CaseExpression {
	if !p.expectCurrent(lexer.CASE) {
		return nil
	}
	ce := &ast.CaseExpression{Token: p.curToken}
	p.nextToken()
	ce.Expression = p.parseExpression()
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
	c.Expression = p.parseExpression()
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

func (p *Parser) parseIsvoidExpression() *ast.IsvoidExpression {
	ie := &ast.IsvoidExpression{Token: p.curToken}
	if !p.expectCurrent(lexer.ISVOID) {
		return nil
	}
	p.nextToken()
	ie.Expression = p.parseExpression()
	return ie
}

func (p *Parser) parseNotExpression() *ast.NotExpression {
	ne := &ast.NotExpression{Token: p.curToken}
	if !p.expectCurrent(lexer.NOT) {
		return nil
	}
	p.nextToken()
	ne.Expression = p.parseExpression()
	return ne
}

func (p *Parser) parseBoolExpression() *ast.BoolExpression {
	be := &ast.BoolExpression{Token: p.curToken}
	be.Value = p.curToken.Literal == "true"
	return be
}

func (p *Parser) parseIntegerExpression() *ast.IntExpression {
	ie := &ast.IntExpression{Token: p.curToken}
	value, err := strconv.Atoi(p.curToken.Literal)
	if err != nil {
		p.errors = append(p.errors, fmt.Sprintf("could not parse integer: %v", p.curToken.Literal))
		return nil
	}
	ie.Value = value
	return ie
}

func (p *Parser) parseStringExpression() *ast.StringExpression {
	se := &ast.StringExpression{Token: p.curToken}
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
		mc.Arguments = append(mc.Arguments, p.parseExpression())
		if !p.expectAndPeek(lexer.COMMA) {
			break
		}
	}
	if !p.expectAndPeek(lexer.RPAREN) {
		return nil
	}
	return mc
}

func (p *Parser) parseAssignment() *ast.Assignment {
	a := &ast.Assignment{Token: p.curToken}
	a.Name = &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal}
	if !p.expectAndPeek(lexer.ASSIGN) {
		return nil
	}
	p.nextToken()
	a.Expression = p.parseExpression()
	return a
}

func (p *Parser) parseNEGExpression() *ast.NegExpression {
	ne := &ast.NegExpression{Token: p.curToken}
	if !p.expectCurrent(lexer.NEG) {
		return nil
	}
	p.nextToken()
	ne.Expression = p.parseExpression()
	return ne
}

func (p *Parser) parseLTExpression() *ast.LTExpression {
	lt := &ast.LTExpression{Token: p.curToken}
	if !p.expectCurrent(lexer.LT) {
		return nil
	}
	p.nextToken()
	lt.Left = p.parseExpression()
	p.nextToken()
	lt.Right = p.parseExpression()
	return lt
}

func (p *Parser) parseLEExpression() *ast.LEExpression {
	le := &ast.LEExpression{Token: p.curToken}
	if !p.expectCurrent(lexer.LE) {
		return nil
	}
	p.nextToken()
	le.Left = p.parseExpression()
	p.nextToken()
	le.Right = p.parseExpression()
	return le
}

func (p *Parser) parseEQExpression() *ast.EQExpression {
	eq := &ast.EQExpression{Token: p.curToken}
	if !p.expectCurrent(lexer.EQ) {
		return nil
	}
	p.nextToken()
	eq.Left = p.parseExpression()
	p.nextToken()
	eq.Right = p.parseExpression()
	return eq
}

func (p *Parser) parsePlusExpression() *ast.PlusExpression {
	pe := &ast.PlusExpression{Token: p.curToken}
	if !p.expectCurrent(lexer.PLUS) {
		return nil
	}
	p.nextToken()
	pe.Left = p.parseExpression()
	p.nextToken()
	pe.Right = p.parseExpression()
	return pe
}

func (p *Parser) parseMinusExpression() *ast.MinusExpression {
	me := &ast.MinusExpression{Token: p.curToken}
	if !p.expectCurrent(lexer.MINUS) {
		return nil
	}
	p.nextToken()
	me.Left = p.parseExpression()
	p.nextToken()
	me.Right = p.parseExpression()
	return me
}

func (p *Parser) parseTimesExpression() *ast.TimesExpression {
	te := &ast.TimesExpression{Token: p.curToken}
	if !p.expectCurrent(lexer.TIMES) {
		return nil
	}
	p.nextToken()
	te.Left = p.parseExpression()
	p.nextToken()
	te.Right = p.parseExpression()
	return te
}

func (p *Parser) parseDivideExpression() *ast.DivideExpression {
	de := &ast.DivideExpression{Token: p.curToken}
	if !p.expectCurrent(lexer.DIVIDE) {
		return nil
	}
	p.nextToken()
	de.Left = p.parseExpression()
	p.nextToken()
	de.Right = p.parseExpression()
	return de
}

