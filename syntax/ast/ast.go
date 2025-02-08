package ast

import (
	"cool-compiler/lexer"
)

type Node interface {
	TokenLiteral() string
}

type Statement interface {
	Node
	statementNode()
}

type Expression interface {
	Node
	expressionNode()
}

type Feature interface {
	Node
	featureNode()
}

type TypeIdentifier struct {
	Token lexer.Token
	Value string
}

func (ti *TypeIdentifier) TokenLiteral() string { return ti.Token.Literal }

type ObjectIdentifier struct {
	Token lexer.Token
	Value string
}

func (oi *ObjectIdentifier) TokenLiteral() string { return oi.Token.Literal }
func (oi *ObjectIdentifier) expressionNode()      {}

type Program struct {
	Classes []*Class
}

func (p *Program) TokenLiteral() string { return "" }

type Class struct {
	Token    lexer.Token
	Name     *ObjectIdentifier
	Features []Feature
	//parent : typeidentifier
}

func (c *Class) TokenLiteral() string { return c.Token.Literal }

type Attribute struct {
	Token lexer.Token
	Name  *ObjectIdentifier
	Type  *TypeIdentifier
}

func (a *Attribute) TokenLiteral() string { return a.Token.Literal }
func (a *Attribute) featureNode()         {}

type Method struct {
	Token lexer.Token
	Name  *ObjectIdentifier
	Type  *TypeIdentifier
}

func (m *Method) TokenLiteral() string { return m.Token.Literal }
func (m *Method) featureNode()         {}

type Formal struct {
	Token lexer.Token
	Name  *ObjectIdentifier
	Type  *TypeIdentifier
}
func (f *Formal) TokenLiteral() string { return f.Token.Literal }
func (f *Formal) featureNode()         {}
// block expression
type BlockExpression struct {
	Token       lexer.Token
	Expressions []Expression
}

func (be *BlockExpression) TokenLiteral() string { return be.Token.Literal }
func (be *BlockExpression) expressionNode()      {}

//if Expression
type IfExpression struct {
	Token       lexer.Token
	Condition   Expression
	Consequence Expression
	Alternative Expression
}

func (ie *IfExpression) TokenLiteral() string { return ie.Token.Literal }
func (ie *IfExpression) expressionNode()      {}

//while Expression
type WhileExpression struct {
	Token       lexer.Token
	Condition   Expression
	Body        Expression
}

func (we *WhileExpression) TokenLiteral() string { return we.Token.Literal }
func (we *WhileExpression) expressionNode()      {}

//let Expression
type LetExpression struct {
	Token    lexer.Token
	Name	 *ObjectIdentifier
	Type	 *TypeIdentifier
	Init	 Expression
	Formals  []*Formal
	Body     Expression
} 

func (le *LetExpression) TokenLiteral() string { return le.Token.Literal }
func (le *LetExpression) expressionNode()      {}

//case Expression
type CaseExpression struct {
	Token       lexer.Token
	Expression  Expression
	Cases 	 []*Case
}
func (ce *CaseExpression) TokenLiteral() string { return ce.Token.Literal }
func (ce *CaseExpression) expressionNode()      {}

type Case struct {
	Token    lexer.Token
	Name     *ObjectIdentifier
	Type     *TypeIdentifier
	Expression Expression
}

func (c *Case) TokenLiteral() string { return c.Token.Literal }
func (c *Case) expressionNode()      {}

//new Expression
type NewExpression struct {
	Token       lexer.Token
	Type        *TypeIdentifier
}

func (ne *NewExpression) TokenLiteral() string { return ne.Token.Literal }
func (ne *NewExpression) expressionNode()      {}

//isvoid Expression
type IsVoidExpression struct {
	Token       lexer.Token
	Expression  Expression
}

func (ie *IsVoidExpression) TokenLiteral() string { return ie.Token.Literal }
func (ie *IsVoidExpression) expressionNode()      {}

//not Expression
type NotExpression struct {
	Token       lexer.Token
	Expression  Expression
}

func (ne *NotExpression) TokenLiteral() string { return ne.Token.Literal }
func (ne *NotExpression) expressionNode()      {}


//bool Expression

type BooleanLiteral struct {
	Token       lexer.Token
	Value       bool
}

func (be *BooleanLiteral) TokenLiteral() string { return be.Token.Literal }
func (be *BooleanLiteral) expressionNode()      {}

//int Expression
type IntegerLiteral struct {
	Token       lexer.Token
	Value       int
}

func (ie *IntegerLiteral) TokenLiteral() string { return ie.Token.Literal }
func (ie *IntegerLiteral) expressionNode()      {}


//string Expression
type StringLiteral struct {
	Token       lexer.Token
	Value      string
}

func (se *StringLiteral) TokenLiteral() string { return se.Token.Literal }
func (se *StringLiteral) expressionNode()      {}

//method call
type MethodCall struct {
	Token       lexer.Token
	Object      Expression
	Method      *ObjectIdentifier
	Arguments   []Expression
}

func (mc *MethodCall) TokenLiteral() string { return mc.Token.Literal }
func (mc *MethodCall) expressionNode()      {}


//assignment
type Assignment struct {
	Token       lexer.Token
	Name        *ObjectIdentifier
	Expression  Expression
}

func (a *Assignment) TokenLiteral() string { return a.Token.Literal }
func (a *Assignment) expressionNode()      {}

//neg Expression
type NegExpression struct {
	Token       lexer.Token
	Expression  Expression
}

func (ne *NegExpression) TokenLiteral() string { return ne.Token.Literal }
func (ne *NegExpression) expressionNode()      {}

//LT  Expression
type LTExpression struct {
	Token       lexer.Token
	Left        Expression
	Right       Expression
}

func (lt *LTExpression) TokenLiteral() string { return lt.Token.Literal }
func (lt *LTExpression) expressionNode()      {}

//LE  Expression
type LEExpression struct {
	Token       lexer.Token
	Left        Expression
	Right       Expression
}

func (le *LEExpression) TokenLiteral() string { return le.Token.Literal }
func (le *LEExpression) expressionNode()      {}


//EQ  Expression
type EQExpression struct {
	Token       lexer.Token
	Left        Expression
	Right       Expression
}

func (eq *EQExpression) TokenLiteral() string { return eq.Token.Literal }
func (eq *EQExpression) expressionNode()      {}

//PLUS  Expression
type PlusExpression struct {
	Token       lexer.Token
	Left        Expression
	Right       Expression
}

func (pe *PlusExpression) TokenLiteral() string { return pe.Token.Literal }
func (pe *PlusExpression) expressionNode()      {}

//MINUS  Expression
type MinusExpression struct {
	Token       lexer.Token
	Left        Expression
	Right       Expression
}

func (me *MinusExpression) TokenLiteral() string { return me.Token.Literal }
func (me *MinusExpression) expressionNode()      {}

//TIMES  Expression
type TimesExpression struct {
	Token       lexer.Token
	Left        Expression
	Right       Expression
}

func (te *TimesExpression) TokenLiteral() string { return te.Token.Literal }
func (te *TimesExpression) expressionNode()      {}

//DIVIDE  Expression
type DivideExpression struct {
	Token       lexer.Token
	Left        Expression
	Right       Expression
}

func (de *DivideExpression) TokenLiteral() string { return de.Token.Literal }
func (de *DivideExpression) expressionNode()      {}


type InfixExpression struct {
	Token    lexer.Token 
	Left     Expression
	Operator string
	Right    Expression
}

func (ie *InfixExpression) TokenLiteral() string { return ie.Token.Literal }
func (ie *InfixExpression) expressionNode()      {}

