package parser

import (
	"cool-compiler/ast"
	"fmt"
	"strings"
)
func SerializeExpression(exp ast.Expression) string {
    switch node := exp.(type) {
    case *ast.IntegerLiteral:
        return fmt.Sprintf("%d", node.Value)
    case *ast.StringLiteral:
        return fmt.Sprintf("%q", node.Value)
    case *ast.BooleanLiteral:
        return fmt.Sprintf("%t", node.Value)
    case *ast.ObjectIdentifier:
        return node.Value
    case *ast.Assignment:
        return fmt.Sprintf("%s <- %s", node.Name.Value, SerializeExpression(node.Expression))
    case *ast.MethodCall:
        args := make([]string, len(node.Arguments))
        for i, arg := range node.Arguments {
            args[i] = SerializeExpression(arg)
        }
        if node.Type != nil {
            return fmt.Sprintf("%s@%s.%s(%s)", 
                SerializeExpression(node.Object),
                node.Type.Value,
                node.Method.Value,
                strings.Join(args, ", "))
        }
        return fmt.Sprintf("%s.%s(%s)", 
            SerializeExpression(node.Object),
            node.Method.Value,
            strings.Join(args, ", "))
    case *ast.FunctionCall:
        args := make([]string, len(node.Arguments))
        for i, arg := range node.Arguments {
            args[i] = SerializeExpression(arg)
        }
        return fmt.Sprintf("%s(%s)", node.Function.Value, strings.Join(args, ", "))
    case *ast.InfixExpression:
        return fmt.Sprintf("(%s %s %s)", 
            SerializeExpression(node.Left), 
            node.Operator, 
            SerializeExpression(node.Right))
    case *ast.CaseExpression:
        var sb strings.Builder
        sb.WriteString("case ")
        sb.WriteString(SerializeExpression(node.Expression))
        sb.WriteString(" of ")
        for i, c := range node.Cases {
            if i > 0 {
                sb.WriteString(", ")
            }
            sb.WriteString(fmt.Sprintf("%s: %s => %s", 
                c.Name.Value, 
                c.Type.Value, 
                SerializeExpression(c.Expression)))
        }
        sb.WriteString(" esac")
        return sb.String()
    case *ast.NotExpression:
        return fmt.Sprintf("not %s", SerializeExpression(node.Expression))
    case *ast.NegExpression:
        return fmt.Sprintf("~%s", SerializeExpression(node.Expression))
    case *ast.BinaryExpression:
        return fmt.Sprintf("(%s %s %s)", 
            SerializeExpression(node.Left), 
            node.Operator, 
            SerializeExpression(node.Right))
    case *ast.IfExpression:
        return fmt.Sprintf("if %s then %s else %s fi", 
            SerializeExpression(node.Condition), 
            SerializeExpression(node.Consequence), 
            SerializeExpression(node.Alternative))
    case *ast.WhileExpression:
        return fmt.Sprintf("while %s loop %s pool", 
            SerializeExpression(node.Condition), 
            SerializeExpression(node.Body))
    case *ast.BlockExpression:
        var sb strings.Builder
        sb.WriteString("{ ")
        for i, expr := range node.Expressions {
            sb.WriteString(SerializeExpression(expr))
            if i < len(node.Expressions)-1 {
                sb.WriteString("; ")
            }
        }
        sb.WriteString(" }")
        return sb.String()
    case *ast.LetExpression:
        var sb strings.Builder
        sb.WriteString("let ")
        sb.WriteString(node.Name.Value)
        sb.WriteString(" : ")
        sb.WriteString(node.Type.Value)
        if node.Init != nil {
            sb.WriteString(" <- ")
            sb.WriteString(SerializeExpression(node.Init))
        }
        for _, binding := range node.Bindings {
            sb.WriteString(", ")
            sb.WriteString(binding.Name.Value)
            sb.WriteString(" : ")
            sb.WriteString(binding.Type.Value)
            if binding.Init != nil {
                sb.WriteString(" <- ")
                sb.WriteString(SerializeExpression(binding.Init))
            }
        }
        sb.WriteString(" in ")
        sb.WriteString(SerializeExpression(node.Body))
        return sb.String()
    case *ast.NewExpression:
        return fmt.Sprintf("new %s", node.Type.Value)
    case *ast.IsVoidExpression:
        return fmt.Sprintf("isvoid %s", SerializeExpression(node.Expression))
    default:
        return fmt.Sprintf("Unknown expression type: %T", node)
    }
}

// SerializeProgram serializes an entire AST Program into a string representation.
func SerializeProgram(program *ast.Program) string {
	var sb strings.Builder

	for i, class := range program.Classes {
		sb.WriteString(SerializeClass(class))
		if i < len(program.Classes)-1 {
			sb.WriteString("\n")
		}
	}

	return sb.String()
}

// SerializeClass serializes a Class node into a string representation.
func SerializeClass(class *ast.Class) string {
	var sb strings.Builder

	// Write class declaration
	sb.WriteString(fmt.Sprintf("class %s", class.Name.Value))
	if class.Parent != nil && class.Parent.Value != "Object" {
		sb.WriteString(fmt.Sprintf(" inherits %s", class.Parent.Value))
	}
	sb.WriteString(" {\n")

	// Write features
	for _, feature := range class.Features {
		switch f := feature.(type) {
		case *ast.Method:
			sb.WriteString(SerializeMethod(f))
		case *ast.Attribute:
			sb.WriteString(SerializeAttribute(f))
		}
		sb.WriteString(";\n")
	}

	sb.WriteString("}")
	return sb.String()
}

// SerializeMethod serializes a Method node into a string representation.
func SerializeMethod(method *ast.Method) string {
	var sb strings.Builder

	// Write method name
	sb.WriteString(fmt.Sprintf("\t%s(", method.Name.Value))

	// Write parameters
	for i, param := range method.Parameters {
		sb.WriteString(fmt.Sprintf("%s: %s", param.Name.Value, param.Type.Value))
		if i < len(method.Parameters)-1 {
			sb.WriteString(", ")
		}
	}

	// Write return type and body
	sb.WriteString(fmt.Sprintf(") : %s ", method.ReturnType.Value))
	sb.WriteString(SerializeExpression(method.Body))

	return sb.String()
}

// SerializeAttribute serializes an Attribute node into a string representation.
func SerializeAttribute(attr *ast.Attribute) string {
	var sb strings.Builder

	// Write attribute name and type
	sb.WriteString(fmt.Sprintf("\t%s: %s", attr.Name.Value, attr.Type.Value))

	// Write initialization if present
	if attr.Init != nil {
		sb.WriteString(" <- ")
		sb.WriteString(SerializeExpression(attr.Init))
	}

	return sb.String()
}
