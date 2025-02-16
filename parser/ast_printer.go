package parser

import (
	"cool-compiler/ast"
	"fmt"
	"strings"
)

// PrintAST prints the AST in a tree structure
func PrintAST(program *ast.Program) string {
	var sb strings.Builder
	sb.WriteString("AST Tree:\n")
	sb.WriteString("└── Program\n")
	
	// Print each class
	for i, class := range program.Classes {
		prefix := "    ├── "
		if i == len(program.Classes)-1 {
			prefix = "    └── "
		}
		sb.WriteString(prefix + "Class: " + class.Name.Value + "\n")
		classPrefix := "    │   "
		if i == len(program.Classes)-1 {
			classPrefix = "        "
		}
		
		if class.Parent != nil {
			sb.WriteString(classPrefix + "├── Parent: " + class.Parent.Value + "\n")
		}
		
		// Print features
		for j, feature := range class.Features {
			featurePrefix := classPrefix + "├── "
			if j == len(class.Features)-1 {
				featurePrefix = classPrefix + "└── "
			}
			
			switch f := feature.(type) {
			case *ast.Method:
				sb.WriteString(featurePrefix + "Method: " + f.Name.Value + "\n")
				methodPrefix := classPrefix + "│   "
				if j == len(class.Features)-1 {
					methodPrefix = classPrefix + "    "
				}
				
				sb.WriteString(methodPrefix + "├── ReturnType: " + f.ReturnType.Value + "\n")
				
				// Print parameters
				if len(f.Parameters) > 0 {
					sb.WriteString(methodPrefix + "├── Parameters:\n")
					for k, param := range f.Parameters {
						paramPrefix := methodPrefix + "│   ├── "
						if k == len(f.Parameters)-1 {
							paramPrefix = methodPrefix + "│   └── "
						}
						sb.WriteString(paramPrefix + param.Name.Value + ": " + param.Type.Value + "\n")
					}
				}
				
				// Print body
				sb.WriteString(methodPrefix + "└── Body:\n")
				sb.WriteString(printExpression(f.Body, methodPrefix+"    "))
				
			case *ast.Attribute:
				sb.WriteString(featurePrefix + "Attribute: " + f.Name.Value + "\n")
				attrPrefix := classPrefix + "│   "
				if j == len(class.Features)-1 {
					attrPrefix = classPrefix + "    "
				}
				sb.WriteString(attrPrefix + "├── Type: " + f.Type.Value + "\n")
				if f.Init != nil {
					sb.WriteString(attrPrefix + "└── Init:\n")
					sb.WriteString(printExpression(f.Init, attrPrefix+"    "))
				}
			}
		}
	}
	return sb.String()
}

func printExpression(exp ast.Expression, indent string) string {
	var sb strings.Builder
	
	switch node := exp.(type) {
	case *ast.IntegerLiteral:
		sb.WriteString(indent + "└── Integer: " + fmt.Sprintf("%d", node.Value) + "\n")
		
	case *ast.StringLiteral:
		sb.WriteString(indent + "└── String: " + node.Value + "\n")
		
	case *ast.BooleanLiteral:
		sb.WriteString(indent + "└── Boolean: " + fmt.Sprintf("%t", node.Value) + "\n")
		
	case *ast.ObjectIdentifier:
		sb.WriteString(indent + "Identifier: " + node.Value + "\n")
		
	case *ast.Assignment:
		sb.WriteString(indent + "Assignment\n")
		sb.WriteString(indent + "    ├── Name: " + node.Name.Value + "\n")
		sb.WriteString(indent + "    └── Value:\n")
		sb.WriteString(printExpression(node.Expression, indent+"        "))
		
	case *ast.MethodCall:
		sb.WriteString(indent + "└── MethodCall\n")
		sb.WriteString(indent + "    ├── Object:\n")
		sb.WriteString(printExpression(node.Object, indent+"    │   "))
		if node.Type != nil {
			sb.WriteString(indent + "    ├── Type: " + node.Type.Value + "\n")
		}
		sb.WriteString(indent + "    ├── Method: " + node.Method.Value + "\n")
		if len(node.Arguments) > 0 {
			sb.WriteString(indent + "    └── Arguments:\n")
			for _, arg := range node.Arguments {
				sb.WriteString(printExpression(arg, indent+"        "))
			}
		}
		
	case *ast.BlockExpression:
        sb.WriteString(indent + "└── Block\n")
        for i, expr := range node.Expressions {
            exprPrefix := "├── "
            if i == len(node.Expressions)-1 {
                exprPrefix = "└── "
            }
            sb.WriteString(printExpression(expr, indent+"    "+exprPrefix))
        }
		
	case *ast.IfExpression:
		sb.WriteString(indent + "└── If\n")
		sb.WriteString(indent + "    ├── Condition:\n")
		sb.WriteString(printExpression(node.Condition, indent+"    │   "))
		sb.WriteString(indent + "    ├── Then:\n")
		sb.WriteString(printExpression(node.Consequence, indent+"    │   "))
		sb.WriteString(indent + "    └── Else:\n")
		sb.WriteString(printExpression(node.Alternative, indent+"        "))
		
	case *ast.WhileExpression:
		sb.WriteString(indent + "└── While\n")
		sb.WriteString(indent + "    ├── Condition:\n")
		sb.WriteString(printExpression(node.Condition, indent+"    │   "))
		sb.WriteString(indent + "    └── Body:\n")
		sb.WriteString(printExpression(node.Body, indent+"        "))
		
	case *ast.LetExpression:
		sb.WriteString(indent + "└── Let\n")
		sb.WriteString(indent + "    ├── Bindings:\n")
		bindingPrefix := indent + "    │   "
		
		// Print first binding
		sb.WriteString(bindingPrefix + "├── " + node.Name.Value + ": " + node.Type.Value + "\n")
		if node.Init != nil {
			sb.WriteString(bindingPrefix + "│   └── Init:\n")
			sb.WriteString(printExpression(node.Init, bindingPrefix+"    "))
		}
		
		// Print additional bindings
		for i, binding := range node.Bindings {
			prefix := bindingPrefix + "├── "
			if i == len(node.Bindings)-1 {
				prefix = bindingPrefix + "└── "
			}
			sb.WriteString(prefix + binding.Name.Value + ": " + binding.Type.Value + "\n")
			if binding.Init != nil {
				initPrefix := bindingPrefix + "│   "
				if i == len(node.Bindings)-1 {
					initPrefix = bindingPrefix + "    "
				}
				sb.WriteString(initPrefix + "└── Init:\n")
				sb.WriteString(printExpression(binding.Init, initPrefix+"    "))
			}
		}
		
		sb.WriteString(indent + "    └── Body:\n")
		sb.WriteString(printExpression(node.Body, indent+"        "))
		
	case *ast.CaseExpression:
		sb.WriteString(indent + "└── Case\n")
		sb.WriteString(indent + "    ├── Expression:\n")
		sb.WriteString(printExpression(node.Expression, indent+"    │   "))
		sb.WriteString(indent + "    └── Cases:\n")
		for i, c := range node.Cases {
			prefix := indent + "        ├── "
			if i == len(node.Cases)-1 {
				prefix = indent + "        └── "
			}
			sb.WriteString(prefix + c.Name.Value + ": " + c.Type.Value + " =>\n")
			sb.WriteString(printExpression(c.Expression, indent+"            "))
		}
		
	case *ast.NewExpression:
		sb.WriteString(indent + "└── New: " + node.Type.Value + "\n")
		
	case *ast.IsVoidExpression:
		sb.WriteString(indent + "└── IsVoid\n")
		sb.WriteString(printExpression(node.Expression, indent+"    "))
		
	case *ast.NotExpression:
		sb.WriteString(indent + "└── Not\n")
		sb.WriteString(printExpression(node.Expression, indent+"    "))
		
	case *ast.NegExpression:
		sb.WriteString(indent + "└── Negation\n")
		sb.WriteString(printExpression(node.Expression, indent+"    "))
		
	case *ast.InfixExpression:
		sb.WriteString(indent + "└── Infix: " + node.Operator + "\n")
		sb.WriteString(indent + "    ├── Left:\n")
		sb.WriteString(printExpression(node.Left, indent+"    │   "))
		sb.WriteString(indent + "    └── Right:\n")
		sb.WriteString(printExpression(node.Right, indent+"        "))
		
	case *ast.FunctionCall:
		sb.WriteString(indent + "└── FunctionCall: " + node.Function.Value + "\n")
		if len(node.Arguments) > 0 {
			sb.WriteString(indent + "    └── Arguments:\n")
			for i, arg := range node.Arguments {
				argPrefix := indent + "        ├── "
				if i == len(node.Arguments)-1 {
					argPrefix = indent + "        └── "
				}
				sb.WriteString(printExpression(arg, argPrefix))
			}
		}
		
	default:
		sb.WriteString(indent + "└── Unknown: " + fmt.Sprintf("%T", node) + "\n")
	}
	
	return sb.String()
}