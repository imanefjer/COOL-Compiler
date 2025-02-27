package codegen

import (
	"cool-compiler/ast"
	"cool-compiler/lexer"
)

func (g *CodeGenerator) addBuiltInClasses(program *ast.Program) {
	objectClass := &ast.Class{
		Token:  lexer.Token{Literal: "class"},
		Name:   &ast.ObjectIdentifier{Token: lexer.Token{Literal: "Object"}, Value: "Object"},
		Parent: &ast.TypeIdentifier{Value: ""}, 
		Features: []ast.Feature{
			&ast.Method{
				Token:      lexer.Token{Literal: "method"},
				Name:       &ast.ObjectIdentifier{Token: lexer.Token{Literal: "abort"}, Value: "abort"},
				Parameters: []*ast.Formal{},
				ReturnType: &ast.TypeIdentifier{Token: lexer.Token{Literal: "Object"}, Value: "Object"},
				Body:       nil,
			},
			&ast.Method{
				Token:      lexer.Token{Literal: "method"},
				Name:       &ast.ObjectIdentifier{Token: lexer.Token{Literal: "type_name"}, Value: "type_name"},
				Parameters: []*ast.Formal{},
				ReturnType: &ast.TypeIdentifier{Token: lexer.Token{Literal: "String"}, Value: "String"},
				Body:       nil,
			},
			&ast.Method{
				Token:      lexer.Token{Literal: "method"},
				Name:       &ast.ObjectIdentifier{Token: lexer.Token{Literal: "copy"}, Value: "copy"},
				Parameters: []*ast.Formal{},
				ReturnType: &ast.TypeIdentifier{Token: lexer.Token{Literal: "SELF_TYPE"}, Value: "SELF_TYPE"},
				Body:       nil,
			},
		},
	}
	ioClass := &ast.Class{
		Token:  lexer.Token{Literal: "class"},
		Name:   &ast.ObjectIdentifier{Token: lexer.Token{Literal: "IO"}, Value: "IO"},
		Parent: &ast.TypeIdentifier{Token: lexer.Token{Literal: "inherits"}, Value: "Object"},
		Features: []ast.Feature{
			&ast.Method{
				Token: lexer.Token{Literal: "method"},
				Name:  &ast.ObjectIdentifier{Token: lexer.Token{Literal: "out_string"}, Value: "out_string"},
				Parameters: []*ast.Formal{
					{
						Name: &ast.ObjectIdentifier{Token: lexer.Token{Literal: "x"}, Value: "x"},
						Type: &ast.TypeIdentifier{Token: lexer.Token{Literal: "String"}, Value: "String"},
					},
				},
				ReturnType: &ast.TypeIdentifier{Token: lexer.Token{Literal: "SELF_TYPE"}, Value: "SELF_TYPE"},
				Body:       nil,
			},
			&ast.Method{
				Token: lexer.Token{Literal: "method"},
				Name:  &ast.ObjectIdentifier{Token: lexer.Token{Literal: "out_int"}, Value: "out_int"},
				Parameters: []*ast.Formal{
					{
						Name: &ast.ObjectIdentifier{Token: lexer.Token{Literal: "x"}, Value: "x"},
						Type: &ast.TypeIdentifier{Token: lexer.Token{Literal: "Int"}, Value: "Int"},
					},
				},
				ReturnType: &ast.TypeIdentifier{Token: lexer.Token{Literal: "SELF_TYPE"}, Value: "SELF_TYPE"},
				Body:       nil,
			},
			&ast.Method{
				Token:      lexer.Token{Literal: "method"},
				Name:       &ast.ObjectIdentifier{Token: lexer.Token{Literal: "in_string"}, Value: "in_string"},
				Parameters: []*ast.Formal{},
				ReturnType: &ast.TypeIdentifier{Token: lexer.Token{Literal: "String"}, Value: "String"},
				Body:       nil,
			},
			&ast.Method{
				Token:      lexer.Token{Literal: "method"},
				Name:       &ast.ObjectIdentifier{Token: lexer.Token{Literal: "in_int"}, Value: "in_int"},
				Parameters: []*ast.Formal{},
				ReturnType: &ast.TypeIdentifier{Token: lexer.Token{Literal: "Int"}, Value: "Int"},
				Body:       nil,
			},
		},
	}

	stringClass := &ast.Class{
		Token:  lexer.Token{Literal: "class"},
		Name:   &ast.ObjectIdentifier{Token: lexer.Token{Literal: "String"}, Value: "String"},
		Parent: &ast.TypeIdentifier{Token: lexer.Token{Literal: "inherits"}, Value: "Object"},
		Features: []ast.Feature{
			&ast.Attribute{
				Name: &ast.ObjectIdentifier{Value: "value"},
				Type: &ast.TypeIdentifier{Value: "String"},
			},
			// length method
			&ast.Method{
				Token:      lexer.Token{Literal: "method"},
				Name:       &ast.ObjectIdentifier{Token: lexer.Token{Literal: "length"}, Value: "length"},
				Parameters: []*ast.Formal{},
				ReturnType: &ast.TypeIdentifier{Token: lexer.Token{Literal: "Int"}, Value: "Int"},
				Body:       &ast.IntegerLiteral{Value: 0}, 

			},
			// concat method
			&ast.Method{
				Token: lexer.Token{Literal: "method"},
				Name:  &ast.ObjectIdentifier{Token: lexer.Token{Literal: "concat"}, Value: "concat"},
				Parameters: []*ast.Formal{
					{
						Name: &ast.ObjectIdentifier{Token: lexer.Token{Literal: "s"}, Value: "s"},
						Type: &ast.TypeIdentifier{Token: lexer.Token{Literal: "String"}, Value: "String"},
					},
				},
				ReturnType: &ast.TypeIdentifier{Token: lexer.Token{Literal: "String"}, Value: "String"},
				Body:       &ast.StringLiteral{Value: ""}, 
			},
			// substr method
			&ast.Method{
				Token: lexer.Token{Literal: "method"},
				Name:  &ast.ObjectIdentifier{Token: lexer.Token{Literal: "substr"}, Value: "substr"},
				Parameters: []*ast.Formal{
					{
						Name: &ast.ObjectIdentifier{Token: lexer.Token{Literal: "i"}, Value: "i"},
						Type: &ast.TypeIdentifier{Token: lexer.Token{Literal: "Int"}, Value: "Int"},
					},
					{
						Name: &ast.ObjectIdentifier{Token: lexer.Token{Literal: "l"}, Value: "l"},
						Type: &ast.TypeIdentifier{Token: lexer.Token{Literal: "Int"}, Value: "Int"},
					},
				},
				ReturnType: &ast.TypeIdentifier{Token: lexer.Token{Literal: "String"}, Value: "String"},
				Body:       &ast.StringLiteral{Value: ""}, 
			},
		},
	}
	boolClass := &ast.Class{
        Token:  lexer.Token{Literal: "class"},
        Name:   &ast.ObjectIdentifier{Token: lexer.Token{Literal: "Bool"}, Value: "Bool"},
        Parent: &ast.TypeIdentifier{Token: lexer.Token{Literal: "inherits"}, Value: "Object"},
        Features: []ast.Feature{
            &ast.Attribute{
                Name: &ast.ObjectIdentifier{Value: "val"},
                Type: &ast.TypeIdentifier{Value: "Bool"},
            },
        },
    }
	intClass := &ast.Class{
        Token:  lexer.Token{Literal: "class"},
        Name:   &ast.ObjectIdentifier{Token: lexer.Token{Literal: "Int"}, Value: "Int"},
        Parent: &ast.TypeIdentifier{Token: lexer.Token{Literal: "inherits"}, Value: "Object"},
        Features: []ast.Feature{
            &ast.Attribute{
                Name: &ast.ObjectIdentifier{Value: "val"},
                Type: &ast.TypeIdentifier{Value: "Int"},
            },
        },
    }
	program.Classes = append([]*ast.Class{stringClass, ioClass, objectClass, boolClass, intClass}, program.Classes...)
}
