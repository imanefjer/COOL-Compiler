package main

import (
	"cool-compiler/lexer"
	"flag"
	"fmt"
	"os"
	"strings"
)

func main() {
	
	inputFilePath := flag.String("i", "", "lexical/lexer/lexer.go")
	flag.Parse()

	if *inputFilePath == "" {
		fmt.Println("Error: Input file path is required.")
		os.Exit(1)
	}

	code, err := os.ReadFile(*inputFilePath)
	if err != nil {
		fmt.Printf("Error reading input file: %v %v\n", err, *inputFilePath)
		os.Exit(1)
	}

	l := lexer.NewLexer(strings.NewReader(string(code)))
	token := l.NextToken()
	for token.Type != lexer.EOF && token.Type != lexer.ERROR {
		fmt.Printf("%s %s\n", token.Literal, token.Type)
		token = l.NextToken()
	}
	fmt.Println("Done compiling!")

}
