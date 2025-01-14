package main

import (
	"cool-compiler/lexer"
	"cool-compiler/parser"
	"flag"
	"fmt"
	"os"
	"strings"
)

func main() {

	inputFilePath := flag.String("i", "", "Path to your program")
	flag.Parse()

	if *inputFilePath == "" {
		fmt.Println("Error: Input file path is required.")
		os.Exit(1)
	}

	code, err := os.ReadFile(*inputFilePath)
	if err != nil {
		fmt.Printf("Error reading input file: %v\n", err)
		os.Exit(1)
	}

	l := lexer.NewLexer(strings.NewReader(string(code)))
	p := parser.New(l)
	_ = p.ParseProgram()

	if len(p.Errors()) > 0 {
		fmt.Println("Parsing Errors:")
		for _, msg := range p.Errors() {
			fmt.Println(msg)
		}
		os.Exit(1)
	}

	fmt.Println("Done compiling!")
}
