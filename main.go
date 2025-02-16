package main

import (
	"cool-compiler/lexer"
	"cool-compiler/parser"
	"cool-compiler/semant"
	"flag"
	"fmt"
	"os"
	"strings"
)

func main() {
	inputFilePath := flag.String("i", "", "Path to the COOL source file")
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

	// Debug: Print the input
	fmt.Println("\n=== Input Program ===")
	fmt.Println(string(code))
	fmt.Println("\n=== Starting Compilation ===")

	// Create lexer and parser
	l := lexer.NewLexer(strings.NewReader(string(code)))
	p := parser.New(l)

	// Parse the program
	program := p.ParseProgram()
	if len(p.Errors()) != 0 {
		fmt.Println("\n=== Parser Errors ===")
		for _, err := range p.Errors() {
			fmt.Println(err)
		}
		os.Exit(1)
	}

	fmt.Println("\n=== Parsing Successful ===")
	fmt.Println("\n=== Starting Semantic Analysis ===")

	// Create and run semantic analyzer
	analyzer := semant.NewSemanticAnalyzer()
	analyzer.Analyze(program)

	// Check for semantic errors
	if len(analyzer.Errors()) != 0 {
		fmt.Println("\n=== Semantic Errors ===")
		for _, err := range analyzer.Errors() {
			fmt.Println(err)
		}
		os.Exit(1)
	}

	fmt.Println("\n=== Semantic Analysis Successful ===")
}
