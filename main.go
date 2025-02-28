package main

import (
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"cool-compiler/codegen"
	"cool-compiler/lexer"
	"cool-compiler/parser"
	"cool-compiler/semant"
)

func main() {
	// Get the COOL input file via the -i flag.
	inputFilePath := flag.String("i", "", "Path to the COOL source file")
	flag.Parse()
	if *inputFilePath == "" {
		fmt.Println("Error: Input file path is required. Use -i <filename.cool>")
		os.Exit(1)
	}

	// Read the COOL source code from the file.
	codeBytes, err := os.ReadFile(*inputFilePath)
	if err != nil {
		fmt.Printf("Error reading file %s: %v\n", *inputFilePath, err)
		os.Exit(1)
	}

	// Preprocess imports
	baseDir := filepath.Dir(*inputFilePath)
	code, err := parser.PreprocessImports(string(codeBytes), baseDir)
	if err != nil {
		fmt.Printf("Error processing imports: %v\n", err)
		os.Exit(1)
	}

	fmt.Println("=== Input Program ===")
	fmt.Println(code)
	fmt.Println("\n=== Starting Compilation ===")

	// Create lexer and parser with preprocessed code
	lex := lexer.NewLexer(strings.NewReader(code))
	parser := parser.New(lex)

	// Parse the program.
	program := parser.ParseProgram()
	if len(parser.Errors()) != 0 {
		fmt.Println("=== Parser Errors ===")
		for _, err := range parser.Errors() {
			fmt.Println(err)
		}
		os.Exit(1)
	}

	fmt.Println(parser.PrintAST(program))
	// Run semantic analysis.
	analyzer := semant.NewSemanticAnalyzer()
	analyzer.Analyze(program)
	if len(analyzer.Errors()) != 0 {
		fmt.Println("=== Semantic Errors ===")
		for _, err := range analyzer.Errors() {
			fmt.Println(err)
		}
		os.Exit(1)
	}
	fmt.Println("=== Semantic Analysis Successful ===")

	// Generate LLVM IR.
	generator := codegen.NewCodeGenerator()
	module, err := generator.Generate(program)
	if err != nil {
		fmt.Printf("Error generating LLVM IR: %v\n", err)
		os.Exit(1)
	}

	fmt.Println("\n=== Generated LLVM IR ===")

	// Write the LLVM IR to a file.
	irFilename := strings.TrimSuffix(*inputFilePath, ".cool") + ".ll"
	if err := os.WriteFile(irFilename, []byte(module.String()), 0644); err != nil {
		fmt.Printf("Error writing IR to file %s: %v\n", irFilename, err)
		os.Exit(1)
	}

	// Run the generated IR using lli.
	fmt.Printf("\n=== Running LLVM IR with lli (%s) ===\n", irFilename)
	cmd := exec.Command("lli", irFilename)
	output, err := cmd.CombinedOutput()
	if err != nil {
		fmt.Printf("Error running lli: %v\n", err)
		os.Exit(1)
	}
	fmt.Printf("Program output:\n%s\n", output)
}
