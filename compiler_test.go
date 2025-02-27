package main

import (
	"bytes"
	"cool-compiler/codegen"
	"cool-compiler/lexer"
	"cool-compiler/parser"
	"cool-compiler/semant"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

func TestCompilerWithFile(t *testing.T) {
	testCases := []struct {
		name     string
		coolCode string
		expected string
	}{
		{
			name: "Factorial Program",
			coolCode: `class Main inherits IO {
    main(): Object {
        let n: Int <- 5, fact: Int <- 1 in {
            while 0 < n loop {
                fact <- fact * n;
                n <- n - 1;
            } pool;
            out_int(fact).out_string("\n");
        }
    };
};`,
			expected: "120",
		},
		{
			name: "Hello World Program",
			coolCode: `class Main inherits IO {
    main(): Object {
        out_string("Hello, World!\n")
    };
};`,
			expected: "Hello, World!",
		},
		{
			name: "Conditional with Class Attribute",
			coolCode: `class Main inherits IO {
    a : Int; 
    main(): Object {
        {
        a <- 3;
        if ( a = 3) then
            out_string("a is 3\n")
        else
            out_string("a is not 3\n")
        fi;
        }
    };
};`,
			expected: "a is 3",
		},
		{
			name: "Inheritance and Method Override",
			coolCode: `class A {
    print(): Object { (new IO).out_string("A\n") };
};

class B inherits A {
    print(): Object { (new IO).out_string("B\n") };
};

class Main inherits IO {
    main(): Object {
        (new B).print()
    };
};`,
			expected: "B",
		},
		{
			name: "Complex Inheritance and Object Copy",
			coolCode: `class Animal {
    name : String <- "Unknown";

    init(n : String) : Animal {
        {
            name <- n;
            self;
        }
    };

    getName() : String {
        name
    };
    
    print() : Object {
        (new IO).out_string("This is an animal\n")
    };
};

class Dog inherits Animal {
    breed : String <- "Mixed";

    init(n : String) : Dog {
        {
            name <- n;
            self;
        }
    };

    getBreed() : String {
        breed
    };
    
    print() : Object {
        (new IO).out_string("This is a dog\n")
    };
};

class Main inherits IO {
    main() : Object {
        {
            let myDog : Dog <- (new Dog).init("Buddy") in {
                out_string("Dog name: ");
                out_string(myDog.getName());
                out_string("\n");
                
                out_string("Dog breed: ");
                out_string(myDog.getBreed());
                out_string("\n");
                
                out_string("Dog print method: ");
                myDog.print();
                
                out_string("Animal print method: ");
                myDog@Animal.print();
                
                out_string("Copy name: ");
                let dogCopy : Dog <- myDog.copy() in {
                    out_string(dogCopy.getName());
                    out_string("\n");
                };
            };
        }
    };
};`,
			expected: `Dog name: Buddy
Dog breed: Mixed
Dog print method: This is a dog
Animal print method: This is an animal
Copy name: Buddy`,
		},
		{
			name: "Complex Arithmetic Operations",
			coolCode: `class Main inherits IO {
    main(): Object {
        {
            let 
                a : Int <- 10,
                b : Int <- 5,
                c : Int <- 3,
                result : Int
            in {
                result <- (a * b + c) / (2 + c) - (b * c);
                out_string("Result of (10 * 5 + 3) / (2 + 3) - (5 * 3) is: ");
                out_int(result);
                out_string("\n");

                result <- (a + b * (c - 1)) * (b - c) + a;
                out_string("Result of (10 + 5 * (3 - 1)) * (5 - 3) + 10 is: ");
                out_int(result);
                out_string("\n");

                result <- a * b / c + (b - c) * (a + b);
                out_string("Result of 10 * 5 / 3 + (5 - 3) * (10 + 5) is: ");
                out_int(result);
                out_string("\n");

                if (result < 100) then {
                    out_string("Result is less than 100\n");
                } else {
                    out_string("Result is greater than or equal to 100\n");
                } fi;
            };
        }
    };
};`,
			expected: `Result of (10 * 5 + 3) / (2 + 3) - (5 * 3) is: -5
Result of (10 + 5 * (3 - 1)) * (5 - 3) + 10 is: 50
Result of 10 * 5 / 3 + (5 - 3) * (10 + 5) is: 46
Result is less than 100`,
		},
		{
			name: "GCD Implementation",
			coolCode: `class Main inherits IO {
        gcd_iterative(a: Int, b: Int): Int {    
            let temp: Int <- 0, x: Int <- a, y: Int <- b in {    
                while 0 < y loop {      
                    temp <- y;      
                    y <- x - (x/y)*y;      
                    x <- temp;     
                } pool;     
                x;   
            }
        };

    gcd_recursive(a: Int, b: Int): Int {
        {
            if b = 0 then
                a
            else
                gcd_recursive(b, a - (a/b)*b)
            fi;
        }
    };

    main(): Object {
        {
            out_string("Iterative GCD(3,6) = ");
            out_int(gcd_iterative(3,6));
            out_string("\n");

            out_string("Recursive GCD(48,18) = ");
            out_int(gcd_recursive(48,18));
            out_string("\n");

            out_string("Iterative GCD(54,14) = ");
            out_int(gcd_iterative(54,14));
            out_string("\n");

            out_string("Recursive GCD(60,30) = ");
            out_int(gcd_recursive(60,30));
            out_string("\n");

            -- Test with larger numbers
            out_string("Iterative GCD(120,55) = ");
            out_int(gcd_iterative(120,55));
            out_string("\n");

            out_string("Recursive GCD(175,44) = ");
            out_int(gcd_recursive(175,44));
            out_string("\n");
        }
    };
};`,
			expected: `Iterative GCD(3,6) = 3
Recursive GCD(48,18) = 6
Iterative GCD(54,14) = 2
Recursive GCD(60,30) = 30
Iterative GCD(120,55) = 5
Recursive GCD(175,44) = 1`,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			result, err := compileAndRunCOOL(t, tc.coolCode, tc.name)
			if err != nil {
				t.Fatalf("Test failed: %v", err)
			}

			got := strings.TrimSpace(result)
			if got != tc.expected {
				t.Errorf("Output mismatch:\nexpected: %q\ngot: %q", tc.expected, got)
			} else {
				t.Logf("Program output matched expected: %q", tc.expected)
			}
		})
	}
}

// compileAndRunCOOL compiles and runs a COOL program, returning its output
func compileAndRunCOOL(t *testing.T, coolCode string, testName string) (string, error) {
	// Create a temporary directory for our test files
	tmpDir, err := os.MkdirTemp("", "cooltest_"+strings.ReplaceAll(testName, " ", "_"))
	if err != nil {
		return "", fmt.Errorf("failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	// Create sanitized file name
	sanitizedName := strings.ToLower(strings.ReplaceAll(testName, " ", "_"))

	// Create the .cool file
	coolFile := filepath.Join(tmpDir, sanitizedName+".cool")
	if err := os.WriteFile(coolFile, []byte(coolCode), 0644); err != nil {
		return "", fmt.Errorf("failed to write COOL file: %v", err)
	}

	// Create paths for IR file and executable
	irFile := filepath.Join(tmpDir, sanitizedName+".ll")
	objFile := filepath.Join(tmpDir, sanitizedName+".o")
	execFile := filepath.Join(tmpDir, sanitizedName)

	// Run the parser on the file
	lex := lexer.NewLexer(strings.NewReader(coolCode))
	parser := parser.New(lex)

	// Parse the program
	program := parser.ParseProgram()
	if len(parser.Errors()) != 0 {
		return "", fmt.Errorf("parser errors: %v", parser.Errors())
	}

	// Run semantic analysis
	analyzer := semant.NewSemanticAnalyzer()
	analyzer.Analyze(program)
	if len(analyzer.Errors()) != 0 {
		return "", fmt.Errorf("semantic errors: %v", analyzer.Errors())
	}

	// Generate LLVM IR
	generator := codegen.NewCodeGenerator()
	module, err := generator.Generate(program)
	if err != nil {
		return "", fmt.Errorf("code generation error: %v", err)
	}

	// Write the LLVM IR to file
	if err := os.WriteFile(irFile, []byte(module.String()), 0644); err != nil {
		return "", fmt.Errorf("failed to write IR to file: %v", err)
	}

	// Try with direct lli first
	t.Logf("Trying to run %s with lli directly...", testName)
	cmd := exec.Command("lli", irFile)
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr

	err = cmd.Run()
	if err != nil {
		t.Logf("Direct lli run failed for %s: %v", testName, err)
		t.Logf("STDERR: %s", stderr.String())

		// Try compiling to object file and then executable
		t.Logf("Trying to compile %s with llc and clang...", testName)

		// Compile IR to object file
		llcCmd := exec.Command("llc", "-filetype=obj", "-o", objFile, irFile)
		llcStderr := new(bytes.Buffer)
		llcCmd.Stderr = llcStderr

		if err := llcCmd.Run(); err != nil {
			return "", fmt.Errorf("llc failed: %v\nOutput: %s", err, llcStderr.String())
		}

		// Link object file to executable
		clangCmd := exec.Command("clang", "-o", execFile, objFile)
		clangStderr := new(bytes.Buffer)
		clangCmd.Stderr = clangStderr

		if err := clangCmd.Run(); err != nil {
			return "", fmt.Errorf("clang failed: %v\nOutput: %s", err, clangStderr.String())
		}

		// Run the executable
		execCmd := exec.Command(execFile)
		execCmd.Stdout = &stdout
		execCmd.Stderr = &stderr

		if err := execCmd.Run(); err != nil {
			return "", fmt.Errorf("execution failed: %v\nOutput: %s", err, stderr.String())
		}
	}

	return stdout.String(), nil
}

func TestInputFunctions(t *testing.T) {
	testCases := []struct {
		name     string
		coolCode string
		input    string // Simulated user input
		expected string
	}{
		{
			name: "Test in_int Function",
			coolCode: `class Main inherits IO {
    main(): Object {
        let num : Int <- in_int() in {
            out_string("You entered: ");
            out_int(num);
            out_string("\n");
        }
    };
};`,
			input:    "42\n",
			expected: "You entered: 42",
		},
		{
			name: "Test in_string Function",
			coolCode: `class Main inherits IO {
    main(): Object {
        let str : String <- in_string() in {
            out_string("You entered: ");
            out_string(str);
        }
    };
};`,
			input:    "Hello COOL!\n",
			expected: "You entered: Hello COOL!",
		},
		{
			name: "Test Both Input Functions",
			coolCode: `class Main inherits IO {
    main(): Object {
        {
            out_string("Enter your name: ");
            let name : String <- in_string() in {
                out_string("Enter your age: ");
                let age : Int <- in_int() in {
                    out_string("Hello, ");
                    out_string(name);
                    out_string(". You are ");
                    out_int(age);
                    out_string(" years old.\n");
                };
            };
        }
    };
};`,
			input:    "Imane\n20\n",
			expected: "Enter your name: Enter your age: Hello, Imane. You are 20 years old.",
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			// Run the test with simulated input
			output, err := compileAndRunCOOLWithInput(tc.coolCode, tc.input, tc.name)
			if err != nil {
				t.Fatalf("Test failed: %v", err)
			}

			// Normalize output by removing extra whitespace
			normalizedOutput := strings.TrimSpace(output)
			normalizedExpected := strings.TrimSpace(tc.expected)

			if normalizedOutput != normalizedExpected {
				t.Errorf("Output mismatch:\nexpected: %q\ngot: %q", normalizedExpected, normalizedOutput)
			} else {
				t.Logf("Program output matched expected: %q", normalizedExpected)
			}
		})
	}
}

// compiles and runs a COOL program with simulated input
func compileAndRunCOOLWithInput( coolCode string, input string, testName string) (string, error) {
	// Create a temporary directory for our test files
	tmpDir, err := os.MkdirTemp("", "cooltest_"+strings.ReplaceAll(testName, " ", "_"))
	if err != nil {
		return "", fmt.Errorf("failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	// Create sanitized file name
	sanitizedName := strings.ToLower(strings.ReplaceAll(testName, " ", "_"))

	// Create the .cool file
	coolFile := filepath.Join(tmpDir, sanitizedName+".cool")
	if err := os.WriteFile(coolFile, []byte(coolCode), 0644); err != nil {
		return "", fmt.Errorf("failed to write COOL file: %v", err)
	}

	// Create input file for simulated user input
	inputFile := filepath.Join(tmpDir, "input.txt")
	if err := os.WriteFile(inputFile, []byte(input), 0644); err != nil {
		return "", fmt.Errorf("failed to write input file: %v", err)
	}

	// Create paths for IR file and executable
	irFile := filepath.Join(tmpDir, sanitizedName+".ll")
	objFile := filepath.Join(tmpDir, sanitizedName+".o")
	execFile := filepath.Join(tmpDir, sanitizedName)

	// Run the parser on the file
	lex := lexer.NewLexer(strings.NewReader(coolCode))
	parser := parser.New(lex)

	// Parse the program
	program := parser.ParseProgram()
	if len(parser.Errors()) != 0 {
		return "", fmt.Errorf("parser errors: %v", parser.Errors())
	}

	// Run semantic analysis
	analyzer := semant.NewSemanticAnalyzer()
	analyzer.Analyze(program)
	if len(analyzer.Errors()) != 0 {
		return "", fmt.Errorf("semantic errors: %v", analyzer.Errors())
	}

	// Generate LLVM IR
	generator := codegen.NewCodeGenerator()
	module, err := generator.Generate(program)
	if err != nil {
		return "", fmt.Errorf("code generation error: %v", err)
	}

	// Write the LLVM IR to file
	if err := os.WriteFile(irFile, []byte(module.String()), 0644); err != nil {
		return "", fmt.Errorf("failed to write IR to file: %v", err)
	}

	// Compile to an executable first
	// Compile IR to object file
	llcCmd := exec.Command("llc", "-filetype=obj", "-o", objFile, irFile)
	llcStderr := new(bytes.Buffer)
	llcCmd.Stderr = llcStderr

	if err := llcCmd.Run(); err != nil {
		return "", fmt.Errorf("llc failed: %v\nOutput: %s", err, llcStderr.String())
	}

	// Link object file to executable
	clangCmd := exec.Command("clang", "-o", execFile, objFile)
	clangStderr := new(bytes.Buffer)
	clangCmd.Stderr = clangStderr

	if err := clangCmd.Run(); err != nil {
		return "", fmt.Errorf("clang failed: %v\nOutput: %s", err, clangStderr.String())
	}

	// Open the input file to use as stdin
	inFile, err := os.Open(inputFile)
	if err != nil {
		return "", fmt.Errorf("failed to open input file: %v", err)
	}
	defer inFile.Close()

	// Run the executable with redirected stdin
	execCmd := exec.Command(execFile)
	execCmd.Stdin = inFile
	var stdout, stderr bytes.Buffer
	execCmd.Stdout = &stdout
	execCmd.Stderr = &stderr

	if err := execCmd.Run(); err != nil {
		return "", fmt.Errorf("execution failed: %v\nOutput: %s", err, stderr.String())
	}

	return stdout.String(), nil
}
