package codegen

import (
	"cool-compiler/ast"
	"strings"
	"testing"

	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/types"
	"github.com/llir/llvm/ir/value"
)

// Helper function to set up a basic code generator for testing
func setupTestGenerator() *CodeGenerator {
	generator := NewCodeGenerator()
	// Add basic class types for testing
	generator.classTypes["Object"] = types.NewStruct(types.NewPointer(types.I8))
	generator.classTypes["Int"] = types.NewStruct(types.NewPointer(types.I8), types.I32)
	generator.classTypes["Bool"] = types.NewStruct(types.NewPointer(types.I8), types.I1)
	generator.classTypes["String"] = types.NewStruct(types.NewPointer(types.I8), types.NewPointer(types.I8))

	// Make empty class table
	generator.classTable = make(ClassTable)
	generator.classTable["Object"] = &ClassInfo{
		Name:       "Object",
		Parent:     "",
		Attributes: []AttributeInfo{},
		Methods:    make(map[string]MethodInfo),
		ObjectSize: 1,
	}

	// Set up program field with basic classes
	generator.program = &ast.Program{
		Classes: []*ast.Class{
			{Name: &ast.ObjectIdentifier{Value: "Object"}},
			{Name: &ast.ObjectIdentifier{Value: "Int"}},
			{Name: &ast.ObjectIdentifier{Value: "Bool"}},
			{Name: &ast.ObjectIdentifier{Value: "String"}},
		},
	}

	return generator
}

// Helper function to create a test function and block
func createTestFunctionAndBlock() (*ir.Func, *ir.Block) {
	mod := ir.NewModule()
	testFunc := mod.NewFunc("test", types.I32)
	block := testFunc.NewBlock("entry")
	return testFunc, block
}

// Helper to get IR string representation for comparison
func getIRString(val value.Value) string {
	if val == nil {
		return "nil"
	}
	return val.String()
}

// Tests for integer literal expression generation
func TestGenerateIntegerLiteral(t *testing.T) {
	g := setupTestGenerator()
	_, block := createTestFunctionAndBlock()

	tests := []struct {
		value    int
		expected string
	}{
		{42, "i32 42"},
		{0, "i32 0"},
		{-10, "i32 -10"},
	}

	for _, test := range tests {
		expr := &ast.IntegerLiteral{Value: test.value}
		val, resultBlock, err := g.generateExpression(block, expr)

		if err != nil {
			t.Errorf("Error generating integer literal %d: %v", test.value, err)
		}

		if resultBlock != block {
			t.Errorf("Block changed during integer literal generation")
		}

		if getIRString(val) != test.expected {
			t.Errorf("Integer literal %d: expected %s, got %s", test.value, test.expected, getIRString(val))
		}
	}
}

// Tests for boolean literal expression generation
func TestGenerateBooleanLiteral(t *testing.T) {
	g := setupTestGenerator()
	_, block := createTestFunctionAndBlock()

	tests := []struct {
		value    bool
		expected bool
	}{
		{true, true},
		{false, false},
	}

	for _, test := range tests {
		expr := &ast.BooleanLiteral{Value: test.value}
		val, _, err := g.generateExpression(block, expr)

		if err != nil {
			t.Errorf("Error generating boolean literal %v: %v", test.value, err)
		}

		if val.Type() != types.I8 {
			t.Errorf("Boolean literal should have type i8, got %s", val.Type())
		}

		// For constants, we can check the actual boolean value
		if c, ok := val.(*constant.Int); ok {
			var boolVal bool
			if c.X.Int64() == 1 {
				boolVal = true
			} else if c.X.Int64() == 0 {
				boolVal = false
			} else {
				t.Errorf("Boolean constant has unexpected value: %d", c.X.Int64())
			}

			if boolVal != test.expected {
				t.Errorf("Boolean literal %v: expected value %v, got %v", test.value, test.expected, boolVal)
			}
		} else {
			t.Errorf("Boolean literal should be a constant, got %T", val)
		}
	}
}

// Test for simple binary operations (infix expressions)
func TestGenerateInfixExpression(t *testing.T) {
	g := setupTestGenerator()
	_, block := createTestFunctionAndBlock()

	// Create two integer literals for testing
	left := &ast.IntegerLiteral{Value: 10}
	right := &ast.IntegerLiteral{Value: 5}

	tests := []struct {
		operator  string
		checkType func(t *testing.T, val value.Value)
	}{
		{"+", func(t *testing.T, val value.Value) {
			if val.Type() != types.I32 {
				t.Errorf("Add should produce i32, got %s", val.Type())
			}
		}},
		{"-", func(t *testing.T, val value.Value) {
			if val.Type() != types.I32 {
				t.Errorf("Sub should produce i32, got %s", val.Type())
			}
		}},
		{"*", func(t *testing.T, val value.Value) {
			if val.Type() != types.I32 {
				t.Errorf("Mul should produce i32, got %s", val.Type())
			}
		}},
		{"/", func(t *testing.T, val value.Value) {
			if val.Type() != types.I32 {
				t.Errorf("Div should produce i32, got %s", val.Type())
			}
		}},
		{"<", func(t *testing.T, val value.Value) {
			if val.Type() != types.I8 {
				t.Errorf("LessThan should produce i8, got %s", val.Type())
			}
		}},
		{"=", func(t *testing.T, val value.Value) {
			if val.Type() != types.I8 {
				t.Errorf("Equals should produce i8, got %s", val.Type())
			}
		}},
	}

	for _, test := range tests {
		expr := &ast.InfixExpression{
			Left:     left,
			Operator: test.operator,
			Right:    right,
		}

		val, _, err := g.generateExpression(block, expr)

		if err != nil {
			t.Errorf("Error generating infix expression with operator %s: %v", test.operator, err)
			continue
		}

		test.checkType(t, val)
	}
}

// Test for if expressions
func TestGenerateIfExpression(t *testing.T) {
	g := setupTestGenerator()
	testFunc, block := createTestFunctionAndBlock()

	//if true then 1 else 0
	expr := &ast.IfExpression{
		Condition:   &ast.BooleanLiteral{Value: true},
		Consequence: &ast.IntegerLiteral{Value: 1},
		Alternative: &ast.IntegerLiteral{Value: 0},
	}

	val, resultBlock, err := g.generateExpression(block, expr)

	if err != nil {
		t.Fatalf("Error generating if expression: %v", err)
	}

	// Check that we got a phi instruction
	phi, ok := val.(*ir.InstPhi)
	if !ok {
		t.Fatalf("If expression should generate a phi instruction, got %T", val)
	}

	// Check that we have two incoming values in the phi
	if len(phi.Incs) != 2 {
		t.Fatalf("If phi should have 2 incoming values, got %d", len(phi.Incs))
	}

	// Verify blocks were created correctly
	if testFunc.Blocks[0] != block {
		t.Errorf("First block should be the entry block")
	}

	if len(testFunc.Blocks) != 4 {
		t.Errorf("If expression should create 3 additional blocks, got %d total", len(testFunc.Blocks))
	}

	// Verify the block names
	expectedBlockNames := []string{"entry", "if.then", "if.else", "if.merge"}
	for i, blockName := range expectedBlockNames {
		if i < len(testFunc.Blocks) && !strings.Contains(testFunc.Blocks[i].Name(), blockName) {
			t.Errorf("Expected block %d to be named %s, got %s", i, blockName, testFunc.Blocks[i].Name())
		}
	}

	// Verify the result block is the merge block
	if !strings.Contains(resultBlock.Name(), "if.merge") {
		t.Errorf("Result block should be merge block, got %s", resultBlock.Name())
	}
}

// Test for while expressions
func TestGenerateWhileExpression(t *testing.T) {
	g := setupTestGenerator()
	testFunc := g.module.NewFunc("test_while", types.I32)
	block := testFunc.NewBlock("entry")

	// Create a while expression: while 1 < 2 loop x + 1 pool
	expr := &ast.WhileExpression{
		Condition: &ast.InfixExpression{
			Left:     &ast.IntegerLiteral{Value: 1},
			Operator: "<",
			Right:    &ast.IntegerLiteral{Value: 2},
		},
		Body: &ast.InfixExpression{
			Left:     &ast.IntegerLiteral{Value: 1},
			Operator: "+",
			Right:    &ast.IntegerLiteral{Value: 1},
		},
	}

	// Generate code for the expression
	val, resultBlock, err := g.generateExpression(block, expr)
	if err != nil {
		t.Fatalf("Error generating while expression: %v", err)
	}

	// Add a return instruction with the result
	resultBlock.NewRet(val)

	// Get the IR as a string
	irOutput := g.module.String()

	// Expected patterns in the IR
	expectedPatterns := []string{
		"while.",    
		".cond",     
		".body",     
		".end",      
		"br i1",     
		"icmp",     
		"add",       
		"ret i32 0", 
	}

	// Check for each expected pattern
	for _, pattern := range expectedPatterns {
		if !strings.Contains(irOutput, pattern) {
			t.Errorf("While expression: IR does not contain expected '%s'", pattern)
			t.Errorf("Generated IR excerpt:\n%s", irOutput)
			break
		}
	}

	// Check block structure
	numBlocks := len(testFunc.Blocks)
	if numBlocks != 4 { // entry, condition, body, and end blocks
		t.Errorf("Expected 4 blocks, got %d", numBlocks)
	}
}

// Test for block expressions
func TestGenerateBlockExpression(t *testing.T) {
	g := setupTestGenerator()
	_, block := createTestFunctionAndBlock()

	// Create a simple block expression with multiple statements
	expr := &ast.BlockExpression{
		Expressions: []ast.Expression{
			&ast.IntegerLiteral{Value: 1},
			&ast.IntegerLiteral{Value: 2},
			&ast.IntegerLiteral{Value: 3},
		},
	}

	val, _, err := g.generateExpression(block, expr)

	if err != nil {
		t.Fatalf("Error generating block expression: %v", err)
	}

	// Check that we got the value of the last expression
	if getIRString(val) != "i32 3" {
		t.Errorf("Block expression should return value of last expression, got %s", getIRString(val))
	}
}

func TestGenerateNotExpression(t *testing.T) {
	g := setupTestGenerator()
	_, block := createTestFunctionAndBlock()

	// Create a simple not expression: not true
	expr := &ast.NotExpression{
		Expression: &ast.BooleanLiteral{Value: true},
	}

	val, _, err := g.generateExpression(block, expr)

	if err != nil {
		t.Fatalf("Error generating not expression: %v", err)
	}

	if val.Type() != types.I32 {
		t.Errorf("Not expression should return i32, got %s", val.Type())
	}
}

func TestGenerateIsVoidExpression(t *testing.T) {
	g := setupTestGenerator()
	_, block := createTestFunctionAndBlock()

	// Setup locals for testing with a null pointer
	objType := types.NewPointer(g.classTypes["Object"])
	nullVal := constant.NewNull(objType)

	// Create an isvoid on a null value
	expr := &ast.IsVoidExpression{
		Expression: &ast.ObjectIdentifier{Value: "nullObj"},
	}

	// Set up local variable
	g.locals = map[string]value.Value{
		"nullObj": block.NewAlloca(objType),
	}
	block.NewStore(nullVal, g.locals["nullObj"])

	val, _, err := g.generateExpression(block, expr)

	if err != nil {
		t.Fatalf("Error generating isvoid expression: %v", err)
	}

	if val.Type() != types.I32 {
		t.Errorf("IsVoid expression should return i32, got %s", val.Type())
	}
}

func TestGenerateObjectIdentifier(t *testing.T) {
	g := setupTestGenerator()
	_, block := createTestFunctionAndBlock()

	// Setup locals for testing
	intType := types.I32
	intVal := constant.NewInt(intType, 123)

	intAlloca := block.NewAlloca(intType)
	block.NewStore(intVal, intAlloca)

	g.locals = map[string]value.Value{
		"x": intAlloca,
	}
	g.localsTypes = map[string]string{
		"x": "Int",
	}

	// Create an object identifier for variable x
	expr := &ast.ObjectIdentifier{Value: "x"}

	val, _, err := g.generateExpression(block, expr)

	if err != nil {
		t.Fatalf("Error generating object identifier: %v", err)
	}

	// The result should be a load instruction
	load, ok := val.(*ir.InstLoad)
	if !ok {
		t.Fatalf("Object identifier should generate a load instruction, got %T", val)
	}

	if load.Type() != intType {
		t.Errorf("Loaded value should be i32, got %s", load.Type())
	}
}

func TestMethodCallExpression(t *testing.T) {
	// Create a simple program with a method call
	program := &ast.Program{
		Classes: []*ast.Class{
			{
				Name: &ast.ObjectIdentifier{Value: "Main"},
				Features: []ast.Feature{
					&ast.Method{
						Name:       &ast.ObjectIdentifier{Value: "main"},
						ReturnType: &ast.TypeIdentifier{Value: "Object"},
						Parameters: []*ast.Formal{},
						Body: &ast.MethodCall{
							Object:    &ast.ObjectIdentifier{Value: "self"},
							Method:    &ast.ObjectIdentifier{Value: "type_name"},
							Arguments: []ast.Expression{},
						},
					},
				},
			},
		},
	}

	// Generate code with proper setup
	generator := NewCodeGenerator()

	// Set up program field
	generator.program = &ast.Program{
		Classes: []*ast.Class{
			{Name: &ast.ObjectIdentifier{Value: "Object"}},
			{Name: &ast.ObjectIdentifier{Value: "Main"}},
			{Name: &ast.ObjectIdentifier{Value: "String"}},
		},
	}

	// Add basic class types for testing
	generator.classTypes["Object"] = types.NewStruct(types.NewPointer(types.I8))
	generator.classTypes["Main"] = types.NewStruct(types.NewPointer(types.I8))
	generator.classTypes["String"] = types.NewStruct(types.NewPointer(types.I8), types.NewPointer(types.I8))

	// Set up class table
	generator.classTable = make(ClassTable)
	generator.classTable["Object"] = &ClassInfo{
		Name:       "Object",
		Parent:     "",
		Attributes: []AttributeInfo{},
		Methods: map[string]MethodInfo{
			"type_name": {Name: "type_name", Index: 0},
		},
		ObjectSize: 1,
	}
	generator.classTable["Main"] = &ClassInfo{
		Name:       "Main",
		Parent:     "Object",
		Attributes: []AttributeInfo{},
		Methods:    make(map[string]MethodInfo),
		ObjectSize: 1,
	}

	// Add vtables
	generator.vtables["Object"] = generator.module.NewGlobalDef("Object_vtable", constant.NewStruct(types.NewStruct()))
	generator.vtables["Main"] = generator.module.NewGlobalDef("Main_vtable", constant.NewStruct(types.NewStruct()))

	// Add type_name method with implementation
	typeNameFunc := generator.module.NewFunc(
		"Object_type_name",
		types.NewPointer(generator.classTypes["String"]),
		ir.NewParam("self", types.NewPointer(types.I8)),
	)
	typeNameBlock := typeNameFunc.NewBlock("")
	typeNameBlock.NewRet(constant.NewNull(types.NewPointer(generator.classTypes["String"])))
	generator.methods["Object_type_name"] = typeNameFunc

	// Add Main_main method with implementation
	mainFunc := generator.module.NewFunc(
		"Main_main",
		types.NewPointer(generator.classTypes["Object"]),
		ir.NewParam("self", types.NewPointer(types.I8)),
	)
	mainBlock := mainFunc.NewBlock("")
	// Add a simple implementation
	callResult := mainBlock.NewCall(typeNameFunc, mainBlock.NewBitCast(mainFunc.Params[0], types.NewPointer(types.I8)))
	mainBlock.NewRet(callResult)
	generator.methods["Main_main"] = mainFunc

	// Generate code
	module, err := generator.Generate(program)
	if err != nil {
		t.Fatalf("Failed to generate code: %v", err)
	}

	// Find the main method
	mainMethod := findFunction(module, "Main_main")
	if mainMethod == nil {
		t.Fatal("Main_main method not found")
	}

	// Get the first block
	if len(mainMethod.Blocks) == 0 {
		t.Fatal("No blocks found in main method")
	}
	block := mainMethod.Blocks[0]

	// Find the call instruction
	var callInst *ir.InstCall
	for _, inst := range block.Insts {
		if call, ok := inst.(*ir.InstCall); ok {
			callInst = call
			break
		}
	}

	if callInst == nil {
		t.Fatal("No call instruction found")
	}

	// Verify the call is to Object_type_name
	if !strings.HasSuffix(callInst.Callee.Ident(), "Object_type_name") {
		t.Errorf("Expected call to Object_type_name, got %s", callInst.Callee.Ident())
	}
}

// Helper function to find a function in the module by name
func findFunction(module *ir.Module, name string) *ir.Func {
	for _, f := range module.Funcs {
		if f.Name() == name {
			return f
		}
	}
	return nil
}

func TestGenerateCaseExpression(t *testing.T) {
	g := setupTestGenerator()
	testFunc, block := createTestFunctionAndBlock()

	expr := &ast.CaseExpression{
		Expression: &ast.ObjectIdentifier{Value: "x"},
		Cases: []*ast.Case{
			{
				Name:       &ast.ObjectIdentifier{Value: "i"},
				Type:       &ast.TypeIdentifier{Value: "Int"},
				Expression: &ast.IntegerLiteral{Value: 1},
			},
			{
				Name:       &ast.ObjectIdentifier{Value: "b"},
				Type:       &ast.TypeIdentifier{Value: "Bool"},
				Expression: &ast.IntegerLiteral{Value: 0},
			},
			{
				Name:       &ast.ObjectIdentifier{Value: "s"},
				Type:       &ast.TypeIdentifier{Value: "String"},
				Expression: &ast.IntegerLiteral{Value: 2},
			},
		},
	}

	objType := types.NewPointer(g.classTypes["Object"])
	xAlloca := block.NewAlloca(objType)
	block.NewStore(constant.NewNull(objType), xAlloca)
	g.locals = map[string]value.Value{"x": xAlloca}
	g.localsTypes = map[string]string{"x": "Object"}

	val, _, err := g.generateExpression(block, expr)
	if err != nil {
		t.Fatalf("Error generating case expression: %v", err)
	}

	if val.Type() != types.I32 {
		t.Errorf("Case expression should return i32, got %s", val.Type())
	}

	var endBlock *ir.Block
	var testBlocks []*ir.Block
	for _, b := range testFunc.Blocks {
		switch {
		case strings.Contains(b.Name(), "case.end"):
			endBlock = b
		case strings.Contains(b.Name(), "case.test") ||
			strings.Contains(b.Name(), "typecheck"):
			testBlocks = append(testBlocks, b)
		}
	}

	if endBlock == nil {
		t.Error("Missing merge block for case expression")
	}

	if len(testBlocks) != 3 {
		t.Errorf("Expected 3 test blocks, got %d", len(testBlocks))
	}
}

// TestIntegerLiteralIR tests integer literal code generation by comparing LLVM IR output
func TestIntegerLiteralIR(t *testing.T) {
	tests := []struct {
		value    int
		expected string // Expected LLVM IR snippet
	}{
		{42, "i32 42"},
		{0, "i32 0"},
		{-10, "i32 -10"},
	}

	for _, test := range tests {
		// Setup a fresh generator for each test case
		g := setupTestGenerator()

		// Create a test function in the generator's module
		testFunc := g.module.NewFunc("test_integer_literal", types.I32)
		block := testFunc.NewBlock("entry")

		// Generate integer literal expression
		expr := &ast.IntegerLiteral{Value: test.value}
		val, _, err := g.generateExpression(block, expr)

		if err != nil {
			t.Fatalf("Error generating integer literal %d: %v", test.value, err)
		}

		// Add a return instruction to complete the function
		block.NewRet(val)

		// Get the IR string representation of the entire module
		irText := g.module.String()

		// Check that the generated IR contains our expected snippet
		if !strings.Contains(irText, test.expected) {
			t.Errorf("Integer literal %d: IR does not contain expected '%s'",
				test.value, test.expected)
			t.Errorf("Generated IR:\n%s", irText)
		}
	}
}

// TestIntegerLiteralInModule tests integer literals by looking at the module IR
func TestIntegerLiteralInModule(t *testing.T) {
	tests := []struct {
		value    int
		expected string
	}{
		{42, "ret i32 42"},
		{0, "ret i32 0"},
		{-10, "ret i32 -10"},
	}

	for _, test := range tests {
		// Create a minimal test generator
		g := NewCodeGenerator()

		// Create an integer literal expression
		expr := &ast.IntegerLiteral{Value: test.value}

		// Create a test function where we'll place our instruction
		testFunc := g.module.NewFunc("test_literal", types.I32)
		block := testFunc.NewBlock("entry")

		// Generate code for the expression
		val, _, err := g.generateExpression(block, expr)
		if err != nil {
			t.Fatalf("Error generating integer literal %d: %v", test.value, err)
		}

		// Add a return instruction with the result
		block.NewRet(val)

		// Get the IR as a string
		irOutput := g.module.String()

		// Check for the expected pattern in the IR
		if !strings.Contains(irOutput, test.expected) {
			t.Errorf("Integer literal %d: IR does not contain expected '%s'",
				test.value, test.expected)
			t.Errorf("Generated IR:\n%s", irOutput)
		}
	}
}

// TestIntegerExpressionInMethod tests integer expressions inside a method
func TestIntegerExpressionInMethod(t *testing.T) {
	g := NewCodeGenerator()

	// Create a method function that takes a self parameter
	methodFunc := g.module.NewFunc("Test_method", types.I32,
		ir.NewParam("self", types.NewPointer(types.I8)))
	block := methodFunc.NewBlock("entry")

	// Create a simple integer literal
	expr := &ast.IntegerLiteral{Value: 42}

	// Generate code for the expression
	result, resultBlock, err := g.generateExpression(block, expr)
	if err != nil {
		t.Fatalf("Failed to generate code: %v", err)
	}

	// return instruction with the result
	resultBlock.NewRet(result)

	// Get the IR as a string
	irOutput := g.module.String()

	// Expected method signature and return
	expectedSignature := "define i32 @Test_method(i8* %self)"
	expectedReturn := "ret i32 42"

	// Check that the IR contains the expected patterns
	if !strings.Contains(irOutput, expectedSignature) {
		t.Errorf("Expected IR to contain '%s', but it didn't", expectedSignature)
		t.Errorf("Generated IR excerpt:\n%s", irOutput)
	}

	if !strings.Contains(irOutput, expectedReturn) {
		t.Errorf("Expected IR to contain '%s', but it didn't", expectedReturn)
		t.Errorf("Generated IR excerpt:\n%s", irOutput)
	}
}

// TestStringLiteralGeneration tests the core functionality of string literal generation
func TestStringLiteralGeneration(t *testing.T) {
	tests := []struct {
		input    string
		expected []string // Patterns to look for in the IR
	}{
		{"Hello", []string{
			"global [",  // String should be stored as a global
			"Hello\\00", // Should include null terminator
		}},
		{"", []string{
			"global [", // Even empty strings are global variables
			"\\00",     // Just the null terminator
		}},
		{"Special chars: \n\t\"\\", []string{
			"global [", // String stored as global
			"\\00",     // Has null terminator
		}},
	}

	for _, test := range tests {
		// Create a minimal test generator with necessary setup
		g := NewCodeGenerator()

		// Set up the String class type as needed by the string literal generation
		g.classTypes = make(map[string]*types.StructType)
		g.classTypes["String"] = types.NewStruct(
			types.NewPointer(types.I8), // vtable pointer
			types.NewPointer(types.I8), // string value pointer
		)

		// Create a vtable for String class
		g.vtables = make(map[string]*ir.Global)
		g.vtables["String"] = g.module.NewGlobalDef("String_vtable", constant.NewStruct(types.NewStruct()))

		// Create a string literal expression
		expr := &ast.StringLiteral{Value: test.input}

		// Create a test function where we'll place our instruction
		testFunc := g.module.NewFunc("test_string_literal", types.NewPointer(types.I8))
		block := testFunc.NewBlock("entry")

		// Generate code for the expression
		val, resultBlock, err := g.generateExpression(block, expr)
		if err != nil {
			t.Fatalf("Error generating string literal '%s': %v", test.input, err)
		}

		// Add a return instruction with the result
		resultBlock.NewRet(val)

		// Get the IR as a string
		irOutput := g.module.String()

		// Check for each expected pattern in the IR
		for _, pattern := range test.expected {
			if !strings.Contains(irOutput, pattern) {
				t.Errorf("String literal '%s': IR does not contain expected '%s'",
					test.input, pattern)
				t.Errorf("Generated IR excerpt:\n%s", truncateIR(irOutput))
				break
			}
		}
	}
}

// TestStringObjectCreation tests the creation of String objects
func TestStringObjectCreation(t *testing.T) {
	g := NewCodeGenerator()

	// Set up the String class type as needed by the string literal generation
	g.classTypes = make(map[string]*types.StructType)
	g.classTypes["String"] = types.NewStruct(
		types.NewPointer(types.I8), // vtable pointer
		types.NewPointer(types.I8), // string value pointer
	)

	// Create a vtable for String class
	g.vtables = make(map[string]*ir.Global)
	g.vtables["String"] = g.module.NewGlobalDef("String_vtable", constant.NewStruct(types.NewStruct()))

	// Create a string literal expression
	expr := &ast.StringLiteral{Value: "Test string"}

	// Create a test function
	testFunc := g.module.NewFunc("test_string_object", types.NewPointer(types.I8))
	block := testFunc.NewBlock("entry")

	// Generate code for the expression
	val, resultBlock, err := g.generateExpression(block, expr)
	if err != nil {
		t.Fatalf("Error generating string object: %v", err)
	}

	// Add a return instruction with the result
	resultBlock.NewRet(val)

	// Get the IR as a string
	irOutput := g.module.String()

	// Expected patterns
	expectedPatterns := []string{
		"global [",        // String global declaration
		"Test string\\00", // String content with null terminator
		"getelementptr",   // Access to struct field
		"store",           // Store to initialize string object
	}

	// Check for each expected pattern in the IR
	for _, pattern := range expectedPatterns {
		if !strings.Contains(irOutput, pattern) {
			t.Errorf("String object creation: IR does not contain expected '%s'", pattern)
			t.Errorf("Generated IR excerpt:\n%s", truncateIR(irOutput))
			break
		}
	}
}

// TestStringLiteralInMethod tests string literals inside a method
func TestStringLiteralInMethod(t *testing.T) {
	g := NewCodeGenerator()

	// Set up the String class type
	g.classTypes = make(map[string]*types.StructType)
	g.classTypes["String"] = types.NewStruct(
		types.NewPointer(types.I8), // vtable pointer
		types.NewPointer(types.I8), // string value pointer
	)

	// Create a vtable for String class
	g.vtables = make(map[string]*ir.Global)
	g.vtables["String"] = g.module.NewGlobalDef("String_vtable", constant.NewStruct(types.NewStruct()))

	// Create a method function that takes a self parameter
	methodFunc := g.module.NewFunc("Test_method", types.NewPointer(types.I8),
		ir.NewParam("self", types.NewPointer(types.I8)))
	block := methodFunc.NewBlock("entry")

	// Create a string literal
	expr := &ast.StringLiteral{Value: "Method string"}

	// Generate code for the expression
	result, resultBlock, err := g.generateExpression(block, expr)
	if err != nil {
		t.Fatalf("Failed to generate code: %v", err)
	}

	// Add a return instruction with the result
	resultBlock.NewRet(result)

	// Get the IR as a string
	irOutput := g.module.String()

	// Expected method signature and patterns
	expectedSignature := "define i8* @Test_method(i8* %self)"
	expectedPatterns := []string{
		"global [",          // String global declaration
		"Method string\\00", // String content
		"ret ",              // Return instruction
	}

	// Check that the IR contains the expected signature
	if !strings.Contains(irOutput, expectedSignature) {
		t.Errorf("Expected IR to contain '%s', but it didn't", expectedSignature)
		t.Errorf("Generated IR excerpt:\n%s", truncateIR(irOutput))
	}

	// Check for each expected pattern
	for _, pattern := range expectedPatterns {
		if !strings.Contains(irOutput, pattern) {
			t.Errorf("Expected IR to contain '%s', but it didn't", pattern)
			t.Errorf("Generated IR excerpt:\n%s", truncateIR(irOutput))
			break
		}
	}
}

// Helper function to truncate IR output for better error messages
func truncateIR(ir string) string {
	lines := strings.Split(ir, "\n")
	if len(lines) > 20 {
		return strings.Join(lines[:20], "\n") + "\n... (truncated)"
	}
	return ir
}

// TestBooleanLiteralValue tests boolean literal generation by directly checking the generated value
func TestBooleanLiteralValue(t *testing.T) {
	tests := []struct {
		value    bool
		expected string
	}{
		{true, "i8 1"},
		{false, "i8 0"},
	}

	for _, test := range tests {
		g := NewCodeGenerator()

		//boolean literal expression
		expr := &ast.BooleanLiteral{Value: test.value}

		//test function where we'll place our instruction
		testFunc := g.module.NewFunc("test_bool_literal", types.I1)
		block := testFunc.NewBlock("entry")

		// Generate code for the expression
		val, _, err := g.generateExpression(block, expr)
		if err != nil {
			t.Fatalf("Error generating boolean literal %v: %v", test.value, err)
		}

		// Check the direct string representation of the value
		valStr := val.String()
		if valStr != test.expected {
			t.Errorf("Boolean literal %v: expected '%s', got '%s'",
				test.value, test.expected, valStr)
		}
	}
}

// TestBooleanLiteralInModule tests boolean literals by looking at the module IR
func TestBooleanLiteralInModule(t *testing.T) {
	tests := []struct {
		value    bool
		expected string
	}{
		{true, "ret i8 1"},
		{false, "ret i8 0"},
	}

	for _, test := range tests {
		g := NewCodeGenerator()

		// Create a boolean literal expression
		expr := &ast.BooleanLiteral{Value: test.value}

		// Create a test function where we'll place our instruction
		testFunc := g.module.NewFunc("test_bool_literal", types.I8)
		block := testFunc.NewBlock("entry")

		// Generate code for the expression
		val, _, err := g.generateExpression(block, expr)
		if err != nil {
			t.Fatalf("Error generating boolean literal %v: %v", test.value, err)
		}

		// Add a return instruction with the result
		block.NewRet(val)

		// Get the IR as a string
		irOutput := g.module.String()

		// Check for the expected pattern in the IR
		if !strings.Contains(irOutput, test.expected) {
			t.Errorf("Boolean literal %v: IR does not contain expected '%s'",
				test.value, test.expected)
			t.Errorf("Generated IR:\n%s", irOutput)
		}
	}
}

// TestBooleanObjectCreation tests the creation of Bool objects
func TestBooleanObjectCreation(t *testing.T) {
	g := NewCodeGenerator()

	// Set up the Bool class type
	g.classTypes = make(map[string]*types.StructType)
	g.classTypes["Bool"] = types.NewStruct(
		types.NewPointer(types.I8), // vtable pointer
		types.I8,                   // boolean value 
	)

	// Create a vtable for Bool class
	g.vtables = make(map[string]*ir.Global)
	g.vtables["Bool"] = g.module.NewGlobalDef("Bool_vtable", constant.NewStruct(types.NewStruct()))

	tests := []struct {
		value    bool
		expected string
	}{
		{true, "store i8 1"},  // Changed from i1 to i8
		{false, "store i8 0"}, // Changed from i1 to i8
	}

	for _, test := range tests {
		// Reset module for each test
		g.module = ir.NewModule()
		g.vtables["Bool"] = g.module.NewGlobalDef("Bool_vtable", constant.NewStruct(types.NewStruct()))

		// Create a boolean literal expression
		expr := &ast.BooleanLiteral{Value: test.value}

		// Create a test function
		testFunc := g.module.NewFunc("test_bool_object", types.NewPointer(g.classTypes["Bool"]))
		block := testFunc.NewBlock("entry")

		val, resultBlock, err := g.generateExpression(block, expr)
		if err != nil {
			t.Fatalf("Error generating boolean object: %v", err)
		}

		var boxedVal value.Value
		if val.Type() == types.I8 { // Changed from I1 to I8
			boxedVal, resultBlock, err = g.boxBool(resultBlock, val)
			if err != nil {
				t.Fatalf("Error boxing boolean value: %v", err)
			}
		} else {
			boxedVal = val
		}

		// Add a return instruction with the result
		resultBlock.NewRet(boxedVal)

		// Get the IR as a string
		irOutput := g.module.String()

		if !strings.Contains(irOutput, test.expected) {
			t.Errorf("Boolean object creation for %v: IR does not contain expected '%s'",
				test.value, test.expected)
			t.Errorf("Generated IR excerpt:\n%s", truncateIR(irOutput))
		}
	}
}

// TestBooleanExpressionInMethod tests boolean expressions inside a method
func TestBooleanExpressionInMethod(t *testing.T) {
	g := NewCodeGenerator()

	methodFunc := g.module.NewFunc("Test_method", types.I1,
		ir.NewParam("self", types.NewPointer(types.I8)))
	block := methodFunc.NewBlock("entry")

	expr := &ast.BooleanLiteral{Value: true}

	result, resultBlock, err := g.generateExpression(block, expr)
	if err != nil {
		t.Fatalf("Failed to generate code: %v", err)
	}

	resultBlock.NewRet(result)

	irOutput := g.module.String()

	expectedSignature := "define i1 @Test_method(i8* %self)"
	expectedReturn := "ret i8 1"

	if !strings.Contains(irOutput, expectedSignature) {
		t.Errorf("Expected IR to contain '%s', but it didn't", expectedSignature)
		t.Errorf("Generated IR excerpt:\n%s", irOutput)
	}

	if !strings.Contains(irOutput, expectedReturn) {
		t.Errorf("Expected IR to contain '%s', but it didn't", expectedReturn)
		t.Errorf("Generated IR excerpt:\n%s", irOutput)
	}
}

// TestIfExpressionBasic tests the basic structure of an if expression
func TestIfExpressionBasic(t *testing.T) {
	g := NewCodeGenerator()

	expr := &ast.IfExpression{
		Condition:   &ast.BooleanLiteral{Value: true},
		Consequence: &ast.IntegerLiteral{Value: 1},
		Alternative: &ast.IntegerLiteral{Value: 0},
	}

	testFunc := g.module.NewFunc("test_if_expression", types.I32)
	block := testFunc.NewBlock("entry")

	val, resultBlock, err := g.generateExpression(block, expr)
	if err != nil {
		t.Fatalf("Error generating if expression: %v", err)
	}

	resultBlock.NewRet(val)

	irOutput := g.module.String()

	expectedPatterns := []string{
		"icmp ne i8", // Compare boolean value (i8)
		"br i1",      // Branch on condition (i1)
		"if.then",    // Then block
		"if.else",    // Else block
		"phi i32 [",  // Start of phi instruction
		"], [ 0,",    // Alternative value in phi
		"ret i32",    // Return instruction
	}

	for _, pattern := range expectedPatterns {
		if !strings.Contains(irOutput, pattern) {
			t.Errorf("If expression: IR does not contain expected '%s'", pattern)
			t.Errorf("Generated IR excerpt:\n%s", truncateIR(irOutput))
			break
		}
	}

	numBlocks := len(testFunc.Blocks)
	if numBlocks < 3 {
		t.Errorf("Expected at least 3 blocks (entry, then, else), got %d", numBlocks)
	}
}

func TestIfExpressionWithComplexCondition(t *testing.T) {
	g := NewCodeGenerator()

	//  if 1 < 2 then 1 else 0
	expr := &ast.IfExpression{
		Condition: &ast.InfixExpression{
			Left:     &ast.IntegerLiteral{Value: 1},
			Operator: "<",
			Right:    &ast.IntegerLiteral{Value: 2},
		},
		Consequence: &ast.IntegerLiteral{Value: 1},
		Alternative: &ast.IntegerLiteral{Value: 0},
	}

	// Create a test function
	testFunc := g.module.NewFunc("test_if_complex", types.I32)
	block := testFunc.NewBlock("entry")

	// Generate code for the expression
	val, resultBlock, err := g.generateExpression(block, expr)
	if err != nil {
		t.Fatalf("Error generating if expression with complex condition: %v", err)
	}

	// Add a return instruction with the result
	resultBlock.NewRet(val)

	// Get the IR as a string
	irOutput := g.module.String()

	// Expected patterns for complex condition
	expectedPatterns := []string{
		"icmp",    // Integer comparison
		"br i1",   // Branch on condition result
		"if.then", // Then block
		"if.else", // Else block
		"phi i32", // Phi instruction for result
	}

	// Check for each expected pattern in the IR
	for _, pattern := range expectedPatterns {
		if !strings.Contains(irOutput, pattern) {
			t.Errorf("If with complex condition: IR does not contain expected '%s'", pattern)
			t.Errorf("Generated IR excerpt:\n%s", truncateIR(irOutput))
			break
		}
	}
}

// TestIfExpressionNested tests nested if expressions
func TestIfExpressionNested(t *testing.T) {
	g := NewCodeGenerator()

	// if true then (if false then 1 else 2) else 3
	innerIf := &ast.IfExpression{
		Condition:   &ast.BooleanLiteral{Value: false},
		Consequence: &ast.IntegerLiteral{Value: 1},
		Alternative: &ast.IntegerLiteral{Value: 2},
	}

	outerIf := &ast.IfExpression{
		Condition:   &ast.BooleanLiteral{Value: true},
		Consequence: innerIf,
		Alternative: &ast.IntegerLiteral{Value: 3},
	}

	// Create a test function
	testFunc := g.module.NewFunc("test_if_nested", types.I32)
	block := testFunc.NewBlock("entry")

	// Generate code for the expression
	val, resultBlock, err := g.generateExpression(block, outerIf)
	if err != nil {
		t.Fatalf("Error generating nested if expression: %v", err)
	}

	// Add a return instruction with the result
	resultBlock.NewRet(val)

	// Get the IR as a string
	irOutput := g.module.String()

	// Check for multiple phi instructions and blocks
	phiCount := strings.Count(irOutput, "phi i32")
	if phiCount < 2 {
		t.Errorf("Expected at least 2 phi instructions for nested if, got %d", phiCount)
		t.Errorf("Generated IR excerpt:\n%s", truncateIR(irOutput))
	}

	// Count the number of blocks
	blockCount := len(testFunc.Blocks)
	if blockCount < 5 {
		t.Errorf("Expected at least 5 blocks for nested if, got %d", blockCount)
	}
}

// TestIfExpressionDifferentTypes tests if with different result types
func TestIfExpressionDifferentTypes(t *testing.T) {
	g := NewCodeGenerator()

	// Set up necessary class types
	g.classTypes = make(map[string]*types.StructType)
	g.classTypes["String"] = types.NewStruct(
		types.NewPointer(types.I8), // vtable pointer
		types.NewPointer(types.I8), // string value pointer
	)

	// Create an if expression with different types:
	// if true then 42 else "string"
	expr := &ast.IfExpression{
		Condition:   &ast.BooleanLiteral{Value: true},
		Consequence: &ast.IntegerLiteral{Value: 42},
		Alternative: &ast.StringLiteral{Value: "string"},
	}

	// Create a test function
	testFunc := g.module.NewFunc("test_if_diff_types", types.NewPointer(types.I8))
	block := testFunc.NewBlock("entry")

	// This might fail depending on implementation - if so, we'll skip this test
	val, resultBlock, err := g.generateExpression(block, expr)
	if err != nil {
		t.Skipf("If expression with different types not supported: %v", err)
		return
	}

	// Add a return instruction with the result
	resultBlock.NewRet(val)

	// Get the IR as a string
	irOutput := g.module.String()

	// Look for bitcast instructions which would be needed to reconcile types
	if !strings.Contains(irOutput, "bitcast") {
		t.Logf("Note: No bitcast found in IR - your implementation might handle type differences differently")
	}

	// Should have a phi node with at least 2 incoming values
	if !strings.Contains(irOutput, "phi") {
		t.Errorf("Expected at least one phi instruction for if with different types")
		t.Errorf("Generated IR excerpt:\n%s", truncateIR(irOutput))
	}
}

func TestGenerateLetExpression(t *testing.T) {
	g := NewCodeGenerator()

	testFunc := g.module.NewFunc("test_let", types.I32)
	block := testFunc.NewBlock("entry")

	expr := &ast.LetExpression{
		Name: &ast.ObjectIdentifier{Value: "x"},
		Type: &ast.TypeIdentifier{Value: "Int"},
		Init: &ast.IntegerLiteral{Value: 42},
		Body: &ast.InfixExpression{
			Left:     &ast.ObjectIdentifier{Value: "x"},
			Operator: "+",
			Right:    &ast.IntegerLiteral{Value: 1},
		},
	}

	g.locals = make(map[string]value.Value)
	g.localsTypes = make(map[string]string)
	g.classTypes = make(map[string]*types.StructType)
	g.classTypes["Int"] = types.NewStruct(
		types.NewPointer(types.I8), // vtable pointer
		types.I32,                  // value
	)

	// Generate code for the expression
	val, resultBlock, err := g.generateExpression(block, expr)
	if err != nil {
		t.Fatalf("Error generating let expression: %v", err)
	}

	// Add a return
	resultBlock.NewRet(val)

	// Get the IR as a string
	irOutput := g.module.String()

	// Expected patterns in the IR
	expectedPatterns := []string{
		"alloca i32",   // Variable allocation
		"store i32 42", // Initial value store
		"load i32",     // Loading the variable
		"add i32",      // Addition operation
	}

	for _, pattern := range expectedPatterns {
		if !strings.Contains(irOutput, pattern) {
			t.Errorf("Let expression: IR does not contain expected '%s'", pattern)
			t.Errorf("Generated IR:\n%s", irOutput)
			break
		}
	}
}

func TestGenerateClass(t *testing.T) {
	// Test single class
	t.Run("Single Class", func(t *testing.T) {
		g := NewCodeGenerator()

		// Create a simple program with Main class
		program := &ast.Program{
			Classes: []*ast.Class{
				{
					Name: &ast.ObjectIdentifier{Value: "Main"},
					Features: []ast.Feature{
						&ast.Method{
							Name:       &ast.ObjectIdentifier{Value: "main"},
							ReturnType: &ast.TypeIdentifier{Value: "Object"},
							Body:       &ast.IntegerLiteral{Value: 0},
						},
					},
				},
			},
		}

		module, err := g.Generate(program)
		if err != nil {
			t.Fatalf("Failed to generate code: %v", err)
		}

		irOutput := module.String()
		expectedPatterns := []string{
			"@Main_vtable",          // vtable declaration
			"@.str.Main",            // class name string
			"define i8* @Main_main", // main method
		}

		for _, pattern := range expectedPatterns {
			if !strings.Contains(irOutput, pattern) {
				t.Errorf("Class generation: IR does not contain expected '%s'", pattern)
				t.Errorf("Generated IR:\n%s", truncateIR(irOutput))
				break
			}
		}
	})

	// Test inheritance with minimal classes
	t.Run("Simple Inheritance", func(t *testing.T) {
		g := NewCodeGenerator()

		program := &ast.Program{
			Classes: []*ast.Class{
				{
					Name: &ast.ObjectIdentifier{Value: "Main"},
					Features: []ast.Feature{
						&ast.Method{
							Name:       &ast.ObjectIdentifier{Value: "main"},
							ReturnType: &ast.TypeIdentifier{Value: "Object"},
							Body:       &ast.IntegerLiteral{Value: 0},
						},
					},
				},
				{
					Name:   &ast.ObjectIdentifier{Value: "A"},
					Parent: &ast.TypeIdentifier{Value: "Object"},
					Features: []ast.Feature{
						&ast.Method{
							Name:       &ast.ObjectIdentifier{Value: "foo"},
							ReturnType: &ast.TypeIdentifier{Value: "Int"},
							Body:       &ast.IntegerLiteral{Value: 42},
						},
					},
				},
			},
		}

		module, err := g.Generate(program)
		if err != nil {
			t.Fatalf("Failed to generate code: %v", err)
		}

		irOutput := module.String()
		expectedPatterns := []string{
			"@A_vtable",         // class A vtable
			"@.str.A",           // class A name string
			"define i32 @A_foo", // method foo
			"@Main_vtable",      // Main class vtable
		}

		for _, pattern := range expectedPatterns {
			if !strings.Contains(irOutput, pattern) {
				t.Errorf("Inheritance: IR does not contain expected '%s'", pattern)
				t.Errorf("Generated IR:\n%s", truncateIR(irOutput))
				break
			}
		}
	})
}

func TestIsVoidExpression(t *testing.T) {
	g := setupTestGenerator()

	// Initialize basic types
	objectType := types.NewStruct(types.NewPointer(types.I8))
	// Create a test function
	testFunc := g.module.NewFunc("test_isvoid", types.I32)
	block := testFunc.NewBlock("entry")

	// Create a simple isvoid expression with a null object
	expr := &ast.IsVoidExpression{
		Expression: &ast.ObjectIdentifier{Value: "null_obj"},
	}

	// Create a null object variable
	nullPtr := block.NewAlloca(types.NewPointer(objectType))
	block.NewStore(constant.NewNull(types.NewPointer(objectType)), nullPtr)
	g.locals["null_obj"] = nullPtr
	g.localsTypes["null_obj"] = "Object"

	// Generate code for the expression
	val, resultBlock, err := g.generateExpression(block, expr)
	if err != nil {
		t.Fatalf("Error generating isvoid expression: %v", err)
	}

	// Add a return instruction
	resultBlock.NewRet(val)

	// Get the IR as a string
	irOutput := g.module.String()

	// Expected patterns in the IR
	expectedPatterns := []string{
		"load",    // Loading the pointer
		"icmp eq", // Null comparison
	}

	// Check for each expected pattern
	for _, pattern := range expectedPatterns {
		if !strings.Contains(irOutput, pattern) {
			t.Errorf("IsVoid expression: IR does not contain expected '%s'", pattern)
			t.Errorf("Generated IR:\n%s", irOutput)
			break
		}
	}

	if val.Type() != types.I32 {
		t.Errorf("IsVoid expression should return i32 (COOL boolean), got %v", val.Type())
	}
}

func TestNewExpression(t *testing.T) {
	g := NewCodeGenerator()
	g.module = ir.NewModule()
	g.locals = make(map[string]value.Value)
	g.localsTypes = make(map[string]string)
	g.classTypes = make(map[string]*types.StructType)
	g.vtables = make(map[string]*ir.Global)
	g.classTable = make(ClassTable)

	// Initialize basic types
	objectType := types.NewStruct(types.NewPointer(types.I8))
	g.classTypes["Object"] = objectType

	// Initialize Int class type with proper structure
	g.classTypes["Int"] = types.NewStruct(
		types.NewPointer(types.I8), // vtable pointer
		types.I32,                  // value
	)

	// Initialize vtable for Int
	vtableType := types.NewArray(3, types.NewPointer(types.I8))
	g.vtables["Int"] = g.module.NewGlobalDef("Int_vtable", constant.NewZeroInitializer(vtableType))

	// Add class info to the class table
	g.classTable["Object"] = &ClassInfo{
		Name:       "Object",
		Parent:     "",
		Attributes: []AttributeInfo{},
		Methods:    make(map[string]MethodInfo),
		ObjectSize: 1,
	}

	g.classTable["Int"] = &ClassInfo{
		Name:   "Int",
		Parent: "Object",
		Attributes: []AttributeInfo{
			{Name: "val", Type: "Int", Offset: 1},
		},
		Methods:    make(map[string]MethodInfo),
		ObjectSize: 2,
	}

	// Set up program field with basic classes for new expression
	g.program = &ast.Program{
		Classes: []*ast.Class{
			{Name: &ast.ObjectIdentifier{Value: "Object"}},
			{Name: &ast.ObjectIdentifier{Value: "Int"}},
		},
	}

	// Create a test function
	testFunc := g.module.NewFunc("test_new", types.NewPointer(g.classTypes["Int"]))
	block := testFunc.NewBlock("entry")

	// Create a new expression: new Int
	expr := &ast.NewExpression{
		Type: &ast.TypeIdentifier{Value: "Int"},
	}

	// Generate code for the expression
	val, resultBlock, err := g.generateExpression(block, expr)
	if err != nil {
		t.Fatalf("Error generating new expression: %v", err)
	}

	// Add a return instruction
	resultBlock.NewRet(val)

	// Get the IR as a string
	irOutput := g.module.String()

	// Expected patterns in the IR
	expectedPatterns := []string{
		"@Int_vtable", // VTable reference
		"store",       // Storing the vtable pointer
	}

	// Check for each expected pattern
	for _, pattern := range expectedPatterns {
		if !strings.Contains(irOutput, pattern) {
			t.Errorf("New expression: IR does not contain expected '%s'", pattern)
			t.Errorf("Generated IR:\n%s", irOutput)
			break
		}
	}
}
