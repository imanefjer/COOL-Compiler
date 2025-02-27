package semant

import (
	"cool-compiler/ast"
	"cool-compiler/lexer"
	"fmt"
)

type SymbolKind int

const (
	SymbolClass SymbolKind = iota
	SymbolMethod
	SymbolAttribute
	SymbolLocal
)

// Symbol represents any named entity in the program
type Symbol struct {
	Name          string
	Kind          SymbolKind
	Type          string      // Type name or SELF_TYPE
	DefiningClass string      // Class where this symbol was originally defined
	Token         lexer.Token // For error reporting

	// For methods
	Parameters []*ast.Formal // Only for methods
	ReturnType string        // Only for methods
	IsBuiltin  bool          // True for methods of basic classes

	// For attributes/locals
	IsInitialized bool // Whether it has an initializer
	InitExpr      ast.Expression

	// For classes
	Parent   string             // Parent class name
	Features map[string]*Symbol // Methods and attributes defined in this class
}

// Scope represents a single scope level (class, method, let block etc)
type Scope struct {
	Kind    SymbolKind // The kind of scope (class, method, block)
	Symbols map[string]*Symbol
	Parent  *Scope
	Class   string // Name of the containing class (for SELF_TYPE resolution)
}

// SymbolTable manages the symbol table and inheritance graph
type SymbolTable struct {
	// Scopes
	CurrentScope *Scope
	GlobalScope  *Scope

	// Class hierarchy
	Classes     map[string]*Symbol
	Inheritance *InheritanceGraph

	// Built-in classes
	BasicClasses map[string]*Symbol

	// Error handling
	Errors []string
}

// InheritanceGraph manages class hierarchy relationships
type InheritanceGraph struct {
	// Maps class to its parent
	Edges map[string]string

	// For cycle detection
	Visited        map[string]bool
	RecursionStack map[string]bool
}

func NewSymbolTable() *SymbolTable {
	st := &SymbolTable{
		GlobalScope:  &Scope{Kind: SymbolClass, Symbols: make(map[string]*Symbol)},
		Classes:      make(map[string]*Symbol),
		BasicClasses: make(map[string]*Symbol),
		Inheritance:  NewInheritanceGraph(),
		Errors:       []string{},
	}

	st.CurrentScope = st.GlobalScope
	st.initializeBasicClasses()
	return st
}

func (st *SymbolTable) initializeBasicClasses() {
	// Initialize Object class first
	objectClass := &Symbol{
		Name:      "Object",
		Kind:      SymbolClass,
		IsBuiltin: true,
		Features:  make(map[string]*Symbol),
	}

	// Add basic Object methods
	objectClass.Features["abort"] = &Symbol{
		Name:       "abort",
		Kind:       SymbolMethod,
		ReturnType: "Object",
		IsBuiltin:  true,
	}

	objectClass.Features["type_name"] = &Symbol{
		Name:       "type_name",
		Kind:       SymbolMethod,
		ReturnType: "String",
		IsBuiltin:  true,
	}

	objectClass.Features["copy"] = &Symbol{
		Name:       "copy",
		Kind:       SymbolMethod,
		ReturnType: "SELF_TYPE",
		IsBuiltin:  true,
	}

	// Initialize IO class with its methods
	ioClass := &Symbol{
		Name:      "IO",
		Kind:      SymbolClass,
		Parent:    "Object",
		IsBuiltin: true,
		Features:  make(map[string]*Symbol),
	}

	// Add IO methods
	ioClass.Features["out_string"] = &Symbol{
		Name:       "out_string",
		Kind:       SymbolMethod,
		ReturnType: "SELF_TYPE",
		IsBuiltin:  true,
		Parameters: []*ast.Formal{{
			Name: &ast.ObjectIdentifier{Value: "x"},
			Type: &ast.TypeIdentifier{Value: "String"},
		}},
	}

	ioClass.Features["out_int"] = &Symbol{
		Name:       "out_int",
		Kind:       SymbolMethod,
		ReturnType: "SELF_TYPE",
		IsBuiltin:  true,
		Parameters: []*ast.Formal{{
			Name: &ast.ObjectIdentifier{Value: "x"},
			Type: &ast.TypeIdentifier{Value: "Int"},
		}},
	}

	ioClass.Features["in_string"] = &Symbol{
		Name:       "in_string",
		Kind:       SymbolMethod,
		ReturnType: "String",
		IsBuiltin:  true,
	}

	ioClass.Features["in_int"] = &Symbol{
		Name:       "in_int",
		Kind:       SymbolMethod,
		ReturnType: "Int",
		IsBuiltin:  true,
	}

	// Initialize String class with its methods
	stringClass := &Symbol{
		Name:      "String",
		Kind:      SymbolClass,
		Parent:    "Object",
		IsBuiltin: true,
		Features:  make(map[string]*Symbol),
	}

	stringClass.Features["length"] = &Symbol{
		Name:       "length",
		Kind:       SymbolMethod,
		ReturnType: "Int",
		IsBuiltin:  true,
	}

	stringClass.Features["concat"] = &Symbol{
		Name:       "concat",
		Kind:       SymbolMethod,
		ReturnType: "String",
		IsBuiltin:  true,
		Parameters: []*ast.Formal{{
			Name: &ast.ObjectIdentifier{Value: "s"},
			Type: &ast.TypeIdentifier{Value: "String"},
		}},
	}

	stringClass.Features["substr"] = &Symbol{
		Name:       "substr",
		Kind:       SymbolMethod,
		ReturnType: "String",
		IsBuiltin:  true,
		Parameters: []*ast.Formal{
			{
				Name: &ast.ObjectIdentifier{Value: "i"},
				Type: &ast.TypeIdentifier{Value: "Int"},
			},
			{
				Name: &ast.ObjectIdentifier{Value: "l"},
				Type: &ast.TypeIdentifier{Value: "Int"},
			},
		},
	}

	// Initialize Int class
	intClass := &Symbol{
		Name:      "Int",
		Kind:      SymbolClass,
		Parent:    "Object",
		IsBuiltin: true,
		Features:  make(map[string]*Symbol),
	}

	// Initialize Bool class
	boolClass := &Symbol{
		Name:      "Bool",
		Kind:      SymbolClass,
		Parent:    "Object",
		IsBuiltin: true,
		Features:  make(map[string]*Symbol),
	}

	// Add all basic classes to the symbol table
	st.BasicClasses["Object"] = objectClass
	st.BasicClasses["IO"] = ioClass
	st.BasicClasses["String"] = stringClass
	st.BasicClasses["Int"] = intClass
	st.BasicClasses["Bool"] = boolClass

	// Add them to Classes map as well
	st.Classes["Object"] = objectClass
	st.Classes["IO"] = ioClass
	st.Classes["String"] = stringClass
	st.Classes["Int"] = intClass
	st.Classes["Bool"] = boolClass

	// Set up inheritance edges
	st.Inheritance.Edges["IO"] = "Object"
	st.Inheritance.Edges["String"] = "Object"
	st.Inheritance.Edges["Int"] = "Object"
	st.Inheritance.Edges["Bool"] = "Object"
}

// EnterScope creates a new scope
func (st *SymbolTable) EnterScope(kind SymbolKind, className string) {
	newScope := &Scope{
		Kind:    kind,
		Symbols: make(map[string]*Symbol),
		Parent:  st.CurrentScope,
		Class:   className,
	}
	st.CurrentScope = newScope
}

// ExitScope returns to the parent scope
func (st *SymbolTable) ExitScope() {
	if st.CurrentScope.Parent != nil {
		st.CurrentScope = st.CurrentScope.Parent
	}
}

func NewInheritanceGraph() *InheritanceGraph {
	return &InheritanceGraph{
		Edges:          make(map[string]string),
		Visited:        make(map[string]bool),
		RecursionStack: make(map[string]bool),
	}
}

// AddInheritanceEdge adds a parent-child relationship to the inheritance graph
func (g *InheritanceGraph) AddInheritanceEdge(child, parent string) error {
	// Check for inheritance from basic types
	if parent == "Int" || parent == "String" || parent == "Bool" {
		return fmt.Errorf("class %s cannot inherit from %s", child, parent)
	}

	// Check if class is already defined
	if _, exists := g.Edges[child]; exists {
		return fmt.Errorf("class %s is redefined", child)
	}

	g.Edges[child] = parent
	return nil
}

// DetectCycles checks for inheritance cycles starting from a given class
func (g *InheritanceGraph) DetectCycles(className string) bool {
	// Reset recursion stack for new traversal
	g.RecursionStack = make(map[string]bool)
	g.Visited = make(map[string]bool)

	return g.hasCycle(className)
}

// hasCycle is a helper method that implements cycle detection using DFS
func (g *InheritanceGraph) hasCycle(className string) bool {
	// If class is in recursion stack, we found a cycle
	if g.RecursionStack[className] {
		return true
	}

	// If already visited and not in recursion stack, no cycle through this path
	if g.Visited[className] {
		return false
	}

	// Mark class as visited and add to recursion stack
	g.Visited[className] = true
	g.RecursionStack[className] = true

	// Check parent class
	if parent, exists := g.Edges[className]; exists {
		if g.hasCycle(parent) {
			return true
		}
	}

	// Remove from recursion stack and return
	g.RecursionStack[className] = false
	return false
}

// ValidateInheritance checks if all parent classes exist and no cycles exist
func (g *InheritanceGraph) ValidateInheritance() []string {
	var errors []string

	// Check each class
	for className := range g.Edges {
		// Check for cycles
		if g.DetectCycles(className) {
			errors = append(errors, fmt.Sprintf("inheritance cycle detected involving class %s", className))
			continue
		}

		// Validate parent class exists
		current := className
		for current != "Object" {
			parent, exists := g.Edges[current]
			if !exists {
				errors = append(errors, fmt.Sprintf("class %s inherits from undefined class %s",
					current, parent))
				break
			}
			current = parent
		}
	}

	return errors
}

// LookupSymbol searches for a symbol in current and parent scopes
func (st *SymbolTable) LookupSymbol(name string) (*Symbol, bool) {

	// Special case for 'self'
	if name == "self" {
		// Create a temporary symbol for 'self'
		selfSymbol := &Symbol{
			Name: "self",
			Kind: SymbolLocal,
			Type: "SELF_TYPE",
		}
		return selfSymbol, true
	}

	// Start from current scope and work up
	for scope := st.CurrentScope; scope != nil; scope = scope.Parent {
		if symbol, exists := scope.Symbols[name]; exists {

			return symbol, true
		}
	}

	// If not found in any scope, check if it's a class attribute
	if st.CurrentScope != nil && st.CurrentScope.Class != "" {
		currentClass := st.CurrentScope.Class

		if attr, exists := st.LookupAttribute(currentClass, name); exists {

			return attr, true
		}
	}

	return nil, false
}

// lookupInherited searches for a symbol in parent classes
func (st *SymbolTable) lookupInherited(className, symbolName string) (*Symbol, bool) {
	current := className
	visited := make(map[string]bool) // Prevent infinite loops in case of cycles

	for current != "" && current != "Object" && !visited[current] {
		visited[current] = true

		// Get the class symbol
		classSymbol, exists := st.Classes[current]
		if !exists {
			return nil, false
		}

		// Check if symbol exists in this class
		if symbol, exists := classSymbol.Features[symbolName]; exists {
			return symbol, true
		}

		// Move up to parent class
		current = st.Inheritance.Edges[current]
	}

	// Check Object class last
	if objectClass, exists := st.BasicClasses["Object"]; exists {
		if symbol, exists := objectClass.Features[symbolName]; exists {
			return symbol, true
		}
	}

	return nil, false
}

// LookupMethod looks up a method in a specific class and its parent classes
func (st *SymbolTable) LookupMethod(className, methodName string) (*Symbol, bool) {

	// If className is SELF_TYPE, we need to look in Object first for built-in methods
	if className == "SELF_TYPE" {
		if objectClass, exists := st.BasicClasses["Object"]; exists {
			if method, exists := objectClass.Features[methodName]; exists && method.Kind == SymbolMethod {
				return method, true
			}
		}
	}

	// Resolve SELF_TYPE to actual class name for lookup
	actualClassName := className
	if className == "SELF_TYPE" {
		// For SELF_TYPE, we need to look in the current class and its inheritance chain
		for _, classSymbol := range st.Classes {
			if method, exists := classSymbol.Features[methodName]; exists && method.Kind == SymbolMethod {
				return method, true
			}
		}
	}

	// Look in the actual class
	if class, exists := st.Classes[actualClassName]; exists {
		if method, exists := class.Features[methodName]; exists && method.Kind == SymbolMethod {
			return method, true
		}
	}

	// Look in basic classes
	if class, exists := st.BasicClasses[actualClassName]; exists {
		if method, exists := class.Features[methodName]; exists && method.Kind == SymbolMethod {
			return method, true
		}
	}

	// Check inheritance chain
	current := actualClassName
	for current != "" {
		if classSymbol, exists := st.Classes[current]; exists {
			if method, exists := classSymbol.Features[methodName]; exists && method.Kind == SymbolMethod {
				return method, true
			}
			current = classSymbol.Parent
		} else if basicClass, exists := st.BasicClasses[current]; exists {
			if method, exists := basicClass.Features[methodName]; exists && method.Kind == SymbolMethod {
				return method, true
			}
			current = basicClass.Parent
		} else {
			break
		}
	}

	//  check Object class for built-in methods
	if objectClass, exists := st.BasicClasses["Object"]; exists {
		if method, exists := objectClass.Features[methodName]; exists && method.Kind == SymbolMethod {
			return method, true
		}
	}

	return nil, false
}

// LookupAttribute looks up an attribute in a specific class and its parent classes
func (st *SymbolTable) LookupAttribute(className, attrName string) (*Symbol, bool) {
	symbol, exists := st.lookupInherited(className, attrName)
	if !exists {
		return nil, false
	}

	if symbol.Kind != SymbolAttribute {
		return nil, false
	}

	return symbol, true
}

// GetClassFeatures returns all features (methods and attributes) available to a class
// including inherited ones
func (st *SymbolTable) GetClassFeatures(className string) map[string]*Symbol {
	features := make(map[string]*Symbol)
	order := make([]string, 0)

	// Collect features from inheritance chain
	current := className
	for current != "" {
		if classSymbol, exists := st.Classes[current]; exists {
			for name := range classSymbol.Features {
				if _, exists := features[name]; !exists {
					features[name] = classSymbol.Features[name]
					order = append(order, name)
				}
			}
		}
		current = st.Inheritance.Edges[current]
	}

	return features
}

// AddMethod adds and validates a method in the current class
func (st *SymbolTable) AddMethod(className string, method *ast.Method) error {

	// Get the class symbol
	classSymbol, exists := st.Classes[className]
	if !exists {
		return fmt.Errorf("internal error: class %s not found", className)
	}

	// Check for duplicate parameter names
	paramNames := make(map[string]bool)
	for _, param := range method.Parameters {
		if param.Name.Value == "self" {
			return fmt.Errorf("line %d:%d: 'self' cannot be used as a parameter name",
				param.Name.Token.Line, param.Name.Token.Column)
		}

		if paramNames[param.Name.Value] {
			return fmt.Errorf("line %d:%d: duplicate parameter name '%s' in method '%s'",
				param.Name.Token.Line, param.Name.Token.Column, param.Name.Value, method.Name.Value)
		}
		paramNames[param.Name.Value] = true

		// Validate parameter type exists
		if !st.isValidType(param.Type.Value) {
			return fmt.Errorf("line %d:%d: undefined type '%s' for parameter '%s'",
				param.Type.Token.Line, param.Type.Token.Column, param.Type.Value, param.Name.Value)
		}
	}

	// Create method symbol
	methodSym := &Symbol{
		Name:       method.Name.Value,
		Kind:       SymbolMethod,
		ReturnType: method.ReturnType.Value,
		Parameters: method.Parameters,
		Token:      method.Token,
	}

	// Check if method exists in parent classes
	parentClass := classSymbol.Parent
	for parentClass != "" {
		if parentMethod, exists := st.LookupMethod(parentClass, method.Name.Value); exists {
			// Check parameter types match exactly
			if len(parentMethod.Parameters) != len(method.Parameters) {
				return fmt.Errorf("line %d:%d: wrong number of parameters in overridden method '%s': expected %d, got %d",
					method.Token.Line, method.Token.Column, method.Name.Value,
					len(parentMethod.Parameters), len(method.Parameters))
			}

			for i, param := range parentMethod.Parameters {
				if param.Type.Value != method.Parameters[i].Type.Value {
					return fmt.Errorf("line %d:%d: parameter #%d type mismatch in overridden method '%s': expected '%s', got '%s'",
						method.Parameters[i].Type.Token.Line, method.Parameters[i].Type.Token.Column,
						i+1, method.Name.Value, param.Type.Value, method.Parameters[i].Type.Value)
				}
			}

			// Check return type conforms (covariant)
			if !st.IsConformingType(method.ReturnType.Value, parentMethod.ReturnType, className) {
				return fmt.Errorf("line %d:%d: invalid override in class '%s': return type '%s' does not conform to parent's return type '%s'",
					method.ReturnType.Token.Line, method.ReturnType.Token.Column,
					className, method.ReturnType.Value, parentMethod.ReturnType)
			}
			break
		}
		parentClass = st.Classes[parentClass].Parent
	}

	// Add method to class features
	classSymbol.Features[method.Name.Value] = methodSym
	return nil
}

// AddAttribute adds and validates an attribute in the current class
func (st *SymbolTable) AddAttribute(className string, attr *ast.Attribute) error {

	// Get the class symbol first
	classSymbol, exists := st.Classes[className]
	if !exists {
		return fmt.Errorf("internal error: class %s not found", className)
	}

	// Check if attribute name is 'self'
	if attr.Name.Value == "self" {
		return fmt.Errorf("attribute cannot be named 'self' in class %s", className)
	}

	// Check if attribute is already defined in this class
	if _, exists := classSymbol.Features[attr.Name.Value]; exists {
		return fmt.Errorf("attribute %s is redefined in class %s", attr.Name.Value, className)
	}

	// Check if attribute type exists
	if !st.isValidType(attr.Type.Value) {
		return fmt.Errorf("undefined type %s for attribute %s in class %s",
			attr.Type.Value, attr.Name.Value, className)
	}
	if attr.Init != nil {
		initType := st.GetExpressionType(attr.Init, className)
		resolvedAttrType := st.ResolveSelfType(attr.Type.Value, className)

		if !st.IsConformingType(initType, resolvedAttrType, className) {
			return fmt.Errorf("initializer for attribute %s has type %s, which does not conform to declared type %s in line %d ",
				attr.Name.Value, initType, resolvedAttrType, attr.Token.Line)
		}
	}
	// Check for attribute redefinition in inheritance chain
	if _, exists := st.findAttributeInParents(className, attr.Name.Value); exists {
		return fmt.Errorf("attribute %s is redefined in class %s (line %d)", attr.Name.Value, className, attr.Token.Line)
	}

	// Create attribute symbol
	attrSymbol := &Symbol{
		Name:          attr.Name.Value,
		Kind:          SymbolAttribute,
		Type:          attr.Type.Value,
		DefiningClass: className,
		Token:         attr.Token,
		IsInitialized: attr.Init != nil,
		InitExpr:      attr.Init,
	}

	// Add attribute to class features
	classSymbol.Features[attr.Name.Value] = attrSymbol
	return nil
}

func (st *SymbolTable) findAttributeInParents(className, attrName string) (*Symbol, bool) {
	current := st.Inheritance.Edges[className] // Start with parent
	for current != "" && current != "Object" {
		if classSymbol, exists := st.Classes[current]; exists {
			if attr, exists := classSymbol.Features[attrName]; exists && attr.Kind == SymbolAttribute {
				return attr, true
			}
		}
		current = st.Inheritance.Edges[current]
	}

	// Check Object class
	if objectClass, exists := st.BasicClasses["Object"]; exists {
		if attr, exists := objectClass.Features[attrName]; exists && attr.Kind == SymbolAttribute {
			return attr, true
		}
	}

	return nil, false
}

func (st *SymbolTable) isValidType(typeName string) bool {
	// SELF_TYPE is always valid
	if typeName == "SELF_TYPE" {
		return true
	}

	// Check if it's a defined class
	if _, exists := st.Classes[typeName]; exists {
		return true
	}

	// Check if it's a basic class
	if _, exists := st.BasicClasses[typeName]; exists {
		return true
	}

	return false
}

// Type conformance and SELF_TYPE utilities

// IsConformingType checks if type1 conforms to type2 following Cool's type rules
func (st *SymbolTable) IsConformingType(type1, type2 string, currentClass string) bool {

	// Handle SELF_TYPE cases
	if type1 == "SELF_TYPE" && type2 == "SELF_TYPE" {
		return true
	}
	if type1 == "SELF_TYPE" {
		type1 = currentClass
		// SELF_TYPE conforms to the declared type if the current class conforms to it
	}
	if type2 == "SELF_TYPE" {
		return false // No type except SELF_TYPE conforms to SELF_TYPE
	}

	// A type conforms to itself
	if type1 == type2 {
		return true
	}

	// Check conformance through inheritance chain
	current := type1
	visited := make(map[string]bool)

	for current != "" && !visited[current] {
		visited[current] = true
		if current == type2 {
			return true
		}
		current = st.Inheritance.Edges[current]
	}

	return false
}

// GetLeastUpperBound finds the closest common ancestor of two types
func (st *SymbolTable) GetLeastUpperBound(type1, type2 string, currentClass string) string {

	// Handle SELF_TYPE cases
	if type1 == "SELF_TYPE" && type2 == "SELF_TYPE" {
		return "SELF_TYPE"
	}
	if type1 == "SELF_TYPE" {
		type1 = currentClass
	}
	if type2 == "SELF_TYPE" {
		type2 = currentClass
	}

	// Get ancestors of type1
	ancestors1 := make(map[string]bool)
	current := type1
	for current != "" {
		ancestors1[current] = true
		current = st.Inheritance.Edges[current]
	}

	// Find first common ancestor walking up type2's hierarchy
	current = type2
	for current != "" {
		if ancestors1[current] {
			return current
		}
		current = st.Inheritance.Edges[current]
	}

	return "Object" // Default case
}

// ResolveSelfType resolves SELF_TYPE to actual type based on context
func (st *SymbolTable) ResolveSelfType(typeName string, currentClass string) string {
	if typeName == "SELF_TYPE" {
		return currentClass
	}
	return typeName
}

// GetExpressionType determines the static type of an expression, handling SELF_TYPE
func (st *SymbolTable) GetExpressionType(expr ast.Expression, currentClass string) string {

	result := func() string {
		switch e := expr.(type) {

		case *ast.IntegerLiteral:
			return "Int"
		case *ast.StringLiteral:
			return "String"
		case *ast.BooleanLiteral:
			return "Bool"
		case *ast.MethodCall:
			var objectType string
			if e.Object != nil {
				objectType = st.GetExpressionType(e.Object, currentClass)
			} else {
				objectType = "SELF_TYPE"
			}

			method, exists := st.LookupMethod(objectType, e.Method.Value)
			if !exists {
				st.Errors = append(st.Errors,
					fmt.Sprintf("line %d:%d: undefined method '%s' called on object of type %s",
						e.Method.Token.Line, e.Method.Token.Column, e.Method.Value, objectType))
				return "Object"
			}

			// If method returns SELF_TYPE, it should be the type of the expression we're dispatching on
			if method.ReturnType == "SELF_TYPE" {
				if objectType == "SELF_TYPE" {
					return currentClass
				}
				return objectType
			}

			return method.ReturnType
		case *ast.FunctionCall:

			// Try to find the method in the current class or its ancestors
			method, exists := st.LookupMethod(currentClass, e.Function.Value)
			if !exists {
				// Look specifically in IO class for common methods
				if e.Function.Value == "out_string" || e.Function.Value == "out_int" ||
					e.Function.Value == "in_string" || e.Function.Value == "in_int" {
					if method, exists := st.LookupMethod("IO", e.Function.Value); exists {
						if method.ReturnType == "SELF_TYPE" {
							return currentClass
						}
						return method.ReturnType
					}
				}

				st.Errors = append(st.Errors,
					fmt.Sprintf("undefined method '%s' called in class %s",
						e.Function.Value, currentClass))
				return "Object" // Default return type for error
			}

			// Check if the method exists but argument count doesn't match
			if len(e.Arguments) != len(method.Parameters) {
				st.Errors = append(st.Errors,
					fmt.Sprintf("wrong number of arguments for method %s: expected %d, got %d",
						e.Function.Value, len(method.Parameters), len(e.Arguments)))
			}

			// Check argument types
			for i, arg := range e.Arguments {
				if i < len(method.Parameters) {
					argType := st.GetExpressionType(arg, currentClass)
					paramType := method.Parameters[i].Type.Value

					if !st.IsConformingType(argType, paramType, currentClass) {
						st.Errors = append(st.Errors,
							fmt.Sprintf("argument #%d type %s does not conform to parameter type %s",
								i+1, argType, paramType))
					}
				}
			}

			// If method returns SELF_TYPE, it's the current class
			if method.ReturnType == "SELF_TYPE" {
				return currentClass
			}
			return method.ReturnType
		case *ast.Assignment:

			// First check if the variable being assigned to exists
			var targetType string
			var targetExists bool

			// Special case for 'self'
			if e.Name.Value == "self" {
				st.Errors = append(st.Errors, fmt.Sprintf("line %d:%d: cannot assign to 'self'",
					e.Name.Token.Line, e.Name.Token.Column))
				return "SELF_TYPE" // Return SELF_TYPE but with error
			}

			// Check in current scope and parent scopes
			if symbol, exists := st.LookupSymbol(e.Name.Value); exists {
				targetType = symbol.Type
				targetExists = true
			} else if attr, exists := st.LookupAttribute(currentClass, e.Name.Value); exists {
				// Check if it's a class attribute
				targetType = attr.Type
				targetExists = true
			} else {
				// Variable is undefined
				st.Errors = append(st.Errors, fmt.Sprintf("line %d:%d: undefined variable '%s'",
					e.Name.Token.Line, e.Name.Token.Column, e.Name.Value))
				return "Object" // Default return type for error
			}

			// Get the type of the expression being assigned
			exprType := st.GetExpressionType(e.Expression, currentClass)
			// Check type conformance
			if targetExists && !st.IsConformingType(exprType, targetType, currentClass) {
				st.Errors = append(st.Errors, fmt.Sprintf("line %d:%d: type '%s' of assigned expression does not conform to type '%s' of identifier '%s'",
					e.Token.Line, e.Token.Column,
					exprType, targetType, e.Name.Value))
			}

			return targetType
		case *ast.BlockExpression:
			if len(e.Expressions) == 0 {
				// Empty block defaults to Object
				return "Object"
			}

			// Process all expressions in the block
			var lastType string
			for _, expr := range e.Expressions {
				lastType = st.GetExpressionType(expr, currentClass)
			}

			return lastType
		case *ast.NotExpression:
			exprType := st.GetExpressionType(e.Expression, currentClass)

			if exprType != "Bool" {
				st.Errors = append(st.Errors, fmt.Sprintf("line %d:%d: 'not' operator requires Bool operand, got %s",
					e.Token.Line, e.Token.Column, exprType))
			}

			return "Bool"
		case *ast.ObjectIdentifier:
			if e.Value == "self" {
				return "SELF_TYPE"
			}

			// First try looking up in current scope and parent scopes
			if symbol, exists := st.LookupSymbol(e.Value); exists {
				return symbol.Type
			}

			// Then try looking up as a class attribute
			if attr, exists := st.LookupAttribute(currentClass, e.Value); exists {
				return attr.Type
			}

			// If we get here, the identifier is undefined
			errMsg := fmt.Sprintf("line %d:%d: undefined identifier '%s'",
				e.Token.Line, e.Token.Column, e.Value)
			st.Errors = append(st.Errors, errMsg)
			return "Object"
		case *ast.BinaryExpression:
			leftType := st.GetExpressionType(e.Left, currentClass)
			rightType := st.GetExpressionType(e.Right, currentClass)

			// For arithmetic operations
			if e.Operator == "+" || e.Operator == "-" || e.Operator == "*" || e.Operator == "/" {
				if leftType != "Int" || rightType != "Int" {
					st.Errors = append(st.Errors, fmt.Sprintf("Arithmetic operation %s requires Int operands", e.Operator))
				}
				return "Int"
			}

			// For comparisons
			if e.Operator == "<" || e.Operator == "<=" || e.Operator == "=" {
				if e.Operator != "=" && (leftType != "Int" || rightType != "Int") {
					st.Errors = append(st.Errors, fmt.Sprintf("Comparison %s requires Int operands", e.Operator))
				}
				return "Bool"
			}

			return "Object"
		case *ast.IsVoidExpression:
			return "Bool" // isvoid always returns Bool
		case *ast.NewExpression:
			if e.Type.Value == "SELF_TYPE" {
				return currentClass
			}
			// Verify the type exists
			if !st.isValidType(e.Type.Value) {
				st.Errors = append(st.Errors, fmt.Sprintf("undefined type %s in new expression",
					e.Type.Value))
				return "Object"
			}
			return e.Type.Value
		case *ast.CaseExpression:
			exprType := st.GetExpressionType(e.Expression, currentClass)

			// Debug print for cases
			for _, branch := range e.Cases {

				// Check if branch type conforms to expression type
				if !st.IsConformingType(exprType, branch.Type.Value, currentClass) {
					errMsg := fmt.Sprintf("line %d:%d: case branch type %s cannot handle expression of type %s",
						branch.Type.Token.Line,
						branch.Type.Token.Column,
						branch.Type.Value,
						exprType)
					st.Errors = append(st.Errors, errMsg)
				}
			}

			// Get LUB of all branch types
			branchTypes := make([]string, len(e.Cases))
			for i, branch := range e.Cases {
				branchTypes[i] = st.GetExpressionType(branch.Expression, currentClass)
			}

			resultType := branchTypes[0]
			for _, t := range branchTypes[1:] {
				resultType = st.GetLeastUpperBound(resultType, t, currentClass)
			}
			return resultType
		case *ast.IfExpression:
			condType := st.GetExpressionType(e.Condition, currentClass)

			if condType != "Bool" {
				st.Errors = append(st.Errors, fmt.Sprintf("if condition must be Bool, got %s", condType))
			}

			thenType := st.GetExpressionType(e.Consequence, currentClass)
			elseType := st.GetExpressionType(e.Alternative, currentClass)

			// The type of an if expression is the LUB of its branches
			return st.GetLeastUpperBound(thenType, elseType, currentClass)
		case *ast.InfixExpression:
			leftType := st.GetExpressionType(e.Left, currentClass)
			rightType := st.GetExpressionType(e.Right, currentClass)

			// For arithmetic operations
			if e.Operator == "+" || e.Operator == "-" || e.Operator == "*" || e.Operator == "/" {
				if leftType != "Int" || rightType != "Int" {
					st.Errors = append(st.Errors, fmt.Sprintf("Arithmetic operation %s requires Int operands", e.Operator))
				}
				return "Int"
			}

			// For comparisons
			if e.Operator == "<" || e.Operator == "<=" || e.Operator == "=" {
				if e.Operator != "=" && (leftType != "Int" || rightType != "Int") {
					st.Errors = append(st.Errors, fmt.Sprintf("Comparison %s requires Int operands", e.Operator))
				}
				return "Bool"
			}

			return "Object"
		case *ast.LetExpression:
			// Create new scope for let bindings
			st.EnterScope(SymbolLocal, currentClass)
			defer st.ExitScope()
		
			// Collect all bindings
			var allBindings []*ast.Binding
			if e.Name != nil {
				// Handle the first binding if present
				allBindings = append(allBindings, &ast.Binding{
					Name: e.Name,
					Type: e.Type,
					Init: e.Init,
				})
			}
			allBindings = append(allBindings, e.Bindings...)
		
			for _, binding := range allBindings {
				// Process initialization expression if present
				var initType string
				if binding.Init != nil {
					initType = st.GetExpressionType(binding.Init, currentClass)
					// Check conformance
					if !st.IsConformingType(initType, binding.Type.Value, currentClass) {
						st.Errors = append(st.Errors, fmt.Sprintf("line %d:%d: let initializer type '%s' does not conform to declared type '%s'",
							binding.Name.Token.Line, binding.Name.Token.Column, initType, binding.Type.Value))
					}
				}
		
				// Add the variable to the current scope
				st.CurrentScope.Symbols[binding.Name.Value] = &Symbol{
					Name:  binding.Name.Value,
					Kind:  SymbolLocal,
					Type:  binding.Type.Value,
					Token: binding.Name.Token,
				}
			}
		
			// Process the body of the let expression
			bodyType := st.GetExpressionType(e.Body, currentClass)
			return bodyType
		}

		return "Object" // Default case
	}()

	return result
}

// GetMethodCallType handles type resolution for method calls, including SELF_TYPE
func (st *SymbolTable) GetMethodCallType(call *ast.MethodCall, currentClass string) string {
	// Get the type of the object being dispatched on
	var dispatchType string
	if call.Object != nil {
		dispatchType = st.GetExpressionType(call.Object, currentClass)
	} else {
		dispatchType = "SELF_TYPE" // Implicit self dispatch
	}

	// Handle static dispatch (@Type)
	if call.Type != nil {
		dispatchType = call.Type.Value
	}

	// Resolve SELF_TYPE in dispatch type
	actualDispatchType := st.ResolveSelfType(dispatchType, currentClass)

	// Look up the method
	method, exists := st.LookupMethod(actualDispatchType, call.Method.Value)
	if !exists {
		return "Object"
	}

	// If method returns SELF_TYPE, it's the type of the dispatch expression
	if method.ReturnType == "SELF_TYPE" {
		return dispatchType // Preserve SELF_TYPE if that's what we started with
	}

	return method.ReturnType
}

// GetAssignmentType returns the static type of an assignment expression
func (st *SymbolTable) GetAssignmentType(assign *ast.Assignment, currentClass string) string {
	// The type of an assignment is the type of the variable being assigned to
	symbol, exists := st.LookupSymbol(assign.Name.Value)
	if !exists {
		return "Object"
	}
	return symbol.Type
}

// Type conformance checking utilities

// ValidateAssignmentTypes checks if a value of sourceType can be assigned to a variable of targetType
func (st *SymbolTable) ValidateAssignmentTypes(sourceType, targetType string, currentClass string) error {
	if !st.IsConformingType(sourceType, targetType, currentClass) {
		return fmt.Errorf("type %s does not conform to type %s", sourceType, targetType)
	}
	return nil
}

// ValidateMethodCallTypes checks type conformance for method arguments
func (st *SymbolTable) ValidateMethodCallTypes(method *Symbol, args []ast.Expression, currentClass string) error {
	if len(args) != len(method.Parameters) {
		return fmt.Errorf("wrong number of arguments: expected %d, got %d",
			len(method.Parameters), len(args))
	}

	for i, arg := range args {
		argType := st.GetExpressionType(arg, currentClass)
		paramType := method.Parameters[i].Type.Value

		if !st.IsConformingType(argType, paramType, currentClass) {
			return fmt.Errorf("argument %d type %s does not conform to parameter type %s",
				i+1, argType, paramType)
		}
	}

	return nil
}

// ValidateIfCondition checks if an expression can be used as an if condition
func (st *SymbolTable) ValidateIfCondition(condition ast.Expression, currentClass string) error {
	condType := st.GetExpressionType(condition, currentClass)
	if condType != "Bool" {
		return fmt.Errorf("if condition must be Bool, got %s", condType)
	}
	return nil
}

// ValidateWhileCondition checks if an expression can be used as a while condition
func (st *SymbolTable) ValidateWhileCondition(condition ast.Expression, currentClass string) error {
	condType := st.GetExpressionType(condition, currentClass)
	if condType != "Bool" {
		return fmt.Errorf("while condition must be Bool, got %s", condType)
	}
	return nil
}

// ValidateCaseBranches checks for case branch type validity and exhaustiveness
func (st *SymbolTable) ValidateCaseBranches(branches []*ast.Case, currentClass string) error {
	seenTypes := make(map[string]bool)
	if len(branches) == 0 {
		return fmt.Errorf("case expression must have at least one branch")
	}

	hasObjectBranch := false
	for _, branch := range branches {
		if branch.Type.Value == "Object" {
			hasObjectBranch = true
		}
	}

	if !hasObjectBranch {
		return fmt.Errorf("case expression should have Object branch as default")
	}

	for _, branch := range branches {
		// Check for duplicate types
		if seenTypes[branch.Type.Value] {
			return fmt.Errorf("duplicate case branch type: %s", branch.Type.Value)
		}
		seenTypes[branch.Type.Value] = true

		// Validate the type exists
		if !st.isValidType(branch.Type.Value) {
			return fmt.Errorf("undefined type in case branch: %s", branch.Type.Value)
		}

		// SELF_TYPE is not allowed in case branches
		if branch.Type.Value == "SELF_TYPE" {
			return fmt.Errorf("SELF_TYPE cannot be used in case branch")
		}
	}

	return nil
}

// GetCaseExpressionType determines the type of a case expression
func (st *SymbolTable) GetCaseExpressionType(expr *ast.CaseExpression, currentClass string) string {
	if len(expr.Cases) == 0 {
		return "Object"
	}

	// Get type of first branch
	branchType := st.GetExpressionType(expr.Cases[0].Expression, currentClass)

	// Find LUB with all other branch types
	for _, branch := range expr.Cases[1:] {
		bodyType := st.GetExpressionType(branch.Expression, currentClass)
		branchType = st.GetLeastUpperBound(branchType, bodyType, currentClass)
	}

	return branchType
}
