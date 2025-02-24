package codegen

import (
	"cool-compiler/ast"
	"cool-compiler/lexer"
	"fmt"
	"strings"
	"unicode"

	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/enum"
	"github.com/llir/llvm/ir/types"
	"github.com/llir/llvm/ir/value"
)

type CodeGenerator struct {
	module     *ir.Module
	classTypes map[string]*types.StructType
	vtables    map[string]*ir.Global
	methods    map[string]*ir.Func
	locals     map[string]value.Value
	// NEW: record the declared (static) type for let–bound variables that are objects.
	localsTypes  map[string]string
	currentClass string              // Track current class during code generation
	classAttrs   map[string][]string // Track class attributes
	classTable   ClassTable
	program      *ast.Program // Add this field
}

func NewCodeGenerator() *CodeGenerator {
	return &CodeGenerator{
		module:      ir.NewModule(),
		classTypes:  make(map[string]*types.StructType),
		vtables:     make(map[string]*ir.Global),
		methods:     make(map[string]*ir.Func),
		locals:      make(map[string]value.Value),
		localsTypes: make(map[string]string),
		classTable:  make(ClassTable), // Initialize the class table map
	}
}

func (g *CodeGenerator) Generate(program *ast.Program) (*ir.Module, error) {
	g.program = program // Store the program
	// 1. Declare classes (collect local attributes and methods).
	g.addBuiltInClasses(program)

	if err := g.DeclareClasses(program.Classes); err != nil {
		return nil, err
	}

	// 2. Build the inheritance table (which merges inherited attributes).
	if err := g.BuildClassTable(program.Classes); err != nil {
		return nil, err
	}

	// 3. Compute object layouts (attribute offsets and total size).
	if err := g.ComputeObjectLayouts(); err != nil {
		return nil, err
	}

	// 4. Now, build the LLVM struct types from the full class table.
	if err := g.BuildClassTypesFromTable(); err != nil {
		return nil, err
	}

	// 5. Construct vtables.
	if err := g.ConstructVTables(); err != nil {
		return nil, err
	}

	// 6. Generate method implementations.
	for _, class := range program.Classes {
		if err := g.generateClass(class); err != nil {
			return nil, err
		}
	}

	// 7. Generate program entry point.
	if err := g.generateProgramEntryPoint(); err != nil {
		return nil, err
	}

	return g.module, nil
}

func (g *CodeGenerator) DeclareClasses(classes []*ast.Class) error {
	// Create Object type first.
	objectType := types.NewStruct(
		types.NewPointer(types.I8), // vtable pointer as i8*
	)
	g.classTypes["Object"] = objectType

	// Initialize maps.
	g.classAttrs = make(map[string][]string)
	classAttributes := make(map[string][]*ast.Attribute)

	// Collect attribute names and attribute nodes.
	for _, class := range classes {
		var attrNames []string
		var attrs []*ast.Attribute
		for _, feature := range class.Features {
			if attr, ok := feature.(*ast.Attribute); ok {
				attrNames = append(attrNames, attr.Name.Value)
				attrs = append(attrs, attr)
			}
		}
		g.classAttrs[class.Name.Value] = attrNames
		classAttributes[class.Name.Value] = attrs
	}

	// Create LLVM struct types for each class.
	for _, class := range classes {
		// Start with the vtable pointer field.
		fields := []types.Type{types.NewPointer(types.I8)}

		// For each attribute declared in this class, select its LLVM type based on its declared type.
		attrs := classAttributes[class.Name.Value]
		for _, attr := range attrs {
			var fieldType types.Type
			switch strings.ToLower(attr.Type.Value) {
			case "int":
				fieldType = types.I32
			case "bool":
				fieldType = types.I1
			default:
				// For user-defined types, use pointer to their struct type
				if classType, exists := g.classTypes[attr.Type.Value]; exists {
					fieldType = types.NewPointer(classType)
				} else {
					fieldType = types.NewPointer(types.I8) // Fallback
				}
			}
			fields = append(fields, fieldType)
		}
		classType := types.NewStruct(fields...)
		g.classTypes[class.Name.Value] = classType
	}

	// Declare methods remains unchanged.
	for _, class := range classes {
		for _, feature := range class.Features {
			if method, ok := feature.(*ast.Method); ok {
				methodName := fmt.Sprintf("%s_%s", class.Name.Value, method.Name.Value)
				var paramTypes []types.Type
				var retType types.Type

				// Handle return types based on declared type
				switch method.ReturnType.Value {
				case "Object":
					retType = types.NewPointer(types.I8)
				case "SELF_TYPE":
					retType = types.NewPointer(types.I8)
				case "Int":
					retType = types.I32
				case "Bool":
					retType = types.I1
				default:
					retType = types.NewPointer(types.I8)
				}

				// First parameter is always self
				paramTypes = append(paramTypes, types.NewPointer(types.I8))

				// Add other parameters
				for _, param := range method.Parameters {
					switch param.Type.Value {
					case "Int":
						paramTypes = append(paramTypes, types.I32)
					case "Bool":
						paramTypes = append(paramTypes, types.I1)
					default:
						paramTypes = append(paramTypes, types.NewPointer(types.I8))
					}
				}

				// Create function with proper signature
				fn := g.module.NewFunc(methodName, retType)
				// Add parameters
				params := make([]*ir.Param, len(paramTypes))
				params[0] = ir.NewParam("self", paramTypes[0])
				for i := 1; i < len(paramTypes); i++ {
					params[i] = ir.NewParam(fmt.Sprintf("p%d", i), paramTypes[i])
				}
				fn.Params = params
				g.methods[methodName] = fn
			}
		}
	}
	return nil
}

// initializeVTables initializes the vtables for all classes
func (g *CodeGenerator) initializeVTables(classes []*ast.Class) error {
	for _, class := range classes {
		// Count methods for vtable size
		methodCount := 0
		for _, feature := range class.Features {
			if _, ok := feature.(*ast.Method); ok {
				methodCount++
			}
		}

		// Create string constants for class names
		className := g.getOrCreateStringConstant(class.Name.Value)
		parentClassName := g.getOrCreateStringConstant("Object")

		// Create vtable type with correct size
		vtableType := types.NewStruct(
			types.NewPointer(types.I8),                                      // type name
			types.NewPointer(types.I8),                                      // parent class name
			types.NewArray(uint64(methodCount), types.NewPointer(types.I8)), // method array
		)

		// Create method list
		methodList := make([]constant.Constant, 0, methodCount)
		for _, feature := range class.Features {
			if method, ok := feature.(*ast.Method); ok {
				methodName := fmt.Sprintf("%s_%s", class.Name.Value, method.Name.Value)
				fn := g.methods[methodName]
				// Cast function to i8* for vtable
				methodPtr := constant.NewBitCast(fn, types.NewPointer(types.I8))
				methodList = append(methodList, methodPtr)
			}
		}

		// Create vtable initializer
		vtableInit := constant.NewStruct(vtableType,
			constant.NewBitCast(className, types.NewPointer(types.I8)),
			constant.NewBitCast(parentClassName, types.NewPointer(types.I8)),
			constant.NewArray(types.NewArray(uint64(methodCount), types.NewPointer(types.I8)),
				methodList...),
		)

		// Create vtable global
		vtable := g.module.NewGlobalDef(
			fmt.Sprintf("%s_vtable", class.Name.Value),
			vtableInit,
		)
		g.vtables[class.Name.Value] = vtable
	}
	return nil
}

// generateClass handles code generation for a single class
func (g *CodeGenerator) generateClass(class *ast.Class) error {
	g.currentClass = class.Name.Value
	defer func() { g.currentClass = "" }()

	// Rest of generateClass remains...
	for _, feature := range class.Features {
		if method, ok := feature.(*ast.Method); ok {
			if err := g.generateMethod(class.Name.Value, method); err != nil {
				return err
			}
		}
	}
	return nil
}

func (g *CodeGenerator) generateProgramEntryPoint() error {
	mainFn := g.module.NewFunc("main", types.I32)
	block := mainFn.NewBlock("entry")

	// Get Main class info and AST node
	mainClassInfo, ok := g.classTable["Main"]
	if !ok {
		return fmt.Errorf("Main class not found")
	}
	var mainClassAST *ast.Class
	for _, class := range g.program.Classes {
		if class.Name.Value == "Main" {
			mainClassAST = class
			break
		}
	}

	// Allocate Main object
	mainType := g.classTypes["Main"]
	mainObj := block.NewAlloca(mainType)

	// Initialize vtable pointer
	vtablePtr := block.NewGetElementPtr(mainType, mainObj,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0),
	)
	block.NewStore(
		block.NewBitCast(g.vtables["Main"], types.NewPointer(types.I8)),
		vtablePtr,
	)

	// Initialize attributes using AST initializers
	for _, attrInfo := range mainClassInfo.Attributes {
		// Find matching attribute in AST
		var attrAST *ast.Attribute
		for _, feature := range mainClassAST.Features {
			if a, ok := feature.(*ast.Attribute); ok && a.Name.Value == attrInfo.Name {
				attrAST = a
				break
			}
		}

		// Get pointer to attribute
		attrPtr := block.NewGetElementPtr(mainType, mainObj,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, int64(attrInfo.Offset)),
		)

		// Initialize the attribute
		var err error
		var initExpr ast.Expression
		if attrAST != nil {
			initExpr = attrAST.Init
		}

		block, err = g.initializeAttribute(block, attrPtr, attrInfo.Type, initExpr)
		if err != nil {
			return fmt.Errorf("error initializing attribute %s: %v", attrInfo.Name, err)
		}
	}

	// Call main method
	mainObjCast := block.NewBitCast(mainObj, types.NewPointer(types.I8))
	mainMethod := g.methods["Main_main"]
	block.NewCall(mainMethod, mainObjCast)

	block.NewRet(constant.NewInt(types.I32, 0))
	return nil
}

func (g *CodeGenerator) generateMethod(className string, method *ast.Method) error {
	fmt.Printf("DEBUG: Generating method %s for class %s\n", method.Name.Value, className)
	fmt.Printf("DEBUG: Method body type: %T\n", method.Body)

	// Reset local variables for this method
	g.locals = make(map[string]value.Value)
	g.localsTypes = make(map[string]string)

	// Rest of the method generation code remains unchanged...
	methodName := fmt.Sprintf("%s_%s", className, method.Name.Value)
	fn, ok := g.methods[methodName]
	if !ok {
		return fmt.Errorf("method %s not found", methodName)
	}

	entryBlock := fn.NewBlock("entry")
	fmt.Printf("DEBUG: Created entry block for method %s\n", methodName)
	if method.Name.Value == "type_name" {
		// Get self parameter
		self := fn.Params[0]

		// Get the vtable pointer
		classType := g.classTypes[className]
		selfPtr := entryBlock.NewBitCast(self, types.NewPointer(classType))
		vtablePtrPtr := entryBlock.NewGetElementPtr(classType, selfPtr,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, 0),
		)
		vtablePtr := entryBlock.NewLoad(types.NewPointer(types.I8), vtablePtrPtr)

		// Cast vtable pointer to correct type
		vtableStructType := types.NewStruct(
			types.NewPointer(types.I8),                    // class name
			types.NewPointer(types.I8),                    // parent class name
			types.NewArray(0, types.NewPointer(types.I8)), // methods
		)
		castedVtable := entryBlock.NewBitCast(vtablePtr, types.NewPointer(vtableStructType))

		// Get class name pointer (first field of vtable)
		classNamePtrPtr := entryBlock.NewGetElementPtr(vtableStructType, castedVtable,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, 0),
		)
		classNamePtr := entryBlock.NewLoad(types.NewPointer(types.I8), classNamePtrPtr)

		// Create new String object
		stringType := g.classTypes["String"]
		stringObj := entryBlock.NewAlloca(stringType)

		// Set vtable for String object
		stringVtablePtr := entryBlock.NewGetElementPtr(stringType, stringObj,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, 0),
		)
		entryBlock.NewStore(
			entryBlock.NewBitCast(g.vtables["String"], types.NewPointer(types.I8)),
			stringVtablePtr,
		)

		// Set string value (class name)
		valuePtr := entryBlock.NewGetElementPtr(stringType, stringObj,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, 1),
		)
		entryBlock.NewStore(classNamePtr, valuePtr)

		// Return String object
		result := entryBlock.NewBitCast(stringObj, types.NewPointer(types.I8))
		entryBlock.NewRet(result)
		return nil
	}

	// Handle abort method
	if className == "Object" && method.Name.Value == "abort" {
		var abortFunc *ir.Func
		for _, f := range g.module.Funcs {
			if f.Name() == "abort" {
				abortFunc = f
				break
			}
		}
		if abortFunc == nil {
			abortFunc = g.module.NewFunc("abort", types.Void)
		}
		entryBlock.NewCall(abortFunc)
		entryBlock.NewUnreachable()
		return nil
	}
	// Special case for String built-in methods
	if className == "String" {
		if method.Name.Value == "concat" {
			self := fn.Params[0]
			s := fn.Params[1]

			// Cast self and s to String struct pointers
			stringType := g.classTypes["String"]
			selfPtr := entryBlock.NewBitCast(self, types.NewPointer(stringType))
			sPtr := entryBlock.NewBitCast(s, types.NewPointer(stringType))

			// Access 'value' fields
			selfValuePtr := entryBlock.NewGetElementPtr(stringType, selfPtr, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 1))
			selfValue := entryBlock.NewLoad(types.I8Ptr, selfValuePtr)

			sValuePtr := entryBlock.NewGetElementPtr(stringType, sPtr, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 1))
			sValue := entryBlock.NewLoad(types.I8Ptr, sValuePtr)

			// Declare necessary functions
			var strlen *ir.Func
			for _, f := range g.module.Funcs {
				if f.Name() == "strlen" {
					strlen = f
					break
				}
			}
			if strlen == nil {
				strlen = g.module.NewFunc("strlen", types.I64, ir.NewParam("str", types.I8Ptr))
			}
			var malloc *ir.Func
			for _, f := range g.module.Funcs {
				if f.Name() == "malloc" {
					malloc = f
					break
				}
			}
			if malloc == nil {
				malloc = g.module.NewFunc("malloc", types.I8Ptr, ir.NewParam("size", types.I64))
			}
			strcpy := g.module.NewFunc("strcpy", types.I8Ptr, ir.NewParam("dest", types.I8Ptr), ir.NewParam("src", types.I8Ptr))
			strcat := g.module.NewFunc("strcat", types.I8Ptr, ir.NewParam("dest", types.I8Ptr), ir.NewParam("src", types.I8Ptr))

			// Compute lengths and allocate buffer
			lenSelf := entryBlock.NewCall(strlen, selfValue)
			lenS := entryBlock.NewCall(strlen, sValue)
			totalLen := entryBlock.NewAdd(lenSelf, lenS)
			totalLenPlus1 := entryBlock.NewAdd(totalLen, constant.NewInt(types.I64, 1))

			buffer := entryBlock.NewCall(malloc, totalLenPlus1)
			entryBlock.NewCall(strcpy, buffer, selfValue)
			entryBlock.NewCall(strcat, buffer, sValue)

			// Allocate new String object
			// nullPtr := constant.NewNull(types.NewPointer(stringType))
			// gepIndex := constant.NewInt(types.I32, 1)
			// gep := constant.NewGetElementPtr(stringType, nullPtr, gepIndex)
			stringSize := constant.NewInt(types.I64, 16) // Size of vtable ptr (8) + string ptr (8)
			stringObjMem := entryBlock.NewCall(malloc, stringSize)
			stringObj := entryBlock.NewBitCast(stringObjMem, types.NewPointer(stringType))

			// Set vtable
			vtablePtr := entryBlock.NewGetElementPtr(stringType, stringObj, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
			entryBlock.NewStore(entryBlock.NewBitCast(g.vtables["String"], types.I8Ptr), vtablePtr)

			// Set value field (i8*)
			valuePtr := entryBlock.NewGetElementPtr(stringType, stringObj, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 1))
			entryBlock.NewStore(buffer, valuePtr)

			// Return the new String object
			result := entryBlock.NewBitCast(stringObj, types.I8Ptr)
			entryBlock.NewRet(result)
			return nil
		}
		if method.Name.Value == "substr" {
			self := fn.Params[0]
			iParam := fn.Params[1]
			lParam := fn.Params[2]

			stringType := g.classTypes["String"]
			selfPtr := entryBlock.NewBitCast(self, types.NewPointer(stringType))
			valuePtr := entryBlock.NewGetElementPtr(stringType, selfPtr, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 1))
			originalStr := entryBlock.NewLoad(types.I8Ptr, valuePtr)

			// Declare strlen if not present
			var strlenFunc *ir.Func
			for _, f := range g.module.Funcs {
				if f.Name() == "strlen" {
					strlenFunc = f
					break
				}
			}
			if strlenFunc == nil {
				strlenFunc = g.module.NewFunc("strlen", types.I64, ir.NewParam("str", types.I8Ptr))
			}
			lenOriginal := entryBlock.NewCall(strlenFunc, originalStr)

			// Convert i and l to i64
			i64_i := entryBlock.NewSExt(iParam, types.I64)
			i64_l := entryBlock.NewSExt(lParam, types.I64)

			// Check for invalid indices (i < 0, l < 0, i + l > length)
			cmp_i_neg := entryBlock.NewICmp(enum.IPredSLT, i64_i, constant.NewInt(types.I64, 0))
			cmp_l_neg := entryBlock.NewICmp(enum.IPredSLT, i64_l, constant.NewInt(types.I64, 0))
			i_plus_l := entryBlock.NewAdd(i64_i, i64_l)
			cmp_i_plus_l_gt_len := entryBlock.NewICmp(enum.IPredSGT, i_plus_l, lenOriginal)

			cond1 := entryBlock.NewOr(cmp_i_neg, cmp_l_neg)
			cond := entryBlock.NewOr(cond1, cmp_i_plus_l_gt_len)

			// Create blocks for error handling
			mainBlock := entryBlock.Parent.NewBlock("substr.main")
			errorBlock := entryBlock.Parent.NewBlock("substr.error")
			entryBlock.NewCondBr(cond, errorBlock, mainBlock)

			// Handle error by calling abort
			var abortFunc *ir.Func
			for _, f := range g.module.Funcs {
				if f.Name() == "abort" {
					abortFunc = f
					break
				}
			}
			if abortFunc == nil {
				abortFunc = g.module.NewFunc("abort", types.Void)
			}
			errorBlock.NewCall(abortFunc)
			errorBlock.NewUnreachable()

			// Main block: proceed with substring extraction
			srcPtr := mainBlock.NewGetElementPtr(types.I8, originalStr, i64_i)

			// Allocate memory for new substring
			var mallocFunc *ir.Func
			for _, f := range g.module.Funcs {
				if f.Name() == "malloc" {
					mallocFunc = f
					break
				}
			}
			if mallocFunc == nil {
				mallocFunc = g.module.NewFunc("malloc", types.I8Ptr, ir.NewParam("size", types.I64))
			}
			bufferSize := mainBlock.NewAdd(i64_l, constant.NewInt(types.I64, 1))
			newBuf := mainBlock.NewCall(mallocFunc, bufferSize)

			// Declare memcpy to copy the substring
			var memcpyFunc *ir.Func
			for _, f := range g.module.Funcs {
				if f.Name() == "memcpy" {
					memcpyFunc = f
					break
				}
			}
			if memcpyFunc == nil {
				memcpyFunc = g.module.NewFunc("memcpy", types.I8Ptr,
					ir.NewParam("dest", types.I8Ptr),
					ir.NewParam("src", types.I8Ptr),
					ir.NewParam("len", types.I64))
			}
			mainBlock.NewCall(memcpyFunc, newBuf, srcPtr, i64_l)

			// Null-terminate the substring
			nullTermPtr := mainBlock.NewGetElementPtr(types.I8, newBuf, i64_l)
			mainBlock.NewStore(constant.NewInt(types.I8, 0), nullTermPtr)

			// Create new String object
			stringSize := constant.NewInt(types.I64, 16) // Size of String struct
			stringObjMem := mainBlock.NewCall(mallocFunc, stringSize)
			stringObj := mainBlock.NewBitCast(stringObjMem, types.NewPointer(stringType))

			// Set vtable pointer
			vtablePtr := mainBlock.NewGetElementPtr(stringType, stringObj, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
			mainBlock.NewStore(mainBlock.NewBitCast(g.vtables["String"], types.I8Ptr), vtablePtr)

			// Set value field
			valueFieldPtr := mainBlock.NewGetElementPtr(stringType, stringObj, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 1))
			mainBlock.NewStore(newBuf, valueFieldPtr)

			// Return the new String object
			result := mainBlock.NewBitCast(stringObj, types.I8Ptr)
			mainBlock.NewRet(result)

			return nil
		}
		// Handle length method
		if method.Name.Value == "length" {
			self := fn.Params[0]

			// Cast self to String struct type: {i8*, i8*} (vtable and value)
			stringType := g.classTypes["String"]
			selfPtr := entryBlock.NewBitCast(self, types.NewPointer(stringType))

			// Get the 'value' field (index 1)
			valuePtr := entryBlock.NewGetElementPtr(stringType, selfPtr,
				constant.NewInt(types.I32, 0),
				constant.NewInt(types.I32, 1),
			)
			strPtr := entryBlock.NewLoad(types.NewPointer(types.I8), valuePtr)

			// Call strlen on the string pointer
			strlen := g.module.NewFunc("strlen", types.I64,
				ir.NewParam("str", types.NewPointer(types.I8)))
			lenCall := entryBlock.NewCall(strlen, strPtr)

			// Truncate to Int (i32)
			result := entryBlock.NewTrunc(lenCall, types.I32)
			entryBlock.NewRet(result)
			return nil
		}
	}

	// Special case for IO built-in methods
	if className == "IO" {
		// Handle out_string
		if method.Name.Value == "out_string" {
			self := fn.Params[0]
			str := fn.Params[1]

			var printf *ir.Func
			for _, f := range g.module.Funcs {
				if f.Name() == "printf" {
					printf = f
					break
				}
			}
			if printf == nil {
				printf = g.module.NewFunc("printf", types.I32, ir.NewParam("format", i8Ptr))
				printf.Sig.Variadic = true
			}

			stringType := g.classTypes["String"]
			strPtr := entryBlock.NewBitCast(str, types.NewPointer(stringType))

			// Access the 'value' field (index 1 in struct)
			valuePtr := entryBlock.NewGetElementPtr(stringType, strPtr,
				constant.NewInt(types.I32, 0), // Structure pointer
				constant.NewInt(types.I32, 1), // Second field (value)
			)
			strValue := entryBlock.NewLoad(types.NewPointer(types.I8), valuePtr)

			// Get format string
			formatGlobal := g.getOrCreateStringConstant("%s\n")
			var formatPtr value.Value
			if arrType, ok := formatGlobal.Type().(*types.ArrayType); ok {
				formatPtr = entryBlock.NewGetElementPtr(arrType, formatGlobal,
					constant.NewInt(types.I32, 0),
					constant.NewInt(types.I32, 0))
			} else {
				formatPtr = formatGlobal
			}

			entryBlock.NewCall(printf, formatPtr, strValue)
			entryBlock.NewRet(self)
			return nil
		}

		// Handle out_int
		if method.Name.Value == "out_int" {
			self := fn.Params[0]
			intArg := fn.Params[1]

			var printf *ir.Func
			for _, f := range g.module.Funcs {
				if f.Name() == "printf" {
					printf = f
					break
				}
			}
			if printf == nil {
				printf = g.module.NewFunc("printf", types.I32, ir.NewParam("format", i8Ptr))
				printf.Sig.Variadic = true
			}

			formatGlobal := g.getOrCreateStringConstant("%d\n")
			var formatPtr value.Value
			if arrType, ok := formatGlobal.Type().(*types.ArrayType); ok {
				formatPtr = entryBlock.NewGetElementPtr(arrType, formatGlobal,
					constant.NewInt(types.I32, 0),
					constant.NewInt(types.I32, 0))
			} else {
				formatPtr = formatGlobal
			}

			entryBlock.NewCall(printf, formatPtr, intArg)
			entryBlock.NewRet(self)
			return nil
		}
	}

	// Normal method generation for non built-in methods:
	// Retrieve the self parameter (declared as i8*).
	self := fn.Params[0]
	classType, ok := g.classTypes[className]
	if !ok {
		return fmt.Errorf("class type not found for %s", className)
	}
	typedSelf := entryBlock.NewBitCast(self, types.NewPointer(classType))
	g.locals["$self"] = typedSelf
	g.localsTypes["$self"] = className // Store the static type of self.

	// Ensure all class attributes are added to locals
	if attrNames, exists := g.classAttrs[className]; exists {
		for i, attrName := range attrNames {
			attrPtr := entryBlock.NewGetElementPtr(
				classType,
				typedSelf,
				constant.NewInt(types.I32, 0),
				constant.NewInt(types.I32, int64(i+1)), // +1 for vtable
			)
			g.locals[attrName] = attrPtr
			fmt.Printf("DEBUG: Added attribute %s to locals for class %s\n", attrName, className)
		}
	} else {
		return fmt.Errorf("no attributes found for class %s", className)
	}
	for i, attrName := range g.classAttrs[className] {
		attrPtr := entryBlock.NewGetElementPtr(
			classType,
			typedSelf,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, int64(i+1)),
		)
		g.locals[attrName] = attrPtr
	}

	value, currentBlock, err := g.generateExpression(entryBlock, method.Body)
	if err != nil {
		return err
	}

	// Only add return instruction if block isn't terminated
	if currentBlock.Term == nil {
		if method.ReturnType.Value == "Object" {
			currentBlock.NewRet(constant.NewNull(types.NewPointer(types.I8)))
		} else {
			currentBlock.NewRet(value)
		}
	}

	return nil
}

var i8Ptr = types.NewPointer(types.I8)

// generateExpression handles code generation for expressions
func (g *CodeGenerator) generateExpression(block *ir.Block, expr ast.Expression) (value.Value, *ir.Block, error) {
	// Add debug logging at the start
	fmt.Printf("DEBUG: Generating LLVM IR for expression type: %T\n", expr)
	if expr == nil {
		fmt.Printf("DEBUG: Expression is nil! Block: %v, Current class: %s\n", block, g.currentClass)
		return nil, nil, fmt.Errorf("received nil expression")
	}

	switch e := expr.(type) {
	case *ast.IntegerLiteral:
		fmt.Printf("DEBUG: Generating integer literal: %d\n", e.Value)
		return constant.NewInt(types.I32, int64(e.Value)), block, nil

	case *ast.BooleanLiteral:
		fmt.Printf("DEBUG: Generating boolean literal: %v\n", e.Value)
		if e.Value {
			return constant.NewInt(types.I1, 1), block, nil
		}
		return constant.NewInt(types.I1, 0), block, nil
	case *ast.StringLiteral:
		// Create global string constant
		strGlobal := g.getOrCreateStringConstant(e.Value)

		// Allocate String object
		stringType := g.classTypes["String"]
		obj := block.NewAlloca(stringType)

		// Initialize vtable pointer (first field)
		vtablePtr := block.NewGetElementPtr(stringType, obj,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, 0),
		)
		block.NewStore(
			block.NewBitCast(g.vtables["String"], types.NewPointer(types.I8)),
			vtablePtr,
		)

		// Initialize 'value' field (second field)
		valuePtr := block.NewGetElementPtr(stringType, obj,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, 1),
		)

		// Get pointer to string data
		var strPtr value.Value
		if arrType, ok := strGlobal.Type().(*types.ArrayType); ok {
			strPtr = block.NewGetElementPtr(arrType, strGlobal,
				constant.NewInt(types.I32, 0),
				constant.NewInt(types.I32, 0),
			)
		} else {
			strPtr = block.NewBitCast(strGlobal, types.NewPointer(types.I8))
		}
		block.NewStore(strPtr, valuePtr)

		// Return the String object as i8*
		return block.NewBitCast(obj, types.NewPointer(types.I8)), block, nil
	case *ast.InfixExpression:
		// Generate code for left and right expressions
		left, leftBlock, err := g.generateExpression(block, e.Left)
		if err != nil {
			return nil, leftBlock, err
		}

		right, rightBlock, err := g.generateExpression(leftBlock, e.Right)
		if err != nil {
			return nil, rightBlock, err
		}

		// Handle different operators
		switch e.Operator {
		case "+":
			return rightBlock.NewAdd(left, right), rightBlock, nil
		case "-":
			return rightBlock.NewSub(left, right), rightBlock, nil
		case "*":
			return rightBlock.NewMul(left, right), rightBlock, nil
		case "/":
			return rightBlock.NewSDiv(left, right), rightBlock, nil
		case "<":
			cmp := rightBlock.NewICmp(enum.IPredSLT, left, right)
			return rightBlock.NewZExt(cmp, types.I32), rightBlock, nil
		default:
			return nil, rightBlock, fmt.Errorf("unsupported operator: %s", e.Operator)
		}

	case *ast.IfExpression:
		cond, currentBlock, err := g.generateExpression(block, e.Condition)
		if err != nil {
			return nil, nil, err
		}
		if cond.Type() != types.I1 {
			cond = currentBlock.NewICmp(enum.IPredNE, cond, constant.NewInt(types.I32, 0))
		}
		thenBlock := currentBlock.Parent.NewBlock("if.then")
		elseBlock := currentBlock.Parent.NewBlock("if.else")
		mergeBlock := currentBlock.Parent.NewBlock("if.merge")

		currentBlock.NewCondBr(cond, thenBlock, elseBlock)

		thenVal, thenCurrentBlock, err := g.generateExpression(thenBlock, e.Consequence)
		if err != nil {
			return nil, nil, err
		}
		thenCurrentBlock.NewBr(mergeBlock)

		elseVal, elseCurrentBlock, err := g.generateExpression(elseBlock, e.Alternative)
		if err != nil {
			return nil, nil, err
		}
		elseCurrentBlock.NewBr(mergeBlock)

		// Ensure both incoming values have the same type.
		if !thenVal.Type().Equal(elseVal.Type()) {
			elseVal = mergeBlock.NewBitCast(elseVal, thenVal.Type())
		}
		phi := mergeBlock.NewPhi(ir.NewIncoming(thenVal, thenBlock), ir.NewIncoming(elseVal, elseBlock))
		return phi, mergeBlock, nil

	case *ast.LetExpression:
		var llvmType types.Type
		switch strings.ToLower(e.Type.Value) {
		case "int":
			llvmType = types.I32
		case "bool":
			llvmType = types.I1
		default:
			// For user-defined types, look up the class type or use i8*
			if classType, exists := g.classTypes[e.Type.Value]; exists {
				llvmType = types.NewPointer(classType)
			} else {
				llvmType = types.NewPointer(types.I8)
			}
		}

		// Allocate storage for the let variable
		varAlloca := block.NewAlloca(llvmType)

		// Generate the initialization expression
		initValue, initBlock, err := g.generateExpression(block, e.Init)
		if err != nil {
			return nil, initBlock, err
		}

		// Cast the value to the correct type if needed
		var valueToStore value.Value
		if initValue.Type().Equal(llvmType) {
			valueToStore = initValue
		} else if types.IsPointer(llvmType) && types.IsPointer(initValue.Type()) {
			// If both types are pointers, bitcast to the target type
			valueToStore = initBlock.NewBitCast(initValue, llvmType)
		} else {
			// If types don't match and aren't both pointers, this is an error
			return nil, initBlock, fmt.Errorf("type mismatch in let expression: got %v, expected %v",
				initValue.Type(), llvmType)
		}

		// Store the value
		initBlock.NewStore(valueToStore, varAlloca)

		// Save any previous binding and update locals
		oldValue, exists := g.locals[e.Name.Value]
		g.locals[e.Name.Value] = varAlloca

		// Generate code for the let body
		bodyValue, bodyBlock, err := g.generateExpression(initBlock, e.Body)
		if err != nil {
			return nil, bodyBlock, err
		}

		// Restore the old binding if it existed
		if exists {
			g.locals[e.Name.Value] = oldValue
		} else {
			delete(g.locals, e.Name.Value)
		}

		return bodyValue, bodyBlock, nil
	case *ast.ObjectIdentifier:
		// First check local variables.
		if varAlloca, exists := g.locals[e.Value]; exists {
			ptrType, ok := varAlloca.Type().(*types.PointerType)
			if !ok {
				return nil, block, fmt.Errorf("unexpected type for variable: %s", e.Value)
			}
			loadInst := block.NewLoad(ptrType.ElemType, varAlloca)
			return loadInst, block, nil
		}

		// If current class is a built-in type, do not attempt attribute lookup.
		lowerClass := strings.ToLower(g.currentClass)
		if lowerClass == "int" || lowerClass == "bool" || lowerClass == "string" {
			return nil, block, fmt.Errorf("undefined variable: %s", e.Value)
		}

		// Otherwise, attempt attribute lookup.
		if g.currentClass != "" {
			// Find attribute info.
			classInfo := g.classTable[g.currentClass]
			var attrInfo *AttributeInfo
			for i := range classInfo.Attributes {
				attr := &classInfo.Attributes[i] // Get pointer to the actual element
				if attr.Name == e.Value {
					attrInfo = attr
					break
				}
			}
			if attrInfo != nil {
				// Get the self parameter from the parent block.
				self := block.Parent.Params[0]
				classType := g.classTypes[g.currentClass]
				// Cast self to the proper type if needed.
				var selfPtr value.Value
				if !self.Type().Equal(types.NewPointer(classType)) {
					selfPtr = block.NewBitCast(self, types.NewPointer(classType))
				} else {
					selfPtr = self
				}
				// Build indices: first index 0 (for the struct), then the attribute offset.
				indices := []value.Value{
					constant.NewInt(types.I32, 0),
					constant.NewInt(types.I32, int64(attrInfo.Offset)),
				}
				attrPtr := block.NewGetElementPtr(classType, selfPtr, indices...)
				// Load the attribute value.
				fieldType := classType.Fields[attrInfo.Offset]
				loadInst := block.NewLoad(fieldType, attrPtr)
				return loadInst, block, nil
			}
		}
		return nil, block, fmt.Errorf("undefined variable: %s", e.Value)

	case *ast.WhileExpression:
		// Create blocks for condition, body, and merge
		condBlock := block.Parent.NewBlock("while.cond")
		bodyBlock := block.Parent.NewBlock("while.body")
		mergeBlock := block.Parent.NewBlock("while.end")
		mergeBlock.NewRet(constant.NewNull(types.NewPointer(types.I8)))

		// Branch to condition block
		block.NewBr(condBlock)

		// Generate condition code
		condition, condEndBlock, err := g.generateExpression(condBlock, e.Condition)
		if err != nil {
			return nil, condEndBlock, err
		}

		// Convert condition to boolean if needed
		var condValue value.Value
		if condition.Type() == types.I1 {
			condValue = condition
		} else {
			condValue = condEndBlock.NewICmp(enum.IPredNE, condition, constant.NewInt(types.I32, 0))
		}

		// Conditional branch: if true go to body, if false go to merge
		condEndBlock.NewCondBr(condValue, bodyBlock, mergeBlock)

		// Generate body code
		_, bodyEndBlock, err := g.generateExpression(bodyBlock, e.Body)
		if err != nil {
			return nil, bodyEndBlock, err
		}

		// Branch back to condition block
		bodyEndBlock.NewBr(condBlock)

		// Return 0 as result (while always returns 0 in COOL)
		result := constant.NewInt(types.I32, 0)
		return result, mergeBlock, nil

	case *ast.MethodCall:
		var receiverObj value.Value
		var receiverType string
		var err error

		if e.Object == nil {
			// Implicit "self" receiver
			receiverObj = g.locals["$self"]
			receiverType = g.currentClass
		} else {
			// Generate code for the receiver expression
			receiverObj, block, err = g.generateExpression(block, e.Object)
			if err != nil {
				return nil, block, err
			}

			// Determine receiver type based on expression
			switch expr := e.Object.(type) {
			case *ast.NewExpression:
				receiverType = expr.Type.Value
			case *ast.ObjectIdentifier:
				// First check if it's a class attribute
				if classInfo, exists := g.classTable[g.currentClass]; exists {
					for _, attr := range classInfo.Attributes {
						if attr.Name == expr.Value {
							receiverType = attr.Type
							break
						}
					}
				}

				// If not found as attribute, check local variables
				if receiverType == "" {
					var exists bool
					receiverType, exists = g.localsTypes[expr.Value]
					if !exists {
						return nil, block, fmt.Errorf("undefined variable: %s", expr.Value)
					}
				}
			default:
				return nil, block, fmt.Errorf("unsupported receiver expression: %T", e.Object)
			}
		}

		// Find the method in the class hierarchy
		methodName := fmt.Sprintf("%s_%s", receiverType, e.Method.Value)
		methodFunc := g.methods[methodName]

		// If method not found in current class, look in parent class (IO)
		if methodFunc == nil && receiverType == "Main" {
			methodName = fmt.Sprintf("IO_%s", e.Method.Value)
			methodFunc = g.methods[methodName]
		}

		if methodFunc == nil {
			return nil, block, fmt.Errorf("method %s not found", methodName)
		}

		// Generate arguments
		args := make([]value.Value, 0, len(e.Arguments)+1)
		args = append(args, receiverObj)

		for _, arg := range e.Arguments {
			argVal, newBlock, err := g.generateExpression(block, arg)
			if err != nil {
				return nil, newBlock, err
			}
			block = newBlock
			args = append(args, argVal)
		}

		// Call the method
		result := block.NewCall(methodFunc, args...)
		if result == nil {
			return nil, block, fmt.Errorf("failed to generate call to %s", methodName)
		}
		if result.Type() == types.Void {
			// For void returns, add unreachable if needed
			if block.Term == nil {
				block.NewUnreachable()
			}
		}
		return result, block, nil

	case *ast.FunctionCall:
		// Generate argument expressions
		if e.Function.Value == "out_string" && g.currentClass == "IO" {
			// Implicit receiver: use self.
			methodCall := &ast.MethodCall{
				Token:     e.Token,
				Object:    &ast.ObjectIdentifier{Token: lexer.Token{Literal: "self"}, Value: "self"},
				Method:    e.Function,
				Arguments: e.Arguments,
			}
			// Now process as a MethodCall.
			return g.generateExpression(block, methodCall)
		}
		args := make([]value.Value, 0, len(e.Arguments))
		currentBlock := block

		for _, arg := range e.Arguments {
			argValue, argBlock, err := g.generateExpression(currentBlock, arg)
			if err != nil {
				return nil, argBlock, err
			}
			args = append(args, argValue)
			currentBlock = argBlock
		}

		// Get function from module
		funcName := fmt.Sprintf("%s_%s", "Main", e.Function.Value) // TODO: handle other classes
		function := g.methods[funcName]
		if function == nil {
			return nil, currentBlock, fmt.Errorf("undefined function: %s", e.Function.Value)
		}

		// Call the function with arguments
		call := currentBlock.NewCall(function, args...)
		return call, currentBlock, nil

	case *ast.NewExpression:
		classType := g.classTypes[e.Type.Value]
		if classType == nil {
			return nil, block, fmt.Errorf("undefined type: %s", e.Type.Value)
		}
		info, ok := g.classTable[e.Type.Value]
		if !ok {
			return nil, block, fmt.Errorf("no class info for type: %s", e.Type.Value)
		}

		objPtr := block.NewAlloca(classType)
		objPtr_i8 := block.NewBitCast(objPtr, i8Ptr)
		// Use the computed object size (assume each field is 8 bytes)
		size := constant.NewInt(types.I64, int64(info.ObjectSize*8))

		// Look up an existing memset intrinsic.
		var memsetFunc *ir.Func
		for _, f := range g.module.Funcs {
			if f.Name() == "llvm.memset.p0i8.i64" {
				memsetFunc = f
				break
			}
		}
		if memsetFunc == nil {
			memsetFunc = g.module.NewFunc("llvm.memset.p0i8.i64", types.Void,
				ir.NewParam("dst", i8Ptr),
				ir.NewParam("val", types.I8),
				ir.NewParam("len", types.I64),
				ir.NewParam("align", types.I32),
				ir.NewParam("isvolatile", types.I1))
		}
		block.NewCall(memsetFunc, objPtr_i8, constant.NewInt(types.I8, 0), size,
			constant.NewInt(types.I32, 0), constant.NewInt(types.I1, 0))

		vtable := g.vtables[e.Type.Value]
		if vtable == nil {
			return nil, block, fmt.Errorf("vtable not found for type: %s", e.Type.Value)
		}
		vtablePtr := block.NewGetElementPtr(classType, objPtr,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, 0))
		castedVtable := block.NewBitCast(vtable, i8Ptr)
		block.NewStore(castedVtable, vtablePtr)

		for _, attr := range info.Attributes {
			attrPtr := block.NewGetElementPtr(classType, objPtr,
				constant.NewInt(types.I32, 0),
				constant.NewInt(types.I32, int64(attr.Offset)))
			var defaultVal value.Value
			switch strings.ToLower(attr.Type) {
			case "int":
				defaultVal = constant.NewInt(types.I32, 0)
			case "bool":
				defaultVal = constant.NewInt(types.I1, 0)
			case "string":
				emptyStr := g.getOrCreateStringConstant("")
				defaultVal = block.NewBitCast(emptyStr, i8Ptr)
			default:
				objType, exists := g.classTypes[attr.Type]
				if !exists {
					return nil, block, fmt.Errorf("unknown attribute type: %s", attr.Type)
				}
				defaultVal = constant.NewNull(types.NewPointer(objType))
			}
			block.NewStore(defaultVal, attrPtr)
		}

		return block.NewBitCast(objPtr, i8Ptr), block, nil
	case *ast.NotExpression:
		// Generate the expression to negate
		expr, exprBlock, err := g.generateExpression(block, e.Expression)
		if err != nil {
			return nil, exprBlock, err
		}

		// Convert to boolean if needed
		var boolValue value.Value
		if expr.Type() != types.I1 {
			boolValue = exprBlock.NewICmp(enum.IPredNE, expr, constant.NewInt(types.I32, 0))
		} else {
			boolValue = expr
		}

		// Negate the boolean and convert back to i32
		notValue := exprBlock.NewXor(boolValue, constant.NewInt(types.I1, 1))
		result := exprBlock.NewZExt(notValue, types.I32)

		return result, exprBlock, nil

	case *ast.IsVoidExpression:
		// Generate the expression to check
		expr, exprBlock, err := g.generateExpression(block, e.Expression)
		if err != nil {
			return nil, exprBlock, err
		}

		// Compare with null pointer
		var isVoid value.Value
		if expr.Type().Equal(types.I32) {
			// For basic types, always false
			isVoid = constant.NewInt(types.I1, 0)
		} else {
			// For objects, compare with null
			ptrType, ok := expr.Type().(*types.PointerType)
			if !ok {
				isVoid = constant.NewInt(types.I1, 0)
			} else {
				nullPtr := constant.NewNull(ptrType)
				isVoid = exprBlock.NewICmp(enum.IPredEQ, expr, nullPtr)
			}
		}

		// Convert boolean to i32
		result := exprBlock.NewZExt(isVoid, types.I32)

		return result, exprBlock, nil

	case *ast.BlockExpression:
		var lastValue value.Value
		currentBlock := block

		// Generate code for each expression in sequence
		for _, expr := range e.Expressions {
			var err error
			lastValue, currentBlock, err = g.generateExpression(currentBlock, expr)
			if err != nil {
				return nil, currentBlock, err
			}
		}

		// Return the value of the last expression
		return lastValue, currentBlock, nil

	case *ast.Assignment:
		// Generate the value to assign
		value, valueBlock, err := g.generateExpression(block, e.Expression)
		if err != nil {
			return nil, valueBlock, err
		}

		// Look up the variable location
		varPtr := g.locals[e.Name.Value]
		if varPtr == nil {
			return nil, valueBlock, fmt.Errorf("undefined variable: %s", e.Name.Value)
		}

		// Get destination type
		destType := varPtr.Type().(*types.PointerType).ElemType

		// Cast value to destination type if necessary
		if value.Type() != destType {
			if types.IsPointer(value.Type()) && types.IsPointer(destType) {
				value = valueBlock.NewBitCast(value, destType)
			} else {
				return nil, valueBlock, fmt.Errorf("type mismatch in assignment: %s -> %s", value.Type(), destType)
			}
		}

		// Store the new value
		valueBlock.NewStore(value, varPtr)

		// Return the assigned value
		return value, valueBlock, nil

	default:
		fmt.Printf("DEBUG: Unhandled expression type: %T\n", expr)
		fmt.Printf("DEBUG: Expression details: %+v\n", expr)
		return nil, nil, fmt.Errorf("unsupported expression type: %T", expr)
	}
}

// getMethodIndex returns the index of a method in the vtable
func (g *CodeGenerator) getMethodIndex(methodName string) int64 {
	// TODO: implement proper method indexing based on class hierarchy
	switch methodName {
	case "main":
		return 0
	default:
		return 0 // For now, return 0 for all methods
	}
}
func (g *CodeGenerator) getOrCreateStringConstant(name string) *ir.Global {
	// Replace non-alphanumeric characters with underscores
	sanitizedName := strings.Map(func(r rune) rune {
		if !unicode.IsLetter(r) && !unicode.IsNumber(r) {
			return '_'
		}
		return r
	}, name)
	strConstName := fmt.Sprintf(".str.%s", sanitizedName)

	// Check if it exists
	for _, global := range g.module.Globals {
		if global.Name() == strConstName {
			return global
		}
	}

	// Create new global
	strConst := g.module.NewGlobalDef(strConstName, constant.NewCharArray(append([]byte(name), 0)))
	return strConst
}

func (g *CodeGenerator) GetClassTable() ClassTable {
	return g.classTable
}

func (g *CodeGenerator) GetModule() *ir.Module {
	return g.module
}

func (g *CodeGenerator) GetVTables() map[string]*ir.Global {
	return g.vtables
}
func (g *CodeGenerator) BuildClassTypesFromTable() error {
	for className, info := range g.classTable {
		// Start with vtable pointer
		fields := []types.Type{types.NewPointer(types.I8)}

		// Add all attributes (inherited and local)
		for _, attr := range info.Attributes {
			var fieldType types.Type
			// Handle String's 'value' as special case (i8*)
			if className == "String" && attr.Name == "value" {
				fieldType = types.NewPointer(types.I8)
			} else {
				switch strings.ToLower(attr.Type) {
				case "int":
					fieldType = types.I32
				case "bool":
					fieldType = types.I1
				default:
					// For class types, use pointer to the class struct type
					if classType, exists := g.classTypes[attr.Type]; exists {
						fieldType = types.NewPointer(classType)
					} else {
						fieldType = types.NewPointer(types.I8)
					}
				}
			}
			fields = append(fields, fieldType)
		}

		classType := types.NewStruct(fields...)
		g.classTypes[className] = classType
	}
	return nil
}

// getClassNameFromType returns the class name for a given LLVM type (if it's a class struct pointer)
func (g *CodeGenerator) getClassNameFromType(t types.Type) string {
	ptrType, ok := t.(*types.PointerType)
	if !ok {
		return ""
	}
	structType, ok := ptrType.ElemType.(*types.StructType)
	if !ok {
		return ""
	}
	for className, classType := range g.classTypes {
		if classType.Equal(structType) {
			return className
		}
	}
	return ""
}

// isSubclass checks if subclass is a descendant of superclass
func (g *CodeGenerator) isSubclass(subclass, superclass string) bool {
	current := subclass
	for current != "" {
		if current == superclass {
			return true
		}
		info, exists := g.classTable[current]
		if !exists {
			return false
		}
		current = info.Parent
	}
	return false
}

// getMethodIndexFor looks up the vtable slot for a given method in the specified class.
func (g *CodeGenerator) getMethodIndexFor(receiverClass, methodName string) int64 {
	if classInfo, ok := g.classTable[receiverClass]; ok {
		if methodInfo, ok := classInfo.Methods[methodName]; ok {
			return int64(methodInfo.Index)
		}
	}
	// Fallback to 0 if not found.
	return 0
}
func (g *CodeGenerator) addBuiltInClasses(program *ast.Program) {
	objectClass := &ast.Class{
		Token:  lexer.Token{Literal: "class"},
		Name:   &ast.ObjectIdentifier{Token: lexer.Token{Literal: "Object"}, Value: "Object"},
		Parent: &ast.TypeIdentifier{Value: ""}, // Explicit empty parent
		Features: []ast.Feature{
			&ast.Method{
				Token:      lexer.Token{Literal: "method"},
				Name:       &ast.ObjectIdentifier{Token: lexer.Token{Literal: "abort"}, Value: "abort"},
				Parameters: []*ast.Formal{},
				ReturnType: &ast.TypeIdentifier{Token: lexer.Token{Literal: "Object"}, Value: "Object"},
				Body:       nil, // No body needed
			},
			&ast.Method{
				Token:      lexer.Token{Literal: "method"},
				Name:       &ast.ObjectIdentifier{Token: lexer.Token{Literal: "type_name"}, Value: "type_name"},
				Parameters: []*ast.Formal{},
				ReturnType: &ast.TypeIdentifier{Token: lexer.Token{Literal: "String"}, Value: "String"},
				Body:       nil,
			},
		},
	}
	// Create built-in IO class with its methods.
	ioClass := &ast.Class{
		Token:  lexer.Token{Literal: "class"},
		Name:   &ast.ObjectIdentifier{Token: lexer.Token{Literal: "IO"}, Value: "IO"},
		Parent: &ast.TypeIdentifier{Token: lexer.Token{Literal: "inherits"}, Value: "Object"},
		Features: []ast.Feature{
			// out_string method
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
			// Add out_int method
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
				Body:       &ast.IntegerLiteral{Value: 0}, // Dummy body

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
				Body:       &ast.StringLiteral{Value: ""}, // Dummy body

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
				Body:       &ast.StringLiteral{Value: ""}, // Dummy body
			},
		},
	}

	// Prepend built-in classes to the program
	program.Classes = append([]*ast.Class{stringClass, ioClass, objectClass}, program.Classes...)
}

func (g *CodeGenerator) initializeAttribute(block *ir.Block, attrPtr value.Value, attrType string, initExpr ast.Expression) (*ir.Block, error) {
	if initExpr != nil {
		// Generate initialization expression
		val, currentBlock, err := g.generateExpression(block, initExpr)
		if err != nil {
			return currentBlock, err
		}

		// Get the destination type
		destPtrType := attrPtr.Type().(*types.PointerType)
		destType := destPtrType.ElemType

		// If we're initializing a String
		if structType, ok := destType.(*types.PointerType); ok &&
			structType.ElemType.Equal(g.classTypes["String"]) {
			// Cast the value to the correct type if needed
			if !val.Type().Equal(destType) {
				val = currentBlock.NewBitCast(val, destType)
			}
		}

		// Store the value
		currentBlock.NewStore(val, attrPtr)
		return currentBlock, nil
	}

	// Default initialization
	var defaultVal value.Value
	switch strings.ToLower(attrType) {
	case "int":
		defaultVal = constant.NewInt(types.I32, 0)
	case "bool":
		defaultVal = constant.NewInt(types.I1, 0)
	case "string":
		// Create empty string
		stringType := g.classTypes["String"]
		obj := block.NewAlloca(stringType)

		// Set vtable
		vtablePtr := block.NewGetElementPtr(stringType, obj,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, 0),
		)
		block.NewStore(
			block.NewBitCast(g.vtables["String"], types.NewPointer(types.I8)),
			vtablePtr,
		)

		// Set empty string value
		emptyStr := g.getOrCreateStringConstant("")
		valuePtr := block.NewGetElementPtr(stringType, obj,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, 1),
		)

		var strPtr value.Value
		if arrType, ok := emptyStr.Type().(*types.ArrayType); ok {
			strPtr = block.NewGetElementPtr(arrType, emptyStr,
				constant.NewInt(types.I32, 0),
				constant.NewInt(types.I32, 0),
			)
		} else {
			strPtr = block.NewBitCast(emptyStr, types.NewPointer(types.I8))
		}
		block.NewStore(strPtr, valuePtr)

		// Store pointer to the String object
		defaultVal = block.NewBitCast(obj, types.NewPointer(types.I8))
	default:
		if classType, exists := g.classTypes[attrType]; exists {
			defaultVal = constant.NewNull(types.NewPointer(classType))
		} else {
			defaultVal = constant.NewNull(types.NewPointer(types.I8))
		}
	}

	// Cast the default value to the correct type if needed
	destPtrType := attrPtr.Type().(*types.PointerType)
	if !defaultVal.Type().Equal(destPtrType.ElemType) {
		defaultVal = block.NewBitCast(defaultVal, destPtrType.ElemType)
	}

	block.NewStore(defaultVal, attrPtr)
	return block, nil
}
