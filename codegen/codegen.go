package codegen

import (
	"cool-compiler/ast"
	"cool-compiler/lexer"
	"fmt"
	"strings"
	"unicode"
"sort"
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
	module := ir.NewModule()
	module.TargetTriple = "arm64-apple-macosx" // For M1/M2 Macs
	return &CodeGenerator{
		module:      module,
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
		return fmt.Errorf("main class not found")
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
	fmt.Printf("\n=== Generating method %s_%s ===\n", className, method.Name.Value)

	// Debug class layout
	classInfo := g.classTable[className]
	fmt.Printf("Class layout for %s:\n", className)
	fmt.Printf("Parent class: %s\n", classInfo.Parent)
	fmt.Printf("Attributes:\n")
	for _, attr := range classInfo.Attributes {
		fmt.Printf("  - %s (type: %s) at offset %d\n", attr.Name, attr.Type, attr.Offset)
	}

	// Debug method info
	fmt.Printf("\nMethod details:\n")
	fmt.Printf("  Name: %s\n", method.Name.Value)
	fmt.Printf("  Return type: %s\n", method.ReturnType.Value)
	fmt.Printf("  Parameters: ")
	for _, param := range method.Parameters {
		fmt.Printf("%s:%s ", param.Name.Value, param.Type.Value)
	}
	fmt.Printf("\n")

	// Debug method body
	fmt.Printf("\nMethod body type: %T\n", method.Body)

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
	if method.Name.Value == "copy" {
		currentClass := className
		classType := g.classTypes[currentClass]

		// Allocate new object
		newObj := entryBlock.NewAlloca(classType)
		newObjI8 := entryBlock.NewBitCast(newObj, types.NewPointer(types.I8))

		// Copy vtable from self
		self := entryBlock.Parent.Params[0]
		selfPtr := entryBlock.NewBitCast(self, types.NewPointer(classType))
		vtablePtr := entryBlock.NewGetElementPtr(classType, selfPtr, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
		vtableVal := entryBlock.NewLoad(types.NewPointer(types.I8), vtablePtr)
		newVtablePtr := entryBlock.NewGetElementPtr(classType, newObj, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
		entryBlock.NewStore(vtableVal, newVtablePtr)

		// Copy each attribute
		classInfo := g.classTable[currentClass]
		for _, attr := range classInfo.Attributes {
			structFieldIndex := attr.Offset // Offset starts at 1 (vtable is at 0)
			attrSrcPtr := entryBlock.NewGetElementPtr(classType, selfPtr,
				constant.NewInt(types.I32, 0),
				constant.NewInt(types.I32, int64(structFieldIndex)),
			)
			// In generateMethod's copy handling:
			attrVal := entryBlock.NewLoad(
				classType.Fields[structFieldIndex], // Directly use the field type
				attrSrcPtr,
			)
			attrDstPtr := entryBlock.NewGetElementPtr(classType, newObj,
				constant.NewInt(types.I32, 0),
				constant.NewInt(types.I32, int64(structFieldIndex)),
			)
			fieldType := classType.Fields[structFieldIndex]

			if fieldType, ok := fieldType.(*types.PointerType); ok {
				if className := g.getClassNameFromType(fieldType); className != "" {
					// Directly cast the pointer without loading its value
					castedVal := entryBlock.NewBitCast(attrVal, fieldType)
					entryBlock.NewStore(castedVal, attrDstPtr)
					continue
				}
			}
			entryBlock.NewStore(attrVal, attrDstPtr)
		}

		entryBlock.NewRet(newObjI8)
		return nil
	}
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

		// Define a struct type for the vtable header (class name and parent)
		vtableHeaderType := types.NewStruct(
			types.NewPointer(types.I8), // class name
			types.NewPointer(types.I8), // parent vtable
		)
		castedVtable := entryBlock.NewBitCast(vtablePtr, types.NewPointer(vtableHeaderType))

		// Get class name pointer (first field of vtable)
		classNamePtrPtr := entryBlock.NewGetElementPtr(vtableHeaderType, castedVtable,
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
		if method.Name.Value == "in_string" {
			// self := fn.Params[0] // Get self parameter
			// entryBlock := fn.NewBlock("entry")

			// Declare needed functions and globals
			mallocFunc := g.getOrCreateMalloc()

			 // Declare opaque FILE type
			 fileType := types.NewStruct()
			 fileType.SetName("struct.__sFILE")
			 fileType.Opaque = true
			 g.module.NewTypeDef("struct.__sFILE", fileType)
			 // Declare stdin as external global (FILE*)
			 var stdinGlobal *ir.Global
			 for _, global := range g.module.Globals {
				 if global.Name() == "__stdinp" {
					 stdinGlobal = global
					 break
				 }
			 }
			 if stdinGlobal == nil {
				// Define the global with the macOS-specific symbol name
				fileType := types.NewStruct()
				 // macOS's FILE type
				fileType.SetName("struct.__sFILE")
				fileType.Opaque = true
				stdinGlobal = g.module.NewGlobal("__stdinp", types.NewPointer(types.NewPointer(fileType)))
				stdinGlobal.Linkage = enum.LinkageExternal
			}
		 
			 // Declare fgets with correct parameter types
			 var fgetsFn *ir.Func
			 for _, f := range g.module.Funcs {
				 if f.Name() == "fgets" {
					 fgetsFn = f
					 break
				 }
			 }
			 if fgetsFn == nil {
				 fgetsFn = g.module.NewFunc(
					 "fgets",
					 types.I8Ptr,
					 ir.NewParam("s", types.I8Ptr),
					 ir.NewParam("size", types.I32),
					 ir.NewParam("stream", types.NewPointer(fileType)),
				 )
			 }

			 stdinVal := entryBlock.NewLoad(types.NewPointer(fileType), stdinGlobal)
			 buffer := entryBlock.NewCall(mallocFunc, constant.NewInt(types.I64, 1024))
			_ = entryBlock.NewCall(fgetsFn, buffer, constant.NewInt(types.I32, 1024), stdinVal)
			// Allocate buffer (1024 bytes)
			// bufferSize := constant.NewInt(types.I64, 1024)
			// buffer := entryBlock.NewCall(mallocFunc, bufferSize)
			// Remove newline character if present
			var strchrFn *ir.Func
			for _, f := range g.module.Funcs {
				if f.Name() == "strchr" {
					strchrFn = f
					break
				}
			}
			if strchrFn == nil {
				strchrFn = g.module.NewFunc("strchr", types.I8Ptr,
					ir.NewParam("s", types.I8Ptr),
					ir.NewParam("c", types.I32),
				)
			}

			// Find newline
			newline := entryBlock.NewCall(strchrFn, buffer, constant.NewInt(types.I32, '\n'))
			foundBlock := fn.NewBlock("in_string.found_newline")
			continueBlock := fn.NewBlock("in_string.continue")

			isNewline := entryBlock.NewICmp(enum.IPredNE, newline, constant.NewNull(types.I8Ptr))
			entryBlock.NewCondBr(isNewline, foundBlock, continueBlock) // Terminator for entry block

			// --- Handle foundBlock ---
			foundBlock.NewStore(constant.NewInt(types.I8, 0), newline)
			foundBlock.NewBr(continueBlock) // Terminator for foundBlock

			// --- Handle continueBlock ---
			// Build String object and return
			stringType := g.classTypes["String"]
			stringMem := continueBlock.NewCall(mallocFunc, constant.NewInt(types.I64, 16))
			stringObj := continueBlock.NewBitCast(stringMem, types.NewPointer(stringType))

			// Set vtable
			vtablePtr := continueBlock.NewGetElementPtr(stringType, stringObj,
				constant.NewInt(types.I32, 0),
				constant.NewInt(types.I32, 0),
			)
			continueBlock.NewStore(
				continueBlock.NewBitCast(g.vtables["String"], types.I8Ptr),
				vtablePtr,
			)

			// Set value field
			valuePtr := continueBlock.NewGetElementPtr(stringType, stringObj,
				constant.NewInt(types.I32, 0),
				constant.NewInt(types.I32, 1),
			)
			continueBlock.NewStore(buffer, valuePtr)

			// Return statement (terminator for continueBlock)
			result := continueBlock.NewBitCast(stringObj, types.I8Ptr)
			continueBlock.NewRet(result) // Terminator for continueBlock

			return nil
		}
		// Inside the IO class handling in generateMethod:
if method.Name.Value == "in_int" {
    // Declare needed functions
    mallocFunc := g.getOrCreateMalloc()
    var fgetsFn, strtolFn, abortFn *ir.Func

    // Get or declare fgets
    for _, f := range g.module.Funcs {
        switch f.Name() {
        case "fgets":
            fgetsFn = f
        case "strtol":
            strtolFn = f
        case "abort":
            abortFn = f
        }
    }
    
    // Declare FILE type
    fileType := types.NewStruct()
    fileType.SetName("struct.__sFILE")
    fileType.Opaque = true

    // Get stdin global
    var stdinGlobal *ir.Global
    for _, global := range g.module.Globals {
        if global.Name() == "__stdinp" {
            stdinGlobal = global
            break
        }
    }

    // Declare strtol if not found
    if strtolFn == nil {
        strtolFn = g.module.NewFunc("strtol", types.I64,
            ir.NewParam("nptr", types.I8Ptr),
            ir.NewParam("endptr", types.NewPointer(types.I8Ptr)),
            ir.NewParam("base", types.I32),
        )
    }

    // Allocate buffer
    buffer := entryBlock.NewCall(mallocFunc, constant.NewInt(types.I64, 1024))
    
    // Load stdin FILE*
    stdinVal := entryBlock.NewLoad(types.NewPointer(fileType), stdinGlobal)
    
    // Call fgets
    entryBlock.NewCall(fgetsFn, buffer, constant.NewInt(types.I32, 1024), stdinVal)

    // Convert string to long
    endPtr := entryBlock.NewAlloca(types.I8Ptr)
    longVal := entryBlock.NewCall(strtolFn, buffer, endPtr, constant.NewInt(types.I32, 10))
    
    // Check conversion result
    errorBlock := fn.NewBlock("in_int.error")
    successBlock := fn.NewBlock("in_int.success")
    
    // Check if endPtr == buffer (no digits parsed)
    cmp := entryBlock.NewICmp(enum.IPredEQ, endPtr, buffer)
    entryBlock.NewCondBr(cmp, errorBlock, successBlock)

    // Error handling
    errorBlock.NewCall(abortFn)
    errorBlock.NewUnreachable()

    // Success case
    // Truncate to i32 and return
    intVal := successBlock.NewTrunc(longVal, types.I32)
    successBlock.NewRet(intVal)

    return nil
}
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
			formatGlobal := g.getOrCreateStringConstant("%s\x00") // Note explicit null terminator
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
	g.localsTypes["$self"] = className
	g.locals["self"] = typedSelf
	g.localsTypes["self"] = className
	// Process method parameters and add them to locals
	for i, astParam := range method.Parameters {
		if i+1 >= len(fn.Params) {
			return fmt.Errorf("parameter %s not found in LLVM function parameters", astParam.Name.Value)
		}
		llvmParam := fn.Params[i+1] // i+1 because 0 is self

		// Allocate space for the parameter
		paramAlloca := entryBlock.NewAlloca(llvmParam.Type())
		entryBlock.NewStore(llvmParam, paramAlloca)

		// Add to locals with the AST parameter name
		g.locals[astParam.Name.Value] = paramAlloca
		g.localsTypes[astParam.Name.Value] = astParam.Type.Value
	}

	// Ensure all class attributes are added to locals
	classInfo = g.classTable[className]
	for _, attr := range classInfo.Attributes {
		// Calculate field index (offset includes vtable pointer at 0)
		fieldIndex := attr.Offset

		// Get pointer to attribute using classType
		attrPtr := entryBlock.NewGetElementPtr(
			classType,
			typedSelf,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, int64(fieldIndex)),
		)

		// Store in locals using actual attribute name
		g.locals[attr.Name] = attrPtr
		fmt.Printf("DEBUG: Added attribute %s (offset %d) to locals\n", attr.Name, fieldIndex)
	}

	value, currentBlock, err := g.generateExpression(entryBlock, method.Body)
	if err != nil {
		return err
	}

	// Ensure block termination
	if currentBlock.Term == nil {
		// Handle void return types
		if method.ReturnType.Value == "Void" || method.ReturnType.Value == "Unit" {
			currentBlock.NewRet(nil)
		} else {
			// Default return value if missing
			if value == nil {
				value = constant.NewNull(types.NewPointer(types.I8))
			}
			currentBlock.NewRet(value)
		}
	}

	// Add explicit terminator for entry block if needed
	if entryBlock.Term == nil {
		entryBlock.NewBr(currentBlock)
	}

	fmt.Printf("=== End generating method %s_%s ===\n\n", className, method.Name.Value)
	return nil
}

var i8Ptr = types.NewPointer(types.I8)

func (g *CodeGenerator) boxPrimitive(block *ir.Block, val value.Value, primType string) (value.Value, *ir.Block, error) {
    className := primType
    classType := g.classTypes[className]
    if classType == nil {
        return nil, block, fmt.Errorf("cannot box %s: no class type", primType)
    }

    // Allocate object
    obj := block.NewAlloca(classType)
    objPtr := block.NewBitCast(obj, types.NewPointer(types.I8))

    // Initialize vtable
    vtablePtr := block.NewGetElementPtr(classType, obj,
        constant.NewInt(types.I32, 0),
        constant.NewInt(types.I32, 0),
    )
    block.NewStore(
        block.NewBitCast(g.vtables[className], types.NewPointer(types.I8)),
        vtablePtr,
    )

    // Store primitive value in attribute
    attrOffset := 1 // Assuming value is at offset 1
    attrPtr := block.NewGetElementPtr(classType, obj,
        constant.NewInt(types.I32, 0),
        constant.NewInt(types.I32, int64(attrOffset)),
    )

    // Cast value to correct type
    var castVal value.Value
    switch primType {
    case "Int":
        castVal = val
    case "Bool":
        castVal = block.NewZExt(val, types.I32)
    default:
        return nil, block, fmt.Errorf("unsupported boxing for %s", primType)
    }

    block.NewStore(castVal, attrPtr)
    return objPtr, block, nil
}
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

		// Replace the CaseExpression case in the generateExpression function with this implementation
	case *ast.CaseExpression:
		fmt.Printf("DEBUG: Generating case expression\n")
	
		// Evaluate target expression
		target, currentBlock, err := g.generateExpression(block, e.Expression)
		if err != nil {
			return nil, currentBlock, err
		}
	
		// Create control flow blocks
		endBlock := currentBlock.Parent.NewBlock("case.end")
		var incoming []*ir.Incoming
	
		// Box primitive types if needed
		if target.Type() == types.I32 {
			fmt.Printf("DEBUG: Boxing Int value for case expression\n")
			// Create Int object
			intType := g.classTypes["Int"]
			if intType == nil {
				return nil, currentBlock, fmt.Errorf("Int type not found in class types")
			}
			
			intObj := currentBlock.NewAlloca(intType)
			
			// Set vtable
			vtablePtr := currentBlock.NewGetElementPtr(intType, intObj,
				constant.NewInt(types.I32, 0),
				constant.NewInt(types.I32, 0),
			)
			
			if g.vtables["Int"] == nil {
				return nil, currentBlock, fmt.Errorf("Int vtable not found")
			}
			
			currentBlock.NewStore(
				currentBlock.NewBitCast(g.vtables["Int"], types.NewPointer(types.I8)),
				vtablePtr,
			)
			
			// Set value field
			valuePtr := currentBlock.NewGetElementPtr(intType, intObj,
				constant.NewInt(types.I32, 0),
				constant.NewInt(types.I32, 1),
			)
			currentBlock.NewStore(target, valuePtr)
			
			// Update target to use boxed object
			target = currentBlock.NewBitCast(intObj, types.NewPointer(types.I8))
		} else if target.Type() == types.I1 {
			fmt.Printf("DEBUG: Boxing Bool value for case expression\n")
			// Create Bool object
			boolType := g.classTypes["Bool"]
			if boolType == nil {
				return nil, currentBlock, fmt.Errorf("Bool type not found in class types")
			}
			
			boolObj := currentBlock.NewAlloca(boolType)
			
			// Set vtable
			vtablePtr := currentBlock.NewGetElementPtr(boolType, boolObj,
				constant.NewInt(types.I32, 0),
				constant.NewInt(types.I32, 0),
			)
			
			if g.vtables["Bool"] == nil {
				return nil, currentBlock, fmt.Errorf("Bool vtable not found")
			}
			
			currentBlock.NewStore(
				currentBlock.NewBitCast(g.vtables["Bool"], types.NewPointer(types.I8)),
				vtablePtr,
			)
			
			// Set value field
			valuePtr := currentBlock.NewGetElementPtr(boolType, boolObj,
				constant.NewInt(types.I32, 0),
				constant.NewInt(types.I32, 1),
			)
			fieldType := boolType.Fields[1]
			if fieldType.Equal(types.I32) {
				// If Bool.val is defined as i32, extend the i1 value
				extendedBool := currentBlock.NewZExt(target, types.I32)
				currentBlock.NewStore(extendedBool, valuePtr)
			} else {
				// If Bool.val is defined as i1, store directly
				currentBlock.NewStore(target, valuePtr)
			}
			
			// Update target to use boxed object
			target = currentBlock.NewBitCast(boolObj, types.NewPointer(types.I8))
		} else if ptrType, isPtrType := target.Type().(*types.PointerType); isPtrType {
			if elemType, isIntPtr := ptrType.ElemType.(*types.IntType); isIntPtr {
				// We have a pointer to an integer, need to load it first
				fmt.Printf("DEBUG: Loading from integer pointer\n")
				
				loadedValue := currentBlock.NewLoad(elemType, target)
				
				if elemType.BitSize == 32 {
					// Int boxing logic
					intType := g.classTypes["Int"]
					if intType == nil {
						return nil, currentBlock, fmt.Errorf("Int type not found in class types")
					}
					
					intObj := currentBlock.NewAlloca(intType)
					
					// Set vtable
					vtablePtr := currentBlock.NewGetElementPtr(intType, intObj,
						constant.NewInt(types.I32, 0),
						constant.NewInt(types.I32, 0),
					)
					
					if g.vtables["Int"] == nil {
						return nil, currentBlock, fmt.Errorf("Int vtable not found")
					}
					
					currentBlock.NewStore(
						currentBlock.NewBitCast(g.vtables["Int"], types.NewPointer(types.I8)),
						vtablePtr,
					)
					
					// Set value field
					valuePtr := currentBlock.NewGetElementPtr(intType, intObj,
						constant.NewInt(types.I32, 0),
						constant.NewInt(types.I32, 1),
					)
					currentBlock.NewStore(loadedValue, valuePtr)
					
					// Update target to use boxed object
					target = currentBlock.NewBitCast(intObj, types.NewPointer(types.I8))
				} else if elemType.BitSize == 1 {
					// Bool boxing logic
					boolType := g.classTypes["Bool"]
					if boolType == nil {
						return nil, currentBlock, fmt.Errorf("Bool type not found in class types")
					}
					
					boolObj := currentBlock.NewAlloca(boolType)
					
					// Set vtable
					vtablePtr := currentBlock.NewGetElementPtr(boolType, boolObj,
						constant.NewInt(types.I32, 0),
						constant.NewInt(types.I32, 0),
					)
					
					if g.vtables["Bool"] == nil {
						return nil, currentBlock, fmt.Errorf("Bool vtable not found")
					}
					
					currentBlock.NewStore(
						currentBlock.NewBitCast(g.vtables["Bool"], types.NewPointer(types.I8)),
						vtablePtr,
					)
					
					// Set value field
					valuePtr := currentBlock.NewGetElementPtr(boolType, boolObj,
						constant.NewInt(types.I32, 0),
						constant.NewInt(types.I32, 1),
					)
									// Check the actual field type before storing
					fieldType := boolType.Fields[1]
					if fieldType.Equal(types.I32) {
						// If Bool.val is defined as i32, extend the i1 value
						extendedBool := currentBlock.NewZExt(loadedValue, types.I32)
						currentBlock.NewStore(extendedBool, valuePtr)
					} else {
						// If Bool.val is defined as i1, store directly 
						currentBlock.NewStore(loadedValue, valuePtr)
					}
					// Update target to use boxed object
					target = currentBlock.NewBitCast(boolObj, types.NewPointer(types.I8))
				}
			}
		}
	
		// Null check handling
		nonNullBlock := currentBlock.Parent.NewBlock("case.non_null")
		abortBlock := currentBlock.Parent.NewBlock("case.abort")
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
		abortBlock.NewCall(abortFunc)
		abortBlock.NewUnreachable()
	
		// Find Object case early
		var objectCase *ast.Case
		// var objectCaseIndex int = -1
		for _, c := range e.Cases {
			if c.Type.Value == "Object" {
				objectCase = c
				// objectCaseIndex = i
				break
			}
		}
	
		// Generate null check branch
		if ptrType, ok := target.Type().(*types.PointerType); ok {
			nullPtr := constant.NewNull(ptrType)
			isNull := currentBlock.NewICmp(enum.IPredEQ, target, nullPtr)
			currentBlock.NewCondBr(isNull, abortBlock, nonNullBlock)
		} else {
			currentBlock.NewBr(nonNullBlock)
		}
		currentBlock = nonNullBlock
	
		// Create object case block
		objectCaseBlock := currentBlock.Parent.NewBlock("case.object")
	
		// Determine if we're dealing with a primitive type or class object
		targetType := g.getClassNameFromType(target.Type())
		isPrimitive := targetType == "Int" || targetType == "Bool" || targetType == "String"
	
		// Filter cases excluding Object case
		var nonObjectCases []*ast.Case
		for _, c := range e.Cases {
			if c.Type.Value != "Object" {
				nonObjectCases = append(nonObjectCases, c)
			}
		}
	
		// Create test blocks for non-Object cases
		var testBlocks []*ir.Block
		for i := range nonObjectCases {
			testBlocks = append(testBlocks, currentBlock.Parent.NewBlock(fmt.Sprintf("case.test.%d", i)))
		}
	
		// Branch to first test block or directly to Object case if no other cases
		if len(testBlocks) > 0 {
			currentBlock.NewBr(testBlocks[0])
		} else {
			currentBlock.NewBr(objectCaseBlock)
		}
	
		// Handle specific type cases
		if len(nonObjectCases) > 0 {
			if isPrimitive {
				// For primitive types, do direct type ID comparison
				for i, caseItem := range nonObjectCases {
					currentBlock = testBlocks[i]
					caseBlock := currentBlock.Parent.NewBlock(fmt.Sprintf("case.%d.body", i))
					
					// Next block is either the next test or the object case
					nextBlock := objectCaseBlock
					if i < len(nonObjectCases)-1 {
						nextBlock = testBlocks[i+1]
					}
	
					// Compare type IDs directly
					typeID := g.typeID(caseItem.Type.Value)
					targetTypeID := g.typeID(targetType)
					cmp := currentBlock.NewICmp(
						enum.IPredEQ,
						constant.NewInt(types.I32, int64(targetTypeID)),
						constant.NewInt(types.I32, int64(typeID)),
					)
	
					currentBlock.NewCondBr(cmp, caseBlock, nextBlock)
	
					// Handle case body
					oldLocals := make(map[string]value.Value)
					oldTypes := make(map[string]string)
					localName := caseItem.Name.Value
	
					// Create a local variable for the case branch
					varAlloca := caseBlock.NewAlloca(target.Type())
					caseBlock.NewStore(target, varAlloca)
					
					// Store previous local if it exists
					if existing, ok := g.locals[localName]; ok {
						oldLocals[localName] = existing
					}
					if existingType, ok := g.localsTypes[localName]; ok {
						oldTypes[localName] = existingType
					}
					
					// Set new local
					g.locals[localName] = varAlloca
					g.localsTypes[localName] = caseItem.Type.Value
	
					// Generate case body
					caseResult, bodyBlock, err := g.generateExpression(caseBlock, caseItem.Expression)
					if err != nil {
						return nil, bodyBlock, err
					}
	
					// Restore previous locals
					if oldVal, exists := oldLocals[localName]; exists {
						g.locals[localName] = oldVal
					} else {
						delete(g.locals, localName)
					}
					if oldType, exists := oldTypes[localName]; exists {
						g.localsTypes[localName] = oldType
					} else {
						delete(g.localsTypes, localName)
					}
	
					bodyBlock.NewBr(endBlock)
					incoming = append(incoming, ir.NewIncoming(caseResult, bodyBlock))
				}
			} else {
				// For object types, check the class hierarchy
				// Get the vtable pointer from the object
				classType := types.NewStruct(types.NewPointer(types.I8)) // Generic object type with vtable ptr
				castedTarget := currentBlock.NewBitCast(target, types.NewPointer(classType))
				vtablePtr := currentBlock.NewGetElementPtr(classType, castedTarget,
					constant.NewInt(types.I32, 0),
					constant.NewInt(types.I32, 0),
				)
				vtablePtrVal := currentBlock.NewLoad(types.NewPointer(types.I8), vtablePtr)
	
				// Sort non-object cases by inheritance depth (most specific first)
				sortedCases := make([]*ast.Case, len(nonObjectCases))
				copy(sortedCases, nonObjectCases)
				sort.Slice(sortedCases, func(i, j int) bool {
					return g.getTypeDepth(sortedCases[i].Type.Value) > g.getTypeDepth(sortedCases[j].Type.Value)
				})
	
				for i, caseItem := range sortedCases {
					currentBlock = testBlocks[i]
					loopBlock := currentBlock.Parent.NewBlock(fmt.Sprintf("case.%d.loop", i))
					matchBlock := currentBlock.Parent.NewBlock(fmt.Sprintf("case.%d.match", i))
					
					// Next block is either the next test or the object case
					nextBlock := objectCaseBlock
					if i < len(sortedCases)-1 {
						nextBlock = testBlocks[i+1]
					}
	
					// Start with current vtable and traverse the inheritance chain
					currentBlock.NewBr(loopBlock)
					
					// Loop block for inheritance traversal
					vtablePhi := loopBlock.NewPhi(ir.NewIncoming(vtablePtrVal, currentBlock))
					
					// Access vtable header to get class name
					vtableHeaderType := types.NewStruct(
						types.NewPointer(types.I8), // class name
						types.NewPointer(types.I8), // parent vtable
					)
					castedVtable := loopBlock.NewBitCast(vtablePhi, types.NewPointer(vtableHeaderType))
					
					// Get class name from vtable
					classNamePtrPtr := loopBlock.NewGetElementPtr(vtableHeaderType, castedVtable,
						constant.NewInt(types.I32, 0),
						constant.NewInt(types.I32, 0),
					)
					classNamePtr := loopBlock.NewLoad(types.NewPointer(types.I8), classNamePtrPtr)
					
					// Compare with case type using strcmp
					typeStrConst := g.getOrCreateStringConstant(caseItem.Type.Value)
					typeStrPtr := loopBlock.NewGetElementPtr(
						types.NewArray(uint64(len(caseItem.Type.Value)+1), types.I8),
						typeStrConst,
						constant.NewInt(types.I32, 0),
						constant.NewInt(types.I32, 0),
					)
					
					// Get or declare strcmp
					var strcmpFunc *ir.Func
					for _, f := range g.module.Funcs {
						if f.Name() == "strcmp" {
							strcmpFunc = f
							break
						}
					}
					if strcmpFunc == nil {
						strcmpFunc = g.module.NewFunc("strcmp", types.I32,
							ir.NewParam("s1", types.NewPointer(types.I8)),
							ir.NewParam("s2", types.NewPointer(types.I8)),
						)
					}
					
					// Call strcmp
					cmpResult := loopBlock.NewCall(strcmpFunc, classNamePtr, typeStrPtr)
					isMatch := loopBlock.NewICmp(enum.IPredEQ, cmpResult, constant.NewInt(types.I32, 0))
					
					// Check if we have a match
					continueBlock := loopBlock.Parent.NewBlock(fmt.Sprintf("case.%d.continue", i))
					loopBlock.NewCondBr(isMatch, matchBlock, continueBlock)
					
					// If no match, check parent vtable
					parentVtablePtrPtr := loopBlock.NewGetElementPtr(vtableHeaderType, castedVtable,
						constant.NewInt(types.I32, 0),
						constant.NewInt(types.I32, 1),
					)
					parentVtablePtr := continueBlock.NewLoad(types.NewPointer(types.I8), parentVtablePtrPtr)
					
					// Check if we've reached Object or null (end of inheritance chain)
					isNull := continueBlock.NewICmp(enum.IPredEQ, parentVtablePtr, 
						constant.NewNull(types.NewPointer(types.I8)))
					isObjectVtable := continueBlock.NewICmp(enum.IPredEQ, parentVtablePtr,
						continueBlock.NewBitCast(g.vtables["Object"], types.NewPointer(types.I8)))
					isEnd := continueBlock.NewOr(isNull, isObjectVtable)
					
					// Branch based on whether we've reached the end
					continueBlock.NewCondBr(isEnd, nextBlock, loopBlock)
					
					// Update the vtable phi node with the parent vtable
					vtablePhi.Incs = append(vtablePhi.Incs, ir.NewIncoming(parentVtablePtr, continueBlock))
					
					// Match block - execute the case body
					// Save old locals
					oldLocals := make(map[string]value.Value)
					oldTypes := make(map[string]string)
					localName := caseItem.Name.Value
					
					// Create a local variable for the case branch
					if existing, ok := g.locals[localName]; ok {
						oldLocals[localName] = existing
					}
					if existingType, ok := g.localsTypes[localName]; ok {
						oldTypes[localName] = existingType
					}
					
					// Set new local - using the target directly (no need for alloca+store)
					g.locals[localName] = target  
					g.localsTypes[localName] = caseItem.Type.Value
					
					// Generate case body
					caseResult, bodyBlock, err := g.generateExpression(matchBlock, caseItem.Expression)
					if err != nil {
						return nil, bodyBlock, err
					}
					
					// Restore previous locals
					if oldVal, exists := oldLocals[localName]; exists {
						g.locals[localName] = oldVal
					} else {
						delete(g.locals, localName)
					}
					if oldType, exists := oldTypes[localName]; exists {
						g.localsTypes[localName] = oldType
					} else {
						delete(g.localsTypes, localName)
					}
					
					bodyBlock.NewBr(endBlock)
					incoming = append(incoming, ir.NewIncoming(caseResult, bodyBlock))
				}
			}
		}
	
		// Always handle Object case
		if objectCase != nil {
			// Set up locals for Object case
			localName := objectCase.Name.Value
			oldLocals := make(map[string]value.Value)
			oldTypes := make(map[string]string)
			
			if existing, ok := g.locals[localName]; ok {
				oldLocals[localName] = existing
			}
			if existingType, ok := g.localsTypes[localName]; ok {
				oldTypes[localName] = existingType
			}
			
			// Set local to target value
			varAlloca := objectCaseBlock.NewAlloca(target.Type())
			objectCaseBlock.NewStore(target, varAlloca)
			g.locals[localName] = varAlloca
			g.localsTypes[localName] = "Object"
			
			// Generate Object case body
			objectResult, objectBodyBlock, err := g.generateExpression(objectCaseBlock, objectCase.Expression)
			if err != nil {
				return nil, objectBodyBlock, err
			}
			
			// Restore locals
			if oldVal, exists := oldLocals[localName]; exists {
				g.locals[localName] = oldVal
			} else {
				delete(g.locals, localName)
			}
			if oldType, exists := oldTypes[localName]; exists {
				g.localsTypes[localName] = oldType
			} else {
				delete(g.localsTypes, localName)
			}
			
			// Branch to end block
			objectBodyBlock.NewBr(endBlock)
			incoming = append(incoming, ir.NewIncoming(objectResult, objectBodyBlock))
		} else {
			// This should never happen with well-typed COOL programs
			objectCaseBlock.NewBr(abortBlock)
			fmt.Printf("WARNING: No Object case found in case expression\n")
		}
		
		// Create final phi node for the result
		if len(incoming) > 0 {
			phi := endBlock.NewPhi(incoming...)
			return phi, endBlock, nil
		} else {
			// This should not happen with well-typed COOL programs
			return constant.NewNull(types.NewPointer(types.I8)), endBlock, nil
		}

	case *ast.StringLiteral:
		// Ensure null termination
		strWithNull := e.Value + "\x00"
		strGlobal := g.getOrCreateStringConstant(strWithNull)

		// Get pointer to first character
		arrType := types.NewArray(uint64(len(strWithNull)), types.I8)
		strPtr := block.NewGetElementPtr(arrType, strGlobal,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, 0),
		)

		// Allocate String object on the heap
		mallocFunc := g.getOrCreateMalloc()
		stringSize := constant.NewInt(types.I64, 16) // Size of String struct
		stringObjMem := block.NewCall(mallocFunc, stringSize)
		stringObj := block.NewBitCast(stringObjMem, types.NewPointer(g.classTypes["String"]))

		// Initialize vtable
		vtablePtr := block.NewGetElementPtr(g.classTypes["String"], stringObj,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, 0),
		)
		block.NewStore(
			block.NewBitCast(g.vtables["String"], types.NewPointer(types.I8)),
			vtablePtr,
		)

		// Set value field
		valuePtr := block.NewGetElementPtr(g.classTypes["String"], stringObj,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, 1),
		)
		block.NewStore(strPtr, valuePtr)

		return stringObj, block, nil
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
		case "=":
			cmp := rightBlock.NewICmp(enum.IPredEQ, left, right)
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

		// NEW: Record the variable's type in localsTypes
		oldType, typeExists := g.localsTypes[e.Name.Value]
		g.localsTypes[e.Name.Value] = e.Type.Value

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

		// NEW: Restore the old type
		if typeExists {
			g.localsTypes[e.Name.Value] = oldType
		} else {
			delete(g.localsTypes, e.Name.Value)
		}

		return bodyValue, bodyBlock, nil
	case *ast.ObjectIdentifier:
		fmt.Printf("\nDEBUG: Accessing identifier: %s\n", e.Value)

		if e.Value == "self" {
			if selfVal, exists := g.locals["self"]; exists {
				return selfVal, block, nil
			}
			return nil, block, fmt.Errorf("self not found in locals")
		}

		// Existing code for other variables...
		if varAlloca, exists := g.locals[e.Value]; exists {
			fmt.Printf("DEBUG: Found in locals at: %v\n", varAlloca)

			ptrType, ok := varAlloca.Type().(*types.PointerType)
			if !ok {
				return nil, block, fmt.Errorf("unexpected type for variable: %s", e.Value)
			}
			loadInst := block.NewLoad(ptrType.ElemType, varAlloca)
			return loadInst, block, nil
		}
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
			fmt.Printf("DEBUG: Looking for attribute in class %s\n", g.currentClass)

			var attrInfo *AttributeInfo
			for i := range classInfo.Attributes {
				attr := &classInfo.Attributes[i] // Get pointer to the actual element
				fmt.Printf("DEBUG: Checking attribute %s at offset %d\n", attr.Name, attr.Offset)
				if attr.Name == e.Value {
					attrInfo = attr
					break
				}
			}
			if attrInfo != nil {
				fmt.Printf("DEBUG: Found attribute %s at offset %d\n", attrInfo.Name, attrInfo.Offset)

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
				fmt.Printf("DEBUG: Generated GEP instruction for attribute access at offset %d\n", attrInfo.Offset)
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

		// Special handling for type_name method on primitive types
		if e.Method.Value == "type_name" {
			// For primitive types (Bool, Int), use Object's type_name implementation
			
			methodName := "Object_type_name"
			methodFunc := g.methods[methodName]

			if methodFunc == nil {
				return nil, block, fmt.Errorf("method %s not found", methodName)
			}
		

			// Get the type name string
			typeNameStr := g.getOrCreateStringConstant(receiverType)

			// Create new String object
			stringType := g.classTypes["String"]
			stringObj := block.NewAlloca(stringType)

			// Set vtable for String object
			stringVtablePtr := block.NewGetElementPtr(stringType, stringObj,
				constant.NewInt(types.I32, 0),
				constant.NewInt(types.I32, 0),
			)
			block.NewStore(
				block.NewBitCast(g.vtables["String"], types.NewPointer(types.I8)),
				stringVtablePtr,
			)

			// Set string value (type name)
			valuePtr := block.NewGetElementPtr(stringType, stringObj,
				constant.NewInt(types.I32, 0),
				constant.NewInt(types.I32, 1),
			)

			var strPtr value.Value
			if arrType, ok := typeNameStr.Type().(*types.ArrayType); ok {
				strPtr = block.NewGetElementPtr(arrType, typeNameStr,
					constant.NewInt(types.I32, 0),
					constant.NewInt(types.I32, 0),
				)
			} else {
				strPtr = block.NewBitCast(typeNameStr, types.NewPointer(types.I8))
			}
			block.NewStore(strPtr, valuePtr)

			// If this is being used as an argument to out_int, return the length of the type name
			if parent, ok := expr.(*ast.MethodCall); ok && parent.Method.Value == "out_int" {
				// Get strlen function
				var strlen *ir.Func
				for _, f := range g.module.Funcs {
					if f.Name() == "strlen" {
						strlen = f
						break
					}
				}
				if strlen == nil {
					strlen = g.module.NewFunc("strlen", types.I64,
						ir.NewParam("str", types.NewPointer(types.I8)))
				}

				// Get length of the type name string
				length := block.NewCall(strlen, strPtr)
				// Convert length to i32
				return block.NewTrunc(length, types.I32), block, nil
			}

			// Otherwise return the String object
			// result := block.NewBitCast(stringObj, types.NewPointer(types.I8))
			return stringObj, block, nil
		}
		//copy method
		if e.Method.Value == "copy" {
			methodName := fmt.Sprintf("%s_copy", receiverType)
			methodFunc := g.methods[methodName]
			if methodFunc == nil {
				return nil, block, fmt.Errorf("method %s not found", methodName)
			}
			return receiverObj, block, nil
		}

		// Rest of the method call handling remains the same...
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
		if receiverType == "Int" || receiverType == "Bool" {
			args = append(args, receiverObj)
		} else {
			args = append(args, block.NewBitCast(receiverObj, types.I8Ptr))
		}
		for _, arg := range e.Arguments {
			argVal, newBlock, err := g.generateExpression(block, arg)
			if err != nil {
				return nil, newBlock, err
			}
			block = newBlock
	
			// Cast object arguments to i8*
			if ptrType, ok := argVal.Type().(*types.PointerType); ok {
				if _, isObject := g.classTypes[g.getClassNameFromType(ptrType)]; isObject {
					argVal = block.NewBitCast(argVal, types.I8Ptr)
				}
			}
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

		// Initialize attributes using class-defined initializers
		for _, attr := range info.Attributes {
			// Find the AST attribute to get the initializer expression
			var attrAST *ast.Attribute
			for _, class := range g.program.Classes {
				if class.Name.Value == e.Type.Value {
					for _, feature := range class.Features {
						if a, ok := feature.(*ast.Attribute); ok && a.Name.Value == attr.Name {
							attrAST = a
							break
						}
					}
					if attrAST != nil {
						break
					}
				}
			}

			// Get pointer to the current attribute
			attrPtr := block.NewGetElementPtr(classType, objPtr,
				constant.NewInt(types.I32, 0),
				constant.NewInt(types.I32, int64(attr.Offset)),
			)

			// Generate code for the initializer if present
			var initExpr ast.Expression
			if attrAST != nil {
				initExpr = attrAST.Init
			}

			var err error
			block, err = g.initializeAttribute(block, attrPtr, attr.Type, initExpr)
			if err != nil {
				return nil, block, fmt.Errorf("error initializing attribute %s: %v", attr.Name, err)
			}
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

// findObjectCase checks if there's a case for the Object type
func findObjectCase(cases []*ast.Case) *ast.Case {
    for _, c := range cases {
        if c.Type.Value == "Object" {
            return c
        }
    }
    return nil
}
func (g *CodeGenerator) typeID(typeName string) int {
	switch typeName {
	case "Int":
		return 1
	case "Bool":
		return 2
	case "String":
		return 3
	default:
		return 0
	}
}

// determineCommonType analyzes a list of values and determines a common type they can all be cast to
func (g *CodeGenerator) determineCommonType(values []value.Value) types.Type {
	if len(values) == 0 {
		return types.NewPointer(types.I8) // Default to i8* as fallback
	}

	// Start with the first type
	commonType := values[0].Type()

	// If all values have the same type, just return that type
	allSameType := true
	for _, val := range values {
		if !val.Type().Equal(commonType) {
			allSameType = false
			break
		}
	}
	if allSameType {
		return commonType
	}

	// If we have mixed pointer and non-pointer types, choose i8*
	hasPointer := false
	hasNonPointer := false
	for _, val := range values {
		if _, isPtr := val.Type().(*types.PointerType); isPtr {
			hasPointer = true
		} else {
			hasNonPointer = true
		}
	}
	if hasPointer && hasNonPointer {
		return types.NewPointer(types.I8)
	}

	// If all are pointers, choose i8* as the common type
	if hasPointer && !hasNonPointer {
		return types.NewPointer(types.I8)
	}

	// If all are integers (i1, i32), choose the larger type
	maxBitWidth := uint64(0)
	allIntegers := true

	for _, val := range values {
		if intType, isInt := val.Type().(*types.IntType); isInt {
			if intType.BitSize > maxBitWidth {
				maxBitWidth = intType.BitSize
			}
		} else {
			allIntegers = false
			break
		}
	}

	if allIntegers {
		return types.NewInt(maxBitWidth)
	}

	// Default to i8* as the most generic type
	return types.NewPointer(types.I8)
}

// castToType casts a value to the target type if needed
func (g *CodeGenerator) castToType(block *ir.Block, val value.Value, targetType types.Type) value.Value {
	// If types are already the same, no need to cast
	if val.Type().Equal(targetType) {
		return val
	}

	// Handle pointer-to-pointer casts
	if ptrTarget, isTargetPtr := targetType.(*types.PointerType); isTargetPtr {
		if _, isValPtr := val.Type().(*types.PointerType); isValPtr {
			return block.NewBitCast(val, ptrTarget)
		}
	}

	// Handle integer-to-integer casts
	if intTarget, isTargetInt := targetType.(*types.IntType); isTargetInt {
		if intVal, isValInt := val.Type().(*types.IntType); isValInt {
			if intTarget.BitSize > intVal.BitSize {
				return block.NewZExt(val, intTarget)
			} else if intTarget.BitSize < intVal.BitSize {
				return block.NewTrunc(val, intTarget)
			}
		}
	}

	// If we can't cast properly, log a warning and use bitcast as a last resort
	fmt.Printf("WARNING: Attempting unusual cast from %v to %v\n", val.Type(), targetType)
	return block.NewBitCast(val, targetType)
}

func (g *CodeGenerator) getTypeDepth(typeName string) int {
    depth := 0
    current := typeName
    for {
        info, exists := g.classTable[current]
        if !exists || info.Parent == "" {
            break
        }
        depth++
        current = info.Parent
    }
    return depth
}
// findCommonAncestor finds the common ancestor in the class hierarchy
func (g *CodeGenerator) findCommonAncestor(classNames []string) string {
	if len(classNames) == 0 {
		return ""
	}
	if len(classNames) == 1 {
		return classNames[0]
	}

	// Get the ancestor chain for the first class
	firstClass := classNames[0]
	ancestors := []string{firstClass}
	current := firstClass

	for {
		info, exists := g.classTable[current]
		if !exists || info.Parent == "" {
			break
		}
		ancestors = append(ancestors, info.Parent)
		current = info.Parent
	}

	// Check each ancestor against other classes
	for _, ancestor := range ancestors {
		isCommonAncestor := true

		for i := 1; i < len(classNames); i++ {
			if !g.isSubclass(classNames[i], ancestor) && classNames[i] != ancestor {
				isCommonAncestor = false
				break
			}
		}

		if isCommonAncestor {
			return ancestor
		}
	}

	// If no common ancestor found (should not happen in COOL), return Object
	return "Object"
}

func (g *CodeGenerator) getOrCreateStringConstant(name string) *ir.Global {
	sanitizedName := strings.Map(func(r rune) rune {
		if !unicode.IsLetter(r) && !unicode.IsNumber(r) {
			return '_'
		}
		return r
	}, name)
	strConstName := fmt.Sprintf(".str.%s", sanitizedName)

	for _, global := range g.module.Globals {
		if global.Name() == strConstName {
			return global
		}
	}

	// Add null terminator
	strWithNull := name + "\x00"
	strConst := g.module.NewGlobalDef(strConstName, constant.NewCharArray([]byte(strWithNull)))
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
			&ast.Method{
				Token:      lexer.Token{Literal: "method"},
				Name:       &ast.ObjectIdentifier{Token: lexer.Token{Literal: "copy"}, Value: "copy"},
				Parameters: []*ast.Formal{},
				ReturnType: &ast.TypeIdentifier{Token: lexer.Token{Literal: "SELF_TYPE"}, Value: "SELF_TYPE"},
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
	// Prepend built-in classes to the program
	program.Classes = append([]*ast.Class{stringClass, ioClass, objectClass, boolClass, intClass}, program.Classes...)
}

func (g *CodeGenerator) initializeAttribute(block *ir.Block, attrPtr value.Value, attrType string, initExpr ast.Expression) (*ir.Block, error) {

	// Get the destination type
	destType := attrPtr.Type().(*types.PointerType).ElemType

	if initExpr != nil {
		val, currentBlock, err := g.generateExpression(block, initExpr)
		if err != nil {
			return currentBlock, err
		}

		// Handle String type specially
		if g.getClassNameFromType(destType) == "String" {
			stringType := g.classTypes["String"]

			// If we have a raw i8* pointer, wrap it in a String object
			if val.Type().Equal(types.I8Ptr) {
				// Create new String object
				newString := currentBlock.NewAlloca(stringType)

				// Set vtable
				vtablePtr := currentBlock.NewGetElementPtr(stringType, newString,
					constant.NewInt(types.I32, 0),
					constant.NewInt(types.I32, 0),
				)
				currentBlock.NewStore(
					currentBlock.NewBitCast(g.vtables["String"], types.I8Ptr),
					vtablePtr,
				)

				// Set value field
				valuePtr := currentBlock.NewGetElementPtr(stringType, newString,
					constant.NewInt(types.I32, 0),
					constant.NewInt(types.I32, 1),
				)
				currentBlock.NewStore(val, valuePtr)

				val = newString
			}

			// Cast to destination type if needed
			if !val.Type().Equal(destType) {
				val = currentBlock.NewBitCast(val, destType)
			}

			currentBlock.NewStore(val, attrPtr)
			return currentBlock, nil
		}

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
		emptyStr := g.getOrCreateStringConstant("\x00")
		strPtr := block.NewGetElementPtr(
			types.NewArray(1, types.I8),
			emptyStr,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, 0),
		)
		defaultVal = strPtr

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

func (g *CodeGenerator) getOrCreateMalloc() *ir.Func {
	// Look for existing malloc function
	for _, f := range g.module.Funcs {
		if f.Name() == "malloc" {
			return f
		}
	}
	// Create new malloc function if not found
	return g.module.NewFunc("malloc", types.I8Ptr, ir.NewParam("size", types.I64))
}
