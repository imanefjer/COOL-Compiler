package codegen

import (
	"cool-compiler/ast"
	"fmt"

	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/enum"
	"github.com/llir/llvm/ir/types"
	"github.com/llir/llvm/ir/value"
)

// generateClass handles code generation for a single class
func (g *CodeGenerator) generateClass(class *ast.Class) error {
	g.currentClass = class.Name.Value
	defer func() { g.currentClass = "" }()

	for _, feature := range class.Features {
		if method, ok := feature.(*ast.Method); ok {
			if err := g.generateMethod(class.Name.Value, method); err != nil {
				return err
			}
		}
	}
	return nil
}
func (g *CodeGenerator) generateMethod(className string, method *ast.Method) error {
	// Reset local variables for this method
	g.locals = make(map[string]value.Value)
	g.localsTypes = make(map[string]string)

	methodName := fmt.Sprintf("%s_%s", className, method.Name.Value)
	fn, ok := g.methods[methodName]
	if !ok {
		return fmt.Errorf("method %s not found", methodName)
	}

	entryBlock := fn.NewBlock("entry")
	
	// Handle common built-in methods first
	switch method.Name.Value {
	case "copy":
		return g.generateMethodCopy(className,entryBlock)
	case "type_name":
		return g.generateMethodTypeName(className, fn, entryBlock)
	case "abort":
		return g.generateMethodAbort(entryBlock)
	}
	
	// Handle class-specific built-in methods
	if className == "String" {
		return g.generateStringMethod(method.Name.Value, fn, entryBlock)
	} else if className == "IO" {
		return g.generateIOMethod(method.Name.Value, fn, entryBlock)
	}

	// Normal method generation for non built-in methods:
	return g.generateRegularMethod(className, method, fn, entryBlock)
}

func (g *CodeGenerator) generateMethodCopy(className string, entryBlock *ir.Block) error {
	classType := g.classTypes[className]

	// Allocate new object
	newObj := entryBlock.NewAlloca(classType)
	newObjI8 := entryBlock.NewBitCast(newObj, i8Ptr)

	// Copy vtable from self
	self := entryBlock.Parent.Params[0]
	selfPtr := entryBlock.NewBitCast(self, types.NewPointer(classType))
	vtablePtr := entryBlock.NewGetElementPtr(classType, selfPtr, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
	vtableVal := entryBlock.NewLoad(types.NewPointer(types.I8), vtablePtr)
	newVtablePtr := entryBlock.NewGetElementPtr(classType, newObj, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
	entryBlock.NewStore(vtableVal, newVtablePtr)

	// Copy each attribute
	classInfo := g.classTable[className]
	for _, attr := range classInfo.Attributes {
		structFieldIndex := attr.Offset // Offset starts at 1 (vtable is at 0)
		attrSrcPtr := entryBlock.NewGetElementPtr(classType, selfPtr,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, int64(structFieldIndex)),
		)
		
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

func (g *CodeGenerator) generateMethodTypeName(className string, fn *ir.Func, entryBlock *ir.Block) error {
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
		entryBlock.NewBitCast(g.vtables["String"], i8Ptr),
		stringVtablePtr,
	)

	// Set string value (class name)
	valuePtr := entryBlock.NewGetElementPtr(stringType, stringObj,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 1),
	)
	entryBlock.NewStore(classNamePtr, valuePtr)

	// Return String object
	result := entryBlock.NewBitCast(stringObj, i8Ptr)
	entryBlock.NewRet(result)
	return nil
}

func (g *CodeGenerator) generateMethodAbort( entryBlock *ir.Block) error {
	abort := g.getOrCreateAbort()
	entryBlock.NewCall(abort)
	entryBlock.NewUnreachable()
	return nil
}

func (g *CodeGenerator) generateStringMethod(methodName string, fn *ir.Func, entryBlock *ir.Block) error {
	switch methodName {
	case "concat":
		return g.generateStringConcat(fn, entryBlock)
	case "substr":
		return g.generateStringSubstr(fn, entryBlock)
	case "length":
		return g.generateStringLength(fn, entryBlock)
	default:
		// Handle regular method for String class
		return fmt.Errorf("unknown String method: %s", methodName)
	}
}

func (g *CodeGenerator) generateStringConcat(fn *ir.Func, entryBlock *ir.Block) error {
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
	strlen := g.getOrCreateStrlen()
	malloc := g.getOrCreateMalloc()
	
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

func (g *CodeGenerator) generateStringSubstr(fn *ir.Func, entryBlock *ir.Block) error {
	self := fn.Params[0]
	iParam := fn.Params[1]
	lParam := fn.Params[2]

	stringType := g.classTypes["String"]
	selfPtr := entryBlock.NewBitCast(self, types.NewPointer(stringType))
	valuePtr := entryBlock.NewGetElementPtr(stringType, selfPtr, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 1))
	originalStr := entryBlock.NewLoad(types.I8Ptr, valuePtr)

	// Declare strlen if not present
	strlenFunc := g.getOrCreateStrlen()
	
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
	abort := g.getOrCreateAbort()
	
	errorBlock.NewCall(abort)
	errorBlock.NewUnreachable()

	// Main block: proceed with substring extraction
	srcPtr := mainBlock.NewGetElementPtr(types.I8, originalStr, i64_i)

	// Allocate memory for new substring
	mallocFunc :=  g.getOrCreateMalloc()

	bufferSize := mainBlock.NewAdd(i64_l, constant.NewInt(types.I64, 1))
	newBuf := mainBlock.NewCall(mallocFunc, bufferSize)

	// Declare memcpy to copy the substring
	memcpy := g.getOrCreateMemcpy()

	mainBlock.NewCall(memcpy, newBuf, srcPtr, i64_l)

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

func (g *CodeGenerator) generateStringLength(fn *ir.Func, entryBlock *ir.Block) error {
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
	strlen := g.getOrCreateStrlen()
	lenCall := entryBlock.NewCall(strlen, strPtr)

	// Truncate to Int (i32)
	result := entryBlock.NewTrunc(lenCall, types.I32)
	entryBlock.NewRet(result)
	return nil
}

func (g *CodeGenerator) generateIOMethod(methodName string, fn *ir.Func, entryBlock *ir.Block) error {
	switch methodName {
	case "in_string":
		return g.generateIOInString(fn, entryBlock)
	case "in_int":
		return g.generateIOInInt(fn, entryBlock)
	case "out_string":
		return g.generateIOOutString(fn, entryBlock)
	case "out_int":
		return g.generateIOOutInt(fn, entryBlock)
	default:
		// Handle regular method for IO class
		return fmt.Errorf("unknown IO method: %s", methodName)
	}
}

func (g *CodeGenerator) generateIOInString(fn *ir.Func, entryBlock *ir.Block) error {
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
	fgetsFn := g.getOrCreateFgets(fileType)

	stdinVal := entryBlock.NewLoad(types.NewPointer(fileType), stdinGlobal)
	buffer := entryBlock.NewCall(mallocFunc, constant.NewInt(types.I64, 1024))
	_ = entryBlock.NewCall(fgetsFn, buffer, constant.NewInt(types.I32, 1024), stdinVal)
	
	// Remove newline character if present
	strchrFn := g.getOrCreateStrchr()

	// Find newline
	newline := entryBlock.NewCall(strchrFn, buffer, constant.NewInt(types.I32, '\n'))
	foundBlock := fn.NewBlock("in_string.found_newline")
	continueBlock := fn.NewBlock("in_string.continue")

	isNewline := entryBlock.NewICmp(enum.IPredNE, newline, constant.NewNull(types.I8Ptr))
	entryBlock.NewCondBr(isNewline, foundBlock, continueBlock) // Terminator for entry block

	// Handle foundBlock 
	foundBlock.NewStore(constant.NewInt(types.I8, 0), newline)
	foundBlock.NewBr(continueBlock) // Terminator for foundBlock

	// Handle continueBlock 
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

func (g *CodeGenerator) generateIOInInt(fn *ir.Func, entryBlock *ir.Block) error {
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

func (g *CodeGenerator) generateIOOutString(fn *ir.Func, entryBlock *ir.Block) error {
	self := fn.Params[0]
	str := fn.Params[1]

	printf := g.getOrCreatePrintf()

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

func (g *CodeGenerator) generateIOOutInt(fn *ir.Func, entryBlock *ir.Block) error {
	self := fn.Params[0]
	intArg := fn.Params[1]

	printf := g.getOrCreatePrintf()


	formatGlobal := g.getOrCreateStringConstant("%d")
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
func (g *CodeGenerator) generateRegularMethod(className string, method *ast.Method, fn *ir.Func, entryBlock *ir.Block) error {
    // Save the current locals and types to restore later
    oldLocals := make(map[string]value.Value)
    oldTypes := make(map[string]string)
    
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
        
        // Save old value for this parameter name if it exists
        if oldVal, exists := g.locals[astParam.Name.Value]; exists {
            oldLocals[astParam.Name.Value] = oldVal
        }
        if oldType, exists := g.localsTypes[astParam.Name.Value]; exists {
            oldTypes[astParam.Name.Value] = oldType
        }
        
        llvmParam := fn.Params[i+1] // i+1 because 0 is self

        // Allocate space for the parameter
        paramAlloca := entryBlock.NewAlloca(llvmParam.Type())
        entryBlock.NewStore(llvmParam, paramAlloca)

        // Add to locals with the AST parameter name
        g.locals[astParam.Name.Value] = paramAlloca
        g.localsTypes[astParam.Name.Value] = astParam.Type.Value
    }
    
    // Ensure all class attributes are added to locals (but don't override parameters)
    classInfo := g.classTable[className]
    for _, attr := range classInfo.Attributes {
        // Skip if a parameter with this name already exists
        if _, exists := g.locals[attr.Name]; exists && oldLocals[attr.Name] == nil {
            continue
        }
        
        // Calculate field index (offset includes vtable pointer at 0)
        fieldIndex := attr.Offset

        // Get pointer to attribute using classType
        attrPtr := entryBlock.NewGetElementPtr(
            classType,
            typedSelf,
            constant.NewInt(types.I32, 0),
            constant.NewInt(types.I32, int64(fieldIndex)),
        )

        // Store in locals using actual attribute name (if not shadowed)
        if _, exists := g.locals[attr.Name]; !exists {
            g.locals[attr.Name] = attrPtr
            g.localsTypes[attr.Name] = attr.Type
        }
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
    
    // Restore original locals
    for name, val := range oldLocals {
        g.locals[name] = val
    }
    for name, typ := range oldTypes {
        g.localsTypes[name] = typ
    }

    return nil
}