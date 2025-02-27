package codegen

import (
	"cool-compiler/ast"
	"fmt"

	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/types"
	"github.com/llir/llvm/ir/value"
)

type CodeGenerator struct {
	module       *ir.Module
	classTypes   map[string]*types.StructType
	vtables      map[string]*ir.Global
	methods      map[string]*ir.Func
	locals       map[string]value.Value
	localsTypes  map[string]string
	currentClass string
	classAttrs   map[string][]string
	classTable   ClassTable
	program      *ast.Program
}

func NewCodeGenerator() *CodeGenerator {
	module := ir.NewModule()
	module.TargetTriple = "arm64-apple-macosx"
	return &CodeGenerator{
		module:      module,
		classTypes:  make(map[string]*types.StructType),
		vtables:     make(map[string]*ir.Global),
		methods:     make(map[string]*ir.Func),
		locals:      make(map[string]value.Value),
		localsTypes: make(map[string]string),
		classTable:  make(ClassTable),
	}
}

func (g *CodeGenerator) Generate(program *ast.Program) (*ir.Module, error) {
	g.program = program
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
	// Create Object type
	objectType := types.NewStruct(
		types.NewPointer(types.I8),
	)
	g.classTypes["Object"] = objectType

	// Initialize maps
	g.classAttrs = make(map[string][]string)
	classAttributes := make(map[string][]*ast.Attribute)

	// Collect attribute names and attribute nodes
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
	// Create LLVM struct types for each class
	for _, class := range classes {
		// Start with the vtable pointer field
		fields := []types.Type{types.NewPointer(types.I8)}
		// For each attribute declared in this class, select its LLVM type based on its declared type
		attrs := classAttributes[class.Name.Value]
		for _, attr := range attrs {
			fieldType := g.convertType(attr.Type.Value)
			fields = append(fields, fieldType)
		}
		classType := types.NewStruct(fields...)
		g.classTypes[class.Name.Value] = classType
	}

	for _, class := range classes {
		for _, feature := range class.Features {
			if method, ok := feature.(*ast.Method); ok {
				methodName := fmt.Sprintf("%s_%s", class.Name.Value, method.Name.Value)
				var paramTypes []types.Type
				var retType types.Type
				
				// Handle return types based on declared type
				switch method.ReturnType.Value {
				case "Int":
					retType = types.I32
				case "Bool":
					retType = types.I1
				default:
					retType = i8Ptr
				}

				// First parameter is always self
				paramTypes = append(paramTypes, i8Ptr)

				// Add other parameters
				for _, param := range method.Parameters {
					paramType := g.convertType(param.Type.Value)
        			paramTypes = append(paramTypes, paramType)
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
		block.NewBitCast(g.vtables["Main"], i8Ptr),
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
	mainObjCast := block.NewBitCast(mainObj, i8Ptr)
	mainMethod := g.methods["Main_main"]
	block.NewCall(mainMethod, mainObjCast)

	block.NewRet(constant.NewInt(types.I32, 0))
	return nil
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
				fieldType = i8Ptr
			} else {
				fieldType = g.convertType(attr.Type)

			}
			fields = append(fields, fieldType)
		}

		classType := types.NewStruct(fields...)
		g.classTypes[className] = classType
	}
	return nil
}
