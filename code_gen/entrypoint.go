// entrypoint.go
package code_gen

import (
	"fmt"

	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/types"
)

// getGlobal searches for a global variable by name.
func getGlobal(mod *ir.Module, name string) *ir.Global {
	for _, g := range mod.Globals {
		if g.Name() == name {
			return g
		}
	}
	return nil
}

// DefineEntryPoint defines the program entrypoint.
func DefineEntryPoint(mod *ir.Module, ct *ClassTable) error {
	mainInfo, ok := ct.Classes["Main"]
	if !ok {
		return nil
	}
	if _, hasMain := mainInfo.Methods["main"]; !hasMain {
		return nil
	}

	// define "main" => i32 main()
	fn := mod.NewFunc("main", types.I32)
	blk := fn.NewBlock("entry")

	// Alloca a "Main" struct
	mainStruct := llvmClassTypes["Main"]
	if mainStruct == nil {
		return fmt.Errorf("no struct for Main")
	}
	local := blk.NewAlloca(mainStruct)
	ptr := blk.NewBitCast(local, types.NewPointer(mainStruct))

	// Initialize vtable (first field is i8**)
	vtableGlobal := getGlobal(mod, mainStruct.Name()+"_vtable")
	if vtableGlobal == nil {
		return fmt.Errorf("vtable for Main not found")
	}
	vtablePtr := blk.NewBitCast(vtableGlobal, types.NewPointer(types.NewPointer(types.I8)))
	blk.NewStore(vtablePtr, blk.NewGetElementPtr(mainStruct, ptr, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0)))

	// call Main_main
	fullName := "Main_main"
	calledFn := methodFuncs[fullName]
	if calledFn == nil {
		return fmt.Errorf("no function for Main_main")
	}
	blk.NewCall(calledFn, ptr)

	// return 0
	blk.NewRet(constant.NewInt(types.I32, 0))
	return nil
}
