package dev.capylang.compiler.ir;

final class CoreIrPipeline {
    private CoreIrPipeline() {
    }

    static CoreIr.FunctionalProgram passThrough(CoreIr.FunctionalProgram program) {
        return CoreIr.passThroughFunctionalProgram(program);
    }

    static CoreIr.ObjectOrientedModule passThrough(CoreIr.ObjectOrientedModule module) {
        return CoreIr.passThroughObjectOrientedModule(module);
    }

    static CoreIr.CompiledProgram passThrough(CoreIr.CompiledProgram program) {
        return CoreIr.passThroughCompiledProgram(program);
    }

    static CoreIr.GeneratedProgram passThrough(CoreIr.GeneratedProgram program) {
        return CoreIr.passThroughGeneratedProgram(program);
    }
}
