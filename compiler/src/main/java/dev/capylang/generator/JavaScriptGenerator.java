package dev.capylang.generator;

import dev.capylang.compiler.CompiledProgram;

import java.util.List;

public final class JavaScriptGenerator implements Generator {
    @Override
    public GeneratedProgram generate(CompiledProgram program) {
        if (!program.objectOrientedModules().isEmpty()) {
            throw new IllegalStateException("Object-oriented `.coo` generation is only supported for JAVA");
        }
        return new GeneratedProgram(List.of());
    }
}
