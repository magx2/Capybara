package dev.capylang.generator;

import dev.capylang.compiler.CompiledProgram;

import java.util.List;

public final class JavaScriptGenerator implements Generator {
    @Override
    public GeneratedProgram generate(CompiledProgram program) {
        return new GeneratedProgram(List.of());
    }
}
