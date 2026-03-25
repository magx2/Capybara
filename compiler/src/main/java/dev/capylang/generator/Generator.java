package dev.capylang.generator;

import dev.capylang.compiler.OutputType;
import dev.capylang.compiler.CompiledProgram;

public sealed interface Generator permits JavaGenerator, JavaScriptGenerator, PythonGenerator {
    static Generator findGenerator(OutputType type) {
        return switch (type) {
            case JAVA -> new JavaGenerator();
            case JAVASCRIPT -> new JavaScriptGenerator();
            case PYTHON -> new PythonGenerator();
        };
    }

    GeneratedProgram generate(CompiledProgram program);
}
