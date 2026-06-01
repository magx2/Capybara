package dev.capylang.generator;

import dev.capylang.compiler.OutputType;
import dev.capylang.compiler.CompiledProgram;

import java.nio.file.Path;

public sealed interface Generator permits JavaGenerator, JavaScriptGenerator, PythonGenerator {
    static Generator findGenerator(OutputType type) {
        return switch (type) {
            case JAVA -> new JavaGenerator();
            case JAVASCRIPT -> new JavaScriptGenerator();
            case PYTHON -> new PythonGenerator();
        };
    }

    static String generatedRelativePath(Path path) {
        return path.toString().replace('\\', '/');
    }

    GeneratedProgram generate(CompiledProgram program);
}
