package dev.capylang.generator;

import dev.capylang.compiler.CompiledProgram;
import dev.capylang.generator.internal.GeneratedPythonGenerator;

/** Bootstrap-compatible entry point for the self-hosted Python generator. */
public final class PythonGenerator {
    private PythonGenerator() {
    }

    public static GeneratedProgram pythonGenerator(CompiledProgram program) {
        var compiledProgram = JavaGenerator.withBundledImportModules(
                JavaGenerator.dataMap(JavaGenerator.toGeneratedValue(program))
        );
        var generated = JavaGenerator.dataMap(GeneratedPythonGenerator.python_generator__81_0(compiledProgram));
        var modules = JavaGenerator.list(generated.get("modules")).stream()
                .map(JavaGenerator::generatedModule)
                .toList();
        return new GeneratedProgram(modules);
    }
}
