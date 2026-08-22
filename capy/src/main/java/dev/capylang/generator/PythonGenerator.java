package dev.capylang.generator;

import dev.capylang.compiler.CompiledProgram;
import dev.capylang.generator.internal.GeneratedPythonGenerator;

/** Bootstrap-compatible entry point for the self-hosted Python generator. */
public final class PythonGenerator {
    private PythonGenerator() {
    }

    public static GeneratedProgram pythonGenerator(CompiledProgram program) {
        var sourceProgram = JavaGenerator.dataMap(JavaGenerator.toGeneratedValue(program));
        var lookupProgram = JavaGenerator.withBundledImportModules(
                JavaGenerator.dataMap(JavaGenerator.toGeneratedValue(program))
        );
        var generated = JavaGenerator.dataMap(GeneratedPythonGenerator.python_generator_with_lookup__85_0(
                sourceProgram,
                lookupProgram
        ));
        var modules = JavaGenerator.list(generated.get("modules")).stream()
                .map(JavaGenerator::generatedModule)
                .toList();
        return new GeneratedProgram(modules);
    }
}
