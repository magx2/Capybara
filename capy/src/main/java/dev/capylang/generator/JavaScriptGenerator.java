package dev.capylang.generator;

import dev.capylang.compiler.CompiledProgram;
import dev.capylang.generator.internal.GeneratedJavaScriptGenerator;

/** Bootstrap-compatible entry point for the self-hosted JavaScript generator. */
public final class JavaScriptGenerator {
    private JavaScriptGenerator() {
    }

    public static GeneratedProgram javaScriptGenerator(CompiledProgram program) {
        var compiledProgram = JavaGenerator.dataMap(JavaGenerator.toGeneratedValue(program));
        var generated = JavaGenerator.dataMap(
                GeneratedJavaScriptGenerator.java_script_generator__75_0(compiledProgram)
        );
        var modules = JavaGenerator.list(generated.get("modules")).stream()
                .map(JavaGenerator::generatedModule)
                .toList();
        return new GeneratedProgram(modules);
    }
}
