package dev.capylang.generator;

import dev.capylang.AsyncTasks;
import dev.capylang.compiler.BackendCompilationContext;
import dev.capylang.compiler.CompiledProgram;
import dev.capylang.generator.internal.GeneratedJavaScriptGenerator;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;

/** Bootstrap-compatible entry point for the self-hosted JavaScript generator. */
public final class JavaScriptGenerator {
    private static final ThreadLocal<ScopedProgram> MAIN_PROGRAM_CONTEXT = new ThreadLocal<>();
    private static final ThreadLocal<CachedGeneration> LAST_GENERATION = new ThreadLocal<>();

    private JavaScriptGenerator() {
    }

    public static GeneratedProgram javaScriptGenerator(CompiledProgram program) {
        var invocation = BackendCompilationContext.generationInvocation().orElse(null);
        var scopedContext = MAIN_PROGRAM_CONTEXT.get();
        var mainContext = scopedContext != null && scopedContext.invocation() == invocation
                ? scopedContext.program()
                : null;
        var testGeneration = JavaGenerator.separateTestGeneration(program, mainContext);
        if (testGeneration.isPresent()) {
            MAIN_PROGRAM_CONTEXT.remove();
            var programs = testGeneration.get();
            return javaScriptGeneratorWithLookup(programs.source(), programs.lookup());
        }
        if (invocation != null && program.modules().stream().anyMatch(module -> !module.name().endsWith(".test"))) {
            MAIN_PROGRAM_CONTEXT.set(new ScopedProgram(invocation, program));
        }
        return javaScriptGeneratorWithLookup(program, program);
    }

    public static GeneratedProgram javaScriptGeneratorWithLookup(
            CompiledProgram source,
            CompiledProgram lookup
    ) {
        var sameProgram = source == lookup;
        source = JavaGenerator.deduplicateProgram(source);
        lookup = sameProgram ? source : JavaGenerator.deduplicateProgram(lookup);
        var cached = LAST_GENERATION.get();
        if (cached != null && cached.source().equals(source) && cached.lookup().equals(lookup)) {
            return cached.generated();
        }
        var sourceProgram = JavaGenerator.dataMap(JavaGenerator.toGeneratedValue(source));
        var lookupProgram = JavaGenerator.withBundledImportModules(source == lookup
                ? sourceProgram
                : JavaGenerator.dataMap(JavaGenerator.toGeneratedValue(lookup)));
        var context = GeneratedJavaScriptGenerator.java_script_generator_context__107_0(
                sourceProgram,
                lookupProgram
        );
        var tasks = new ArrayList<Supplier<List<GeneratedModule>>>();
        for (var module : JavaGenerator.list(JavaGenerator.dataMap(context).get("emittedModules"))) {
            tasks.add(() -> JavaGenerator.generatedModules(
                    GeneratedJavaScriptGenerator.java_script_generator_module_with_context__88_0(
                            module,
                            context
                    )
            ));
        }
        tasks.add(() -> JavaGenerator.generatedModules(
                GeneratedJavaScriptGenerator.java_script_generator_support_with_context__94_0(
                        sourceProgram,
                        context
                )
        ));
        var modules = AsyncTasks.run(tasks).stream()
                .flatMap(List::stream)
                .toList();
        var result = new GeneratedProgram(modules);
        LAST_GENERATION.set(new CachedGeneration(source, lookup, result));
        return result;
    }

    private record CachedGeneration(
            CompiledProgram source,
            CompiledProgram lookup,
            GeneratedProgram generated
    ) {
    }

    private record ScopedProgram(Object invocation, CompiledProgram program) {
    }
}
