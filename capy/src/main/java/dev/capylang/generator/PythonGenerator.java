package dev.capylang.generator;

import dev.capylang.AsyncTasks;
import dev.capylang.compiler.BackendCompilationContext;
import dev.capylang.compiler.CompiledProgram;
import dev.capylang.generator.internal.GeneratedPythonGenerator;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;

/** Bootstrap-compatible entry point for the self-hosted Python generator. */
public final class PythonGenerator {
    private static final ThreadLocal<ScopedProgram> MAIN_PROGRAM_CONTEXT = new ThreadLocal<>();
    private static final ThreadLocal<CachedGeneration> LAST_GENERATION = new ThreadLocal<>();

    private PythonGenerator() {
    }

    public static GeneratedProgram pythonGenerator(CompiledProgram program) {
        var invocation = BackendCompilationContext.generationInvocation().orElse(null);
        var scopedContext = MAIN_PROGRAM_CONTEXT.get();
        var mainContext = scopedContext != null && scopedContext.invocation() == invocation
                ? scopedContext.program()
                : null;
        var testGeneration = JavaGenerator.separateTestGeneration(program, mainContext);
        if (testGeneration.isPresent()) {
            MAIN_PROGRAM_CONTEXT.remove();
            var programs = testGeneration.get();
            return pythonGeneratorWithLookup(programs.source(), programs.lookup());
        }
        if (invocation != null && program.modules().stream().anyMatch(module -> !module.name().endsWith(".test"))) {
            MAIN_PROGRAM_CONTEXT.set(new ScopedProgram(invocation, program));
        }
        return pythonGeneratorWithLookup(program, program);
    }

    public static GeneratedProgram pythonGeneratorWithLookup(
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
        var context = GeneratedPythonGenerator.python_generator_context__122_0(sourceProgram, lookupProgram);
        var tasks = new ArrayList<Supplier<List<GeneratedModule>>>();
        tasks.add(() -> JavaGenerator.generatedModules(GeneratedPythonGenerator.python_generator_prefix__95_0(
                sourceProgram
        )));
        for (var module : JavaGenerator.list(JavaGenerator.dataMap(context).get("emittedModules"))) {
            tasks.add(() -> JavaGenerator.generatedModules(
                    GeneratedPythonGenerator.python_generator_module_with_context__101_0(
                            module,
                            context
                    )
            ));
        }
        tasks.add(() -> JavaGenerator.generatedModules(
                GeneratedPythonGenerator.python_generator_suffix_with_context__107_0(
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
