package dev.capylang;

import capy.lang.Effect;
import capy.lang.Either;
import capy.lang.Program;
import dev.capylang.compiler.CompileConfiguration;
import dev.capylang.compiler.CompiledModule;
import dev.capylang.compiler.CompiledProgram;
import dev.capylang.compiler.CompiledProgramModule;
import dev.capylang.compiler.CompilerError;
import dev.capylang.compiler.LinkedJsonCodec;
import dev.capylang.compiler.NativeProviderCatalog;
import dev.capylang.compiler.NativeProviderManifest;
import dev.capylang.compiler.OutputType;
import dev.capylang.generator.GeneratedModule;
import dev.capylang.generator.GeneratedProgram;
import dev.capylang.generator.Generator;

import java.io.IOException;
import java.io.PrintStream;
import java.lang.reflect.InvocationTargetException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

public final class CapyRuntime {
    private static final int EXIT_SUCCESS = 0;
    private static final int EXIT_COMPILATION_ERROR = 100;

    private CapyRuntime() {
    }

    public static CompiledProgram readLinkedProgram(Path inputDir, boolean requireModules) {
        return LinkedJsonCodec.readProgram(inputDir, requireModules);
    }

    public static NativeProviderManifest readNativeProviderManifest(Path nativeWiring) {
        if (nativeWiring == null) {
            return CompiledProgramModule.emptyNativeProviderManifest();
        }
        return LinkedJsonCodec.readFile(nativeWiring, NativeProviderManifest.class);
    }

    public static int generate(OutputType outputType, Path linkedInputDir, Path generatedOutputDir, PrintStream errors) {
        return generate(outputType, linkedInputDir, generatedOutputDir, true, errors);
    }

    public static int generate(
            OutputType outputType,
            Path linkedInputDir,
            Path generatedOutputDir,
            boolean includeJavaLibResources,
            PrintStream errors
    ) {
        try {
            var program = readLinkedProgram(linkedInputDir, true);
            writeGeneratedProgram(generatedOutputDir, Generator.generate(program, generatorOutputType(outputType)));
            return EXIT_SUCCESS;
        } catch (RuntimeException | IOException exception) {
            errors.println(rootCauseMessage(exception));
            return Program.DEFAULT_FAILED_EXIT_CODE;
        }
    }

    public static int compile(
            Path inputPath,
            Path linkedOutputPath,
            TreeSet<CompiledModule> libraries,
            boolean compileTests,
            PrintStream errors,
            String compilerVersion
    ) {
        return compile(
                inputPath,
                linkedOutputPath,
                libraries,
                compileTests,
                CompiledProgramModule.emptyNativeProviderManifest(),
                errors,
                compilerVersion
        );
    }

    public static int compile(
            Path inputPath,
            Path linkedOutputPath,
            TreeSet<CompiledModule> libraries,
            boolean compileTests,
            NativeProviderManifest nativeProviders,
            PrintStream errors,
            String compilerVersion
    ) {
        if (linkedOutputPath == null) {
            errors.println("Linked output path is required.");
            return Program.DEFAULT_FAILED_EXIT_CODE;
        }
        try {
            var compilation = compileSources(inputPath, libraries, compileTests, nativeProviders, errors);
            if (compilation == null) {
                return EXIT_COMPILATION_ERROR;
            }
            writeCompilationOutput(linkedOutputPath, compilation, compilerVersion);
            return EXIT_SUCCESS;
        } catch (RuntimeException | IOException exception) {
            errors.println(rootCauseMessage(exception));
            return Program.DEFAULT_FAILED_EXIT_CODE;
        }
    }

    public static int compileGenerate(
            OutputType outputType,
            Path inputPath,
            Path generatedOutputPath,
            Path linkedOutputPath,
            Path testInputPath,
            Path testGeneratedOutputPath,
            TreeSet<CompiledModule> libraries,
            boolean compileTests,
            boolean includeJavaLibResources,
            String compilerVersion,
            PrintStream errors
    ) {
        return compileGenerate(
                outputType,
                inputPath,
                generatedOutputPath,
                linkedOutputPath,
                testInputPath,
                testGeneratedOutputPath,
                libraries,
                compileTests,
                includeJavaLibResources,
                CompiledProgramModule.emptyNativeProviderManifest(),
                compilerVersion,
                errors
        );
    }

    public static int compileGenerate(
            OutputType outputType,
            Path inputPath,
            Path generatedOutputPath,
            Path linkedOutputPath,
            Path testInputPath,
            Path testGeneratedOutputPath,
            TreeSet<CompiledModule> libraries,
            boolean compileTests,
            boolean includeJavaLibResources,
            NativeProviderManifest nativeProviders,
            String compilerVersion,
            PrintStream errors
    ) {
        if (generatedOutputPath == null) {
            errors.println("Generated output path is required.");
            return Program.DEFAULT_FAILED_EXIT_CODE;
        }
        try {
            var compilation = compileSources(inputPath, libraries, compileTests, nativeProviders, errors);
            if (compilation == null) {
                return EXIT_COMPILATION_ERROR;
            }
            if (linkedOutputPath != null) {
                writeCompilationOutput(linkedOutputPath, compilation, compilerVersion);
            }
            generateCompiledProgram(outputType, generatedOutputPath, compilation, includeJavaLibResources);

            if (testInputPath != null && testGeneratedOutputPath != null) {
                var testCompilation = compileSources(
                        testInputPath,
                        mergeLibraries(libraries, compilation),
                        true,
                        nativeProviders,
                        errors
                );
                if (testCompilation == null) {
                    return EXIT_COMPILATION_ERROR;
                }
                var testProgram = testGenerationProgram(outputType, compilation.program(), testCompilation.program());
                writeGeneratedProgram(testGeneratedOutputPath, Generator.generate(testProgram, generatorOutputType(outputType)));
            }
            return EXIT_SUCCESS;
        } catch (RuntimeException | IOException exception) {
            errors.println(rootCauseMessage(exception));
            return Program.DEFAULT_FAILED_EXIT_CODE;
        }
    }

    public static CompilationArtifacts compileSources(
            Path inputPath,
            TreeSet<CompiledModule> libraries,
            boolean compileTests,
            PrintStream errors
    ) throws IOException {
        return compileSources(
                inputPath,
                libraries,
                compileTests,
                CompiledProgramModule.emptyNativeProviderManifest(),
                errors
        );
    }

    public static CompilationArtifacts compileSources(
            Path inputPath,
            TreeSet<CompiledModule> libraries,
            boolean compileTests,
            NativeProviderManifest nativeProviders,
            PrintStream errors
    ) throws IOException {
        var configuration = new CompileConfiguration(
                libraries == null ? Set.of() : libraries,
                nativeProviders == null ? CompiledProgramModule.emptyNativeProviderManifest() : nativeProviders
        );
        var compilation = cliCompileSourceDirectory(inputPath, configuration).unsafeRun();
        if (compilation instanceof Either.Left<?, ?> left) {
            return new CompilationArtifacts((CompiledProgram) left.value());
        }
        if (compilation instanceof Either.Right<?, ?> right) {
            @SuppressWarnings("unchecked")
            var compilerErrors = (List<CompilerError>) right.value();
            errors.println(cliCompilerErrorsText(compilerErrors));
            return null;
        }
        throw new IllegalStateException("Unexpected compile result: " + compilation);
    }

    @SuppressWarnings("unchecked")
    private static Effect<Either<CompiledProgram, List<CompilerError>>> cliCompileSourceDirectory(
            Path inputPath,
            CompileConfiguration configuration
    ) {
        return (Effect<Either<CompiledProgram, List<CompilerError>>>) invokeCliHelper(
                "compileSourceDirectory",
                new Class<?>[] {capy.io.Path.class, String.class, CompileConfiguration.class},
                PathUtil.fromJavaPath(inputPath),
                inputPath.toString(),
                configuration
        );
    }

    private static String cliCompilerErrorsText(List<CompilerError> compilerErrors) {
        return (String) invokeCliHelper(
                "compilerErrorsText",
                new Class<?>[] {List.class},
                compilerErrors
        );
    }

    private static Object invokeCliHelper(String methodName, Class<?>[] parameterTypes, Object... arguments) {
        try {
            var method = dev.capylang.cli.Capy.class.getDeclaredMethod(methodName, parameterTypes);
            method.setAccessible(true);
            return method.invoke(null, arguments);
        } catch (InvocationTargetException exception) {
            var cause = exception.getCause();
            if (cause instanceof RuntimeException runtimeException) {
                throw runtimeException;
            }
            if (cause instanceof Error error) {
                throw error;
            }
            throw new IllegalStateException("Generated Capy helper failed: " + methodName, cause);
        } catch (ReflectiveOperationException exception) {
            throw new IllegalStateException("Unable to invoke generated Capy helper: " + methodName, exception);
        }
    }

    public static void writeCompilationOutput(Path outputDir, CompilationArtifacts compilation, String compilerVersion)
            throws IOException {
        LinkedJsonCodec.writeProgram(outputDir, compilation.program());
    }

    public static void generateCompiledProgram(
            OutputType outputType,
            Path generatedOutputDir,
            CompilationArtifacts compilation,
            boolean includeJavaLibResources
    ) throws IOException {
        writeGeneratedProgram(generatedOutputDir, Generator.generate(compilation.program(), generatorOutputType(outputType)));
    }

    public static Comparator<CompiledModule> compiledModuleComparator() {
        return Comparator
                .comparing(CapyRuntime::compiledModulePath)
                .thenComparing(CompiledModule::name);
    }

    public static TreeSet<CompiledModule> mergeLibraries(TreeSet<CompiledModule> libraries, CompilationArtifacts compilation) {
        var mergedLibraries = new TreeSet<>(compiledModuleComparator());
        if (libraries != null) {
            mergedLibraries.addAll(libraries);
        }
        mergedLibraries.addAll(compilation.program().modules());
        return mergedLibraries;
    }

    private static CompiledProgram testGenerationProgram(OutputType outputType, CompiledProgram mainProgram, CompiledProgram testProgram) {
        if (outputType == OutputType.JAVA) {
            return testProgram;
        }
        return mergePrograms(mainProgram, testProgram);
    }

    private static CompiledProgram mergePrograms(CompiledProgram first, CompiledProgram second) {
        var modules = new ArrayList<CompiledModule>();
        modules.addAll(first.modules());
        modules.addAll(second.modules());
        var objectOrientedModules = new ArrayList<>(first.objectOrientedModules());
        objectOrientedModules.addAll(second.objectOrientedModules());
        return new CompiledProgram(
                modules,
                objectOrientedModules,
                first.nativeProviders().providers().isEmpty() ? second.nativeProviders() : first.nativeProviders(),
                mergeNativeProviderCatalog(first.nativeProviderCatalog(), second.nativeProviderCatalog())
        );
    }

    private static NativeProviderCatalog mergeNativeProviderCatalog(NativeProviderCatalog first, NativeProviderCatalog second) {
        if (first.declarations().isEmpty() && first.bindings().isEmpty()) {
            return second;
        }
        if (second.declarations().isEmpty() && second.bindings().isEmpty()) {
            return first;
        }
        var declarations = new ArrayList<>(first.declarations());
        declarations.addAll(second.declarations());
        var bindings = new ArrayList<>(first.bindings());
        bindings.addAll(second.bindings());
        return new NativeProviderCatalog(declarations, bindings);
    }

    private static void writeGeneratedProgram(Path outputDir, GeneratedProgram program) throws IOException {
        Files.createDirectories(outputDir);
        for (GeneratedModule module : program.modules()) {
            var outputFile = outputDir.resolve(module.relativePath());
            var parent = outputFile.getParent();
            if (parent != null) {
                Files.createDirectories(parent);
            }
            Files.writeString(outputFile, module.code());
        }
    }

    private static String compiledModulePath(CompiledModule module) {
        return module.path().isBlank() ? module.name() : module.path() + "/" + module.name();
    }

    private static String generatorOutputType(OutputType outputType) {
        return switch (outputType) {
            case JAVA -> "java";
            case PYTHON -> "python";
            case JAVASCRIPT -> "javascript";
        };
    }

    private static String rootCauseMessage(Throwable throwable) {
        var cause = throwable;
        while (cause.getCause() != null) {
            cause = cause.getCause();
        }
        var message = cause.getMessage();
        return message == null || message.isBlank() ? cause.getClass().getSimpleName() : message;
    }

    public record CompilationArtifacts(CompiledProgram program) {
    }
}
