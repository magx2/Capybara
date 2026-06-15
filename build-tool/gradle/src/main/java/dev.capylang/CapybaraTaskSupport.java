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
import dev.capylang.compiler.OutputType;
import dev.capylang.generator.GeneratedModule;
import dev.capylang.generator.GeneratedProgram;
import dev.capylang.generator.Generator;

import java.io.IOException;
import java.io.PrintStream;
import java.lang.reflect.InvocationTargetException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

final class CapybaraTaskSupport {
    private static final int EXIT_SUCCESS = 0;

    private CapybaraTaskSupport() {
    }

    static CompiledProgram readLinkedProgram(Path inputDir, boolean requireModules) {
        return LinkedJsonCodec.readProgram(inputDir, requireModules);
    }

    static int generate(OutputType outputType, Path linkedInputDir, Path generatedOutputDir, PrintStream errors) {
        try {
            var program = readLinkedProgram(linkedInputDir, true);
            writeGeneratedProgram(generatedOutputDir, Generator.generate(program, generatorOutputType(outputType)));
            return EXIT_SUCCESS;
        } catch (RuntimeException | IOException exception) {
            errors.println(rootCauseMessage(exception));
            return Program.DEFAULT_FAILED_EXIT_CODE;
        }
    }

    static CompilationArtifacts compileSources(
            Path inputPath,
            TreeSet<CompiledModule> libraries,
            boolean compileTests,
            PrintStream errors
    ) throws IOException {
        var configuration = new CompileConfiguration(
                libraries == null ? Set.of() : libraries,
                CompiledProgramModule.emptyNativeProviderManifest()
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
        var capyPath = PathUtil.fromJavaPath(inputPath);
        return (Effect<Either<CompiledProgram, List<CompilerError>>>) invokeCliHelper(
                "compileSourceDirectory",
                new Class<?>[] {capy.io.Path.class, String.class, CompileConfiguration.class},
                capyPath,
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

    static void writeCompilationOutput(Path outputDir, CompilationArtifacts compilation, String compilerVersion) throws IOException {
        LinkedJsonCodec.writeProgram(outputDir, compilation.program());
    }

    static void generateCompiledProgram(
            OutputType outputType,
            Path generatedOutputDir,
            CompilationArtifacts compilation,
            boolean includeJavaLibResources
    ) throws IOException {
        writeGeneratedProgram(generatedOutputDir, Generator.generate(compilation.program(), generatorOutputType(outputType)));
    }

    static Comparator<CompiledModule> compiledModuleComparator() {
        return Comparator
                .comparing(CapybaraTaskSupport::compiledModulePath)
                .thenComparing(CompiledModule::name);
    }

    static TreeSet<CompiledModule> mergeLibraries(TreeSet<CompiledModule> libraries, CompilationArtifacts compilation) {
        var mergedLibraries = new TreeSet<>(compiledModuleComparator());
        if (libraries != null) {
            mergedLibraries.addAll(libraries);
        }
        mergedLibraries.addAll(compilation.program().modules());
        return mergedLibraries;
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

    record CompilationArtifacts(CompiledProgram program) {
    }
}
