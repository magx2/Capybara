package dev.capylang;

import capy.lang.Effect;
import capy.lang.Program;
import dev.capylang.cli.Capy;
import dev.capylang.compiler.OutputType;

import java.io.PrintStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Locale;
import java.util.Optional;
import java.util.stream.Collectors;

final class GeneratedCapyCompiler {
    private GeneratedCapyCompiler() {
    }

    static int compile(
            Path input,
            Path linkedOutput,
            Path generatedOutput,
            Path testInput,
            Path generatedTestOutput,
            Collection<Path> libraryProgramFiles,
            boolean compileTests,
            boolean includeJavaLibResources,
            String outputTypeName,
            String logLevel,
            PrintStream errors
    ) {
        if (generatedOutput == null && testInput == null && generatedTestOutput == null) {
            if (linkedOutput == null) {
                errors.println("Linked output path is required.");
                return 1;
            }
            var options = new Capy.CompileOptions(
                    input.toString(),
                    linkedOutput.toString(),
                    optionalLibraries(libraryProgramFiles),
                    Optional.empty(),
                    compileTests,
                    logLevel(logLevel)
            );
            return runProgram(Capy.runCompile(options), errors);
        }
        if (generatedOutput == null) {
            errors.println("Generated output path is required.");
            return 1;
        }
        var options = new Capy.CompileGenerateOptions(
                outputTypeArgument(outputTypeName),
                input.toString(),
                generatedOutput.toString(),
                optionalPath(linkedOutput),
                optionalPath(testInput),
                optionalPath(generatedTestOutput),
                optionalLibraries(libraryProgramFiles),
                Optional.empty(),
                compileTests,
                includeJavaLibResources,
                logLevel(logLevel)
        );
        return runProgram(Capy.runCompileGenerate(options), errors);
    }

    static int generate(OutputType outputType, Path input, Path output, PrintStream errors) {
        var options = new Capy.GenerateOptions(
                outputTypeArgument(outputType.name()),
                input.toString(),
                output.toString(),
                true,
                Capy.LogLevel.WARN
        );
        return runProgram(Capy.runGenerate(options), errors);
    }

    private static Optional<String> optionalPath(Path path) {
        return path == null ? Optional.empty() : Optional.of(path.toString());
    }

    private static Optional<String> optionalLibraries(Collection<Path> libraryProgramFiles) {
        var libraries = libraryProgramFiles.stream()
                .map(Path::getParent)
                .filter(path -> path != null && Files.isDirectory(path))
                .map(Path::toString)
                .distinct()
                .collect(Collectors.joining(","));
        return libraries.isBlank() ? Optional.empty() : Optional.of(libraries);
    }

    private static String outputTypeArgument(String outputTypeName) {
        return switch (outputTypeName.toUpperCase(Locale.ROOT)) {
            case "JAVA" -> "java";
            case "PYTHON" -> "python";
            case "JAVASCRIPT" -> "javascript";
            default -> throw new IllegalArgumentException("Unknown Capybara output type: " + outputTypeName);
        };
    }

    private static Capy.LogLevel logLevel(String value) {
        return switch (value.toUpperCase(Locale.ROOT)) {
            case "DEBUG" -> Capy.LogLevel.DEBUG;
            case "INFO" -> Capy.LogLevel.INFO;
            case "WARN", "WARNING" -> Capy.LogLevel.WARN;
            case "ERROR" -> Capy.LogLevel.ERROR;
            default -> throw new IllegalArgumentException("Unknown Capybara log level: " + value);
        };
    }

    private static int runProgram(Effect<Program> effect, PrintStream errors) {
        try {
            var program = effect.unsafeRun();
            if (program instanceof Program.Failed failed) {
                return failed.exit_code();
            }
            return 0;
        } catch (RuntimeException exception) {
            errors.println(rootCauseMessage(exception));
            return 1;
        }
    }

    private static String rootCauseMessage(Throwable throwable) {
        var current = throwable;
        while (current.getCause() != null) {
            current = current.getCause();
        }
        return current.getMessage() == null ? current.toString() : current.getMessage();
    }
}
