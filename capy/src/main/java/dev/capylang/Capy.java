package dev.capylang;

import capy.lang.Effect;
import capy.lang.Program;
import dev.capylang.cli.Capy.CompileGenerateOptions;
import dev.capylang.cli.Capy.CompileOptions;
import dev.capylang.cli.Capy.LogLevel;
import dev.capylang.compiler.CompiledProgram;
import dev.capylang.compiler.NativeProviderManifest;
import dev.capylang.compiler.OutputType;

import java.io.PrintStream;
import java.nio.file.Path;
import java.util.Optional;
import java.util.TreeSet;

public final class Capy {
    private Capy() {
    }

    public static void main(String... args) {
        var program = dev.capylang.cli.Capy.run(java.util.List.of(args)).unsafeRun();
        if (program instanceof capy.lang.Program.Failed failed) {
            System.exit(failed.exit_code());
        }
    }

    public static CompiledProgram readLinkedProgram(Path inputDir, boolean includeRuntime) {
        return new CompiledProgram(
                java.util.List.of(),
                java.util.List.of(),
                dev.capylang.compiler.CompiledProgramModule.emptyNativeProviderManifest(),
                dev.capylang.compiler.CompiledProgramModule.emptyNativeProviderCatalog()
        );
    }

    public static NativeProviderManifest readNativeProviderManifest(Path nativeWiring) {
        return dev.capylang.compiler.CompiledProgramModule.emptyNativeProviderManifest();
    }

    public static int compile(
            Path inputPath,
            Path linkedOutputPath,
            TreeSet<?> libraries,
            boolean compileTests,
            PrintStream errors,
            String compilerVersion
    ) {
        if (linkedOutputPath == null) {
            errors.println("Linked output path is required.");
            return Program.DEFAULT_FAILED_EXIT_CODE;
        }
        var options = new CompileOptions(
                inputPath.toString(),
                linkedOutputPath.toString(),
                Optional.empty(),
                Optional.empty(),
                compileTests,
                LogLevel.WARN
        );
        return runProgram(dev.capylang.cli.Capy.runCompile(options), errors);
    }

    public static int compileGenerate(
            OutputType outputType,
            Path inputPath,
            Path generatedOutputPath,
            Path linkedOutputPath,
            Path testInputPath,
            Path testGeneratedOutputPath,
            TreeSet<?> libraries,
            boolean compileTests,
            boolean includeJavaLibResources,
            String compilerVersion,
            PrintStream errors
    ) {
        if (generatedOutputPath == null) {
            errors.println("Generated output path is required.");
            return Program.DEFAULT_FAILED_EXIT_CODE;
        }
        var options = new CompileGenerateOptions(
                outputType,
                inputPath.toString(),
                generatedOutputPath.toString(),
                optionalPath(linkedOutputPath),
                optionalPath(testInputPath),
                optionalPath(testGeneratedOutputPath),
                Optional.empty(),
                Optional.empty(),
                compileTests,
                includeJavaLibResources,
                LogLevel.WARN
        );
        return runProgram(dev.capylang.cli.Capy.runCompileGenerate(options), errors);
    }

    public static int compileGenerate(
            OutputType outputType,
            Path inputPath,
            Path generatedOutputPath,
            Path linkedOutputPath,
            Path testInputPath,
            Path testGeneratedOutputPath,
            TreeSet<?> libraries,
            boolean compileTests,
            boolean includeJavaLibResources,
            NativeProviderManifest nativeProviders,
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
                compilerVersion,
                errors
        );
    }

    private static Optional<String> optionalPath(Path path) {
        return path == null ? Optional.empty() : Optional.of(path.toString());
    }

    private static int runProgram(Effect<Program> effect, PrintStream errors) {
        try {
            var program = effect.unsafeRun();
            if (program instanceof Program.Failed failed) {
                return failed.exit_code();
            }
            return 0;
        } catch (RuntimeException exception) {
            errors.println(exception.getMessage());
            return Program.DEFAULT_FAILED_EXIT_CODE;
        }
    }
}
