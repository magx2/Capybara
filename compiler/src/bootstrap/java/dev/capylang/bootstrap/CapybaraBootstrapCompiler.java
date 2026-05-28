package dev.capylang.bootstrap;

import dev.capylang.compiler.CapybaraCompiler;
import dev.capylang.compiler.CompiledModule;
import dev.capylang.compiler.CompiledProgram;
import dev.capylang.compiler.NativeImplementationScanner;
import dev.capylang.compiler.NativeProviderManifest;
import dev.capylang.compiler.Result;
import dev.capylang.compiler.parser.RawModule;
import dev.capylang.compiler.parser.SourceKind;
import dev.capylang.generator.Generator;
import dev.capylang.generator.GeneratedProgram;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.List;
import java.util.TreeSet;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

public final class CapybaraBootstrapCompiler {
    private CapybaraBootstrapCompiler() {
    }

    public static void main(String[] args) throws IOException {
        if (args.length != 2) {
            System.err.println("Usage: CapybaraBootstrapCompiler <source-dir> <generated-java-dir>");
            System.exit(1);
        }

        configureLogging(Level.WARNING);
        var sourceDir = Path.of(args[0]);
        var generatedJavaDir = Path.of(args[1]);
        var exitCode = compile(sourceDir, generatedJavaDir);
        if (exitCode != 0) {
            System.exit(exitCode);
        }
    }

    private static int compile(Path sourceDir, Path generatedJavaDir) throws IOException {
        var modules = readModules(sourceDir);
        var nativeProviders = NativeImplementationScanner.scan(sourceDir);
        var result = CapybaraCompiler.INSTANCE.compile(
                modules,
                new TreeSet<CompiledModule>(),
                NativeProviderManifest.empty(),
                nativeProviders
        );
        if (result instanceof Result.Error<CompiledProgram> error) {
            System.err.println("Compilation failed with " + error.errors().size() + " error(s):");
            error.errors().forEach(System.err::println);
            return 100;
        }

        var program = ((Result.Success<CompiledProgram>) result).value();
        var generated = Generator.findGenerator(dev.capylang.compiler.OutputType.JAVA).generate(program);
        writeGeneratedProgram(generatedJavaDir, generated);
        return 0;
    }

    private static List<RawModule> readModules(Path sourceDir) throws IOException {
        try (var stream = Files.walk(sourceDir)) {
            return stream
                    .filter(Files::isRegularFile)
                    .filter(CapybaraBootstrapCompiler::isCapybaraSource)
                    .sorted()
                    .map(path -> readModule(sourceDir, path))
                    .toList();
        }
    }

    private static boolean isCapybaraSource(Path path) {
        return SourceKind.fromPath(path).isPresent();
    }

    private static RawModule readModule(Path sourceDir, Path sourceFile) {
        var fileName = sourceFile.getFileName().toString();
        var sourceKind = SourceKind.fromFileName(fileName)
                .orElseThrow(() -> new IllegalArgumentException("Unsupported Capybara source file: " + fileName));
        var modulePath = sourceDir.relativize(sourceFile).getParent();
        try {
            return new RawModule(
                    SourceKind.stripKnownExtension(fileName),
                    modulePath == null ? "" : modulePath.toString(),
                    Files.readString(sourceFile),
                    sourceKind
            );
        } catch (IOException e) {
            throw new UncheckedIOException("Unable to read source file: " + sourceFile, e);
        }
    }

    private static void writeGeneratedProgram(Path generatedJavaDir, GeneratedProgram generatedProgram) throws IOException {
        deleteDirectory(generatedJavaDir);
        Files.createDirectories(generatedJavaDir);
        for (var module : generatedProgram.modules()) {
            var target = generatedJavaDir.resolve(module.relativePath().normalize());
            var parent = target.getParent();
            if (parent != null) {
                Files.createDirectories(parent);
            }
            Files.writeString(target, module.code(), StandardCharsets.UTF_8);
        }
    }

    private static void deleteDirectory(Path directory) throws IOException {
        if (Files.notExists(directory)) {
            return;
        }
        try (var stream = Files.walk(directory)) {
            for (var path : stream.sorted(Comparator.reverseOrder()).toList()) {
                Files.delete(path);
            }
        }
    }

    private static void configureLogging(Level level) {
        var rootLogger = Logger.getLogger("");
        rootLogger.setLevel(level);
        for (Handler handler : rootLogger.getHandlers()) {
            handler.setLevel(level);
        }
    }
}
