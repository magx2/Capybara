package dev.capylang;

import org.gradle.api.DefaultTask;
import org.gradle.api.GradleException;
import org.gradle.api.file.ConfigurableFileCollection;
import org.gradle.api.file.DirectoryProperty;
import org.gradle.api.provider.Property;
import org.gradle.api.tasks.Input;
import org.gradle.api.tasks.InputDirectory;
import org.gradle.api.tasks.InputFiles;
import org.gradle.api.tasks.Optional;
import org.gradle.api.tasks.OutputDirectory;
import org.gradle.api.tasks.PathSensitive;
import org.gradle.api.tasks.PathSensitivity;
import org.gradle.api.tasks.TaskAction;
import dev.capylang.compiler.OutputType;
import dev.capylang.compiler.CompiledModule;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Comparator;
import java.util.TreeSet;
import java.util.logging.Level;
import java.util.logging.Logger;

public abstract class CompileCapybaraTask extends DefaultTask {
    @InputDirectory
    @PathSensitive(PathSensitivity.RELATIVE)
    public abstract DirectoryProperty getInputDir();

    @InputFiles
    @PathSensitive(PathSensitivity.RELATIVE)
    public abstract ConfigurableFileCollection getLibraryProgramFiles();

    @Optional
    @OutputDirectory
    public abstract DirectoryProperty getOutputDir();

    @Input
    public abstract Property<Boolean> getWriteLinkedOutput();

    @Optional
    @OutputDirectory
    public abstract DirectoryProperty getGeneratedOutputDir();

    @Optional
    @InputDirectory
    public abstract DirectoryProperty getTestInputDir();

    @Optional
    @OutputDirectory
    public abstract DirectoryProperty getGeneratedTestOutputDir();

    @Input
    public abstract Property<String> getCompilerVersion();

    @Input
    public abstract Property<Boolean> getCompileTests();

    @Input
    public abstract Property<Boolean> getIncludeJavaLibResources();

    @Input
    public abstract Property<Boolean> getCompileTestSourcesWithMainCompilation();

    @Input
    public abstract Property<Boolean> getIncludeJavaLibResourcesInTestOutput();

    @Input
    public abstract Property<String> getLogLevel();

    @TaskAction
    public void compile() throws IOException {
        var input = getInputDir().get().getAsFile().toPath();
        var writeLinkedOutput = getWriteLinkedOutput().getOrElse(true);
        var output = writeLinkedOutput && getOutputDir().isPresent()
                ? getOutputDir().get().getAsFile().toPath()
                : null;
        var generatedOutput = getGeneratedOutputDir().isPresent() ? getGeneratedOutputDir().get().getAsFile().toPath() : null;
        var compileTestSourcesWithMainCompilation = getCompileTestSourcesWithMainCompilation().getOrElse(false);
        var testInput = compileTestSourcesWithMainCompilation && getTestInputDir().isPresent()
                ? getTestInputDir().get().getAsFile().toPath()
                : null;
        var generatedTestOutput = compileTestSourcesWithMainCompilation && getGeneratedTestOutputDir().isPresent()
                ? getGeneratedTestOutputDir().get().getAsFile().toPath()
                : null;
        var libraries = readLibraryModules(getLibraryProgramFiles().getFiles().stream()
                .map(java.io.File::toPath)
                .map(java.nio.file.Path::getParent)
                .filter(java.util.Objects::nonNull)
                .toList());

        if (output != null) {
            clearDirectory(output);
        }
        if (generatedOutput != null) {
            clearDirectory(generatedOutput);
        }
        if (generatedTestOutput != null) {
            clearDirectory(generatedTestOutput);
        }
        var errors = new ByteArrayOutputStream();
        var exitCode = withJulLogLevel(
                Level.parse(getLogLevel().getOrElse("WARNING")),
                () -> compileAndGenerate(
                        input,
                        writeLinkedOutput ? output : null,
                        generatedOutput,
                        testInput,
                        generatedTestOutput,
                        libraries,
                        new PrintStream(errors)
                )
        );
        if (exitCode != 0) {
            var message = errors.toString().trim();
            throw new GradleException(message.isEmpty() ? "Capybara compile failed with exit code " + exitCode : message);
        }
    }

    private static int withJulLogLevel(Level level, IntIoSupplier action) throws IOException {
        var rootLogger = Logger.getLogger("");
        var previousRootLevel = rootLogger.getLevel();
        var handlers = rootLogger.getHandlers();
        var previousHandlerLevels = new Level[handlers.length];
        for (int i = 0; i < handlers.length; i++) {
            previousHandlerLevels[i] = handlers[i].getLevel();
            handlers[i].setLevel(level);
        }
        rootLogger.setLevel(level);
        try {
            return action.getAsInt();
        } finally {
            rootLogger.setLevel(previousRootLevel);
            for (int i = 0; i < handlers.length; i++) {
                handlers[i].setLevel(previousHandlerLevels[i]);
            }
        }
    }

    private int compileAndGenerate(
            Path input,
            Path output,
            Path generatedOutput,
            Path testInput,
            Path generatedTestOutput,
            TreeSet<CompiledModule> libraries,
            PrintStream errors
    ) throws IOException {
        var compilation = CapyRuntime.compileSources(input, libraries, getCompileTests().getOrElse(false), errors);
        if (compilation == null) {
            return 100;
        }

        if (output != null) {
            CapyRuntime.writeCompilationOutput(output, compilation, getCompilerVersion().get());
        }
        if (generatedOutput != null) {
            CapyRuntime.generateCompiledProgram(
                    OutputType.JAVA,
                    generatedOutput,
                    compilation,
                    getIncludeJavaLibResources().getOrElse(true)
            );
        }
        if (testInput != null && generatedTestOutput != null) {
            var testCompilation = CapyRuntime.compileSources(
                    testInput,
                    CapyRuntime.mergeLibraries(libraries, compilation),
                    true,
                    errors
            );
            if (testCompilation == null) {
                return 100;
            }
            CapyRuntime.generateCompiledProgram(
                    OutputType.JAVA,
                    generatedTestOutput,
                    testCompilation,
                    getIncludeJavaLibResourcesInTestOutput().getOrElse(false)
            );
        }
        return 0;
    }

    private TreeSet<CompiledModule> readLibraryModules(Collection<Path> directories) throws IOException {
        var modules = new TreeSet<CompiledModule>(CapyRuntime.compiledModuleComparator());
        for (var directory : directories) {
            if (Files.notExists(directory) || !Files.isDirectory(directory)) {
                continue;
            }
            modules.addAll(CapyRuntime.readLinkedProgram(directory, false).modules());
        }
        return modules;
    }

    private static void clearDirectory(Path directory) throws IOException {
        if (Files.notExists(directory)) {
            Files.createDirectories(directory);
            return;
        }
        try (var paths = Files.walk(directory)) {
            paths.sorted(Comparator.reverseOrder())
                    .filter(path -> !path.equals(directory))
                    .forEach(path -> {
                        try {
                            Files.deleteIfExists(path);
                        } catch (IOException exception) {
                            throw new UncheckedIOException(exception);
                        }
                    });
        } catch (UncheckedIOException exception) {
            throw exception.getCause();
        }
        Files.createDirectories(directory);
    }

    @FunctionalInterface
    private interface IntIoSupplier {
        int getAsInt() throws IOException;
    }
}
