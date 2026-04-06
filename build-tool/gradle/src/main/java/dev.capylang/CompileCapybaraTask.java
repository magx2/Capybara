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
import org.gradle.api.tasks.TaskAction;
import dev.capylang.compiler.OutputType;
import dev.capylang.compiler.CompiledModule;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Files;
import java.util.Collection;
import java.util.TreeSet;

public abstract class CompileCapybaraTask extends DefaultTask {
    @InputDirectory
    public abstract DirectoryProperty getInputDir();

    @InputFiles
    public abstract ConfigurableFileCollection getAdditionalInputDirs();

    @OutputDirectory
    public abstract DirectoryProperty getOutputDir();

    @Optional
    @OutputDirectory
    public abstract DirectoryProperty getGeneratedOutputDir();

    @Input
    public abstract Property<String> getCompilerVersion();

    @Input
    public abstract Property<Boolean> getCompileTests();

    @Input
    public abstract Property<Boolean> getIncludeJavaLibResources();

    @TaskAction
    public void compile() throws IOException {
        var input = getInputDir().get().getAsFile().toPath();
        var output = getOutputDir().get().getAsFile().toPath();
        var generatedOutput = getGeneratedOutputDir().isPresent() ? getGeneratedOutputDir().get().getAsFile().toPath() : null;
        var libraries = readLibraryModules(getAdditionalInputDirs().getFiles().stream()
                .map(java.io.File::toPath)
                .toList());

        recreateDirectory(output);
        if (generatedOutput != null) {
            recreateDirectory(generatedOutput);
        }
        var errors = new ByteArrayOutputStream();
        var exitCode = compileAndGenerate(
                input,
                output,
                generatedOutput,
                libraries,
                new PrintStream(errors)
        );
        if (exitCode != 0) {
            var message = errors.toString().trim();
            throw new GradleException(message.isEmpty() ? "Capybara compile failed with exit code " + exitCode : message);
        }
    }

    private int compileAndGenerate(
            java.nio.file.Path input,
            java.nio.file.Path output,
            java.nio.file.Path generatedOutput,
            TreeSet<CompiledModule> libraries,
            PrintStream errors
    ) throws IOException {
        var compilation = Capy.compileSources(input, libraries, getCompileTests().getOrElse(false), errors);
        if (compilation == null) {
            return 100;
        }

        Capy.writeCompilationOutput(output, compilation, getCompilerVersion().get());
        if (generatedOutput != null) {
            Capy.generateCompiledProgram(
                    OutputType.JAVA,
                    generatedOutput,
                    compilation,
                    getIncludeJavaLibResources().getOrElse(true)
            );
        }
        return 0;
    }

    private void recreateDirectory(java.nio.file.Path directory) throws IOException {
        if (Files.exists(directory)) {
            try (var files = Files.walk(directory)) {
                files.sorted(java.util.Comparator.reverseOrder())
                        .forEach(path -> path.toFile().delete());
            }
        }
        Files.createDirectories(directory);
    }

    private TreeSet<CompiledModule> readLibraryModules(Collection<java.nio.file.Path> directories) throws IOException {
        var modules = new TreeSet<CompiledModule>();
        for (var directory : directories) {
            if (Files.notExists(directory) || !Files.isDirectory(directory)) {
                continue;
            }
            try (var files = Files.walk(directory)) {
                for (var file : files.filter(Files::isRegularFile)
                        .filter(path -> path.getFileName().toString().endsWith(CompiledModule.EXTENSION))
                        .toList()) {
                    try (var input = Files.newInputStream(file)) {
                        modules.add(Capy.objectMapper().readValue(input, CompiledModule.class));
                    }
                }
            }
        }
        return modules;
    }
}
