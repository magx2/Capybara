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

    @TaskAction
    public void compile() throws IOException {
        var input = getInputDir().get().getAsFile().toPath();
        var output = getOutputDir().get().getAsFile().toPath();
        var writeLinkedOutput = getWriteLinkedOutput().getOrElse(true);
        var generatedOutput = getGeneratedOutputDir().isPresent() ? getGeneratedOutputDir().get().getAsFile().toPath() : null;
        var compileTestSourcesWithMainCompilation = getCompileTestSourcesWithMainCompilation().getOrElse(false);
        var testInput = compileTestSourcesWithMainCompilation && getTestInputDir().isPresent()
                ? getTestInputDir().get().getAsFile().toPath()
                : null;
        var generatedTestOutput = compileTestSourcesWithMainCompilation && getGeneratedTestOutputDir().isPresent()
                ? getGeneratedTestOutputDir().get().getAsFile().toPath()
                : null;
        var libraries = readLibraryModules(getAdditionalInputDirs().getFiles().stream()
                .map(java.io.File::toPath)
                .toList());

        if (writeLinkedOutput) {
            Files.createDirectories(output);
        }
        if (generatedOutput != null) {
            Files.createDirectories(generatedOutput);
        }
        if (generatedTestOutput != null) {
            Files.createDirectories(generatedTestOutput);
        }
        var errors = new ByteArrayOutputStream();
        var exitCode = compileAndGenerate(
                input,
                writeLinkedOutput ? output : null,
                generatedOutput,
                testInput,
                generatedTestOutput,
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
            java.nio.file.Path testInput,
            java.nio.file.Path generatedTestOutput,
            TreeSet<CompiledModule> libraries,
            PrintStream errors
    ) throws IOException {
        var compilation = Capy.compileSources(input, libraries, getCompileTests().getOrElse(false), errors);
        if (compilation == null) {
            return 100;
        }

        if (output != null) {
            Capy.writeCompilationOutput(output, compilation, getCompilerVersion().get());
        }
        if (generatedOutput != null) {
            Capy.generateCompiledProgram(
                    OutputType.JAVA,
                    generatedOutput,
                    compilation,
                    getIncludeJavaLibResources().getOrElse(true)
            );
        }
        if (testInput != null && generatedTestOutput != null) {
            var testCompilation = Capy.compileSources(testInput, mergeLibraries(libraries, compilation), true, errors);
            if (testCompilation == null) {
                return 100;
            }
            Capy.generateCompiledProgram(
                    OutputType.JAVA,
                    generatedTestOutput,
                    testCompilation,
                    getIncludeJavaLibResourcesInTestOutput().getOrElse(false)
            );
        }
        return 0;
    }

    private TreeSet<CompiledModule> mergeLibraries(TreeSet<CompiledModule> libraries, Capy.CompilationArtifacts compilation) {
        var mergedLibraries = new TreeSet<>(libraries);
        mergedLibraries.addAll(compilation.program().modules());
        return mergedLibraries;
    }

    private TreeSet<CompiledModule> readLibraryModules(Collection<java.nio.file.Path> directories) throws IOException {
        var modules = new TreeSet<CompiledModule>();
        for (var directory : directories) {
            if (Files.notExists(directory) || !Files.isDirectory(directory)) {
                continue;
            }
            modules.addAll(Capy.readLinkedProgram(directory, false).modules());
        }
        return modules;
    }
}
