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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

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
        var libraries = readLibraryDirectories();

        if (output != null) {
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
            List<java.nio.file.Path> libraries,
            PrintStream errors
    ) throws IOException {
        if (generatedOutput == null && output == null) {
            throw new GradleException("At least one Capybara compile output directory must be configured for " + getName());
        }

        if (generatedOutput != null) {
            var args = new ArrayList<String>();
            args.add("compile-generate");
            args.add("java");
            args.add("-i");
            args.add(input.toString());
            args.add("-o");
            args.add(generatedOutput.toString());
            if (output != null) {
                args.add("--linked-output");
                args.add(output.toString());
            }
            if (testInput != null && generatedTestOutput != null) {
                args.add("--test-input");
                args.add(testInput.toString());
                args.add("--test-output");
                args.add(generatedTestOutput.toString());
            }
            if (!getIncludeJavaLibResources().getOrElse(true)) {
                args.add("--skip-java-lib");
            }
            appendCompileOptions(args, libraries);
            return CapybaraCliExecutor.execute(args, new PrintStream(new ByteArrayOutputStream()), errors);
        }

        var args = new ArrayList<String>();
        args.add("compile");
        args.add("-i");
        args.add(input.toString());
        args.add("-o");
        args.add(output.toString());
        appendCompileOptions(args, libraries);
        return CapybaraCliExecutor.execute(args, new PrintStream(new ByteArrayOutputStream()), errors);
    }

    private void appendCompileOptions(List<String> args, List<java.nio.file.Path> libraries) {
        if (!libraries.isEmpty()) {
            args.add("-l");
            args.add(String.join(",", libraries.stream().map(java.nio.file.Path::toString).toList()));
        }
        if (getCompileTests().getOrElse(false)) {
            args.add("--compile-tests");
        }
        args.add("--log");
        args.add(getLogLevel().getOrElse("WARN"));
    }

    private List<java.nio.file.Path> readLibraryDirectories() {
        return getLibraryProgramFiles().getFiles().stream()
                .map(java.io.File::toPath)
                .map(java.nio.file.Path::getParent)
                .filter(Objects::nonNull)
                .filter(directory -> Files.exists(directory) && Files.isDirectory(directory))
                .distinct()
                .toList();
    }
}
