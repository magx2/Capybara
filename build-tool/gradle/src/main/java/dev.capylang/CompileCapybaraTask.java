package dev.capylang;

import org.gradle.api.DefaultTask;
import org.gradle.api.GradleException;
import org.gradle.api.file.ConfigurableFileCollection;
import org.gradle.api.file.DirectoryProperty;
import org.gradle.api.provider.Property;
import org.gradle.api.tasks.Input;
import org.gradle.api.tasks.InputDirectory;
import org.gradle.api.tasks.InputFiles;
import org.gradle.api.tasks.OutputDirectory;
import org.gradle.api.tasks.TaskAction;
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

    @Input
    public abstract Property<String> getCompilerVersion();

    @Input
    public abstract Property<Boolean> getCompileTests();

    @TaskAction
    public void compile() throws IOException {
        var input = getInputDir().get().getAsFile().toPath();
        var output = getOutputDir().get().getAsFile().toPath();
        var libraries = readLibraryModules(getAdditionalInputDirs().getFiles().stream()
                .map(java.io.File::toPath)
                .toList());

        recreateDirectory(output);
        var errors = new ByteArrayOutputStream();
        var exitCode = Capy.compile(
                input,
                output,
                libraries,
                getCompileTests().getOrElse(false),
                new PrintStream(errors),
                getCompilerVersion().get()
        );
        if (exitCode != 0) {
            var message = errors.toString().trim();
            throw new GradleException(message.isEmpty() ? "Capybara compile failed with exit code " + exitCode : message);
        }
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
