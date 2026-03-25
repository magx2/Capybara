package dev.capylang;

import org.gradle.api.DefaultTask;
import org.gradle.api.GradleException;
import org.gradle.api.file.DirectoryProperty;
import org.gradle.api.provider.SetProperty;
import org.gradle.api.tasks.Input;
import org.gradle.api.tasks.InputDirectory;
import org.gradle.api.tasks.OutputDirectory;
import org.gradle.api.tasks.TaskAction;
import dev.capylang.compiler.OutputType;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.file.Files;
import java.util.Locale;

public abstract class GenerateCapybaraTask extends DefaultTask {
    @InputDirectory
    public abstract DirectoryProperty getInputDir();

    @OutputDirectory
    public abstract DirectoryProperty getOutputRootDir();

    @Input
    public abstract SetProperty<OutputType> getOutputTypes();

    @TaskAction
    public void generate() throws java.io.IOException {
        var input = getInputDir().get().getAsFile().toPath();
        var outputRoot = getOutputRootDir().get().getAsFile().toPath();
        var outputTypes = getOutputTypes().get();

        if (outputTypes.isEmpty()) {
            throw new GradleException("No output types configured for " + getName());
        }
        Files.createDirectories(outputRoot);

        for (var outputType : outputTypes) {
            var outputDir = outputRoot.resolve(outputType.name().toLowerCase(Locale.ROOT));
            Files.createDirectories(outputDir);
            var errors = new ByteArrayOutputStream();
            var exitCode = Capy.generate(outputType, input, outputDir, new PrintStream(errors));
            if (exitCode != 0) {
                var message = errors.toString().trim();
                throw new GradleException(message.isEmpty() ? "Capybara generate failed with exit code " + exitCode : message);
            }
        }
    }
}
