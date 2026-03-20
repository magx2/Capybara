package pl.grzeslowski.capybara;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.gradle.api.GradleException;
import org.gradle.api.DefaultTask;
import org.gradle.api.file.DirectoryProperty;
import org.gradle.api.provider.SetProperty;
import org.gradle.api.tasks.InputDirectory;
import org.gradle.api.tasks.Input;
import org.gradle.api.tasks.OutputDirectory;
import org.gradle.api.tasks.TaskAction;
import pl.grzeslowski.capybara.compiler.OutputType;
import pl.grzeslowski.capybara.generator.Generator;
import pl.grzeslowski.capybara.linker.LinkedProgram;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.Locale;

public abstract class GenerateCapybaraTask extends DefaultTask {
    private static final String LINKED_PROGRAM_FILE = "linked-program.json";

    @InputDirectory
    public abstract DirectoryProperty getInputDir();

    @OutputDirectory
    public abstract DirectoryProperty getOutputRootDir();

    @Input
    public abstract SetProperty<OutputType> getOutputTypes();

    @TaskAction
    public void generate() {
        var input = getInputDir().get().getAsFile();
        var outputRoot = getOutputRootDir().get().getAsFile();
        var outputTypes = getOutputTypes().get();

        if (outputTypes.isEmpty()) {
            throw new GradleException("No output types configured for " + getName());
        }
        if (!outputRoot.exists() && !outputRoot.mkdirs()) {
            throw new GradleException("Cannot create output root directory: " + outputRoot);
        }
        var linkedProgram = readLinkedProgram(input.toPath());

        getLogger().lifecycle("Generating outputs {} from {} to {}", outputTypes, input, outputRoot);
        outputTypes.forEach(outputType -> {
            var outputDir = outputRoot.toPath().resolve(outputType.name().toLowerCase(Locale.ROOT)).toFile();
            if (!outputDir.exists() && !outputDir.mkdirs()) {
                throw new GradleException("Cannot create output directory: " + outputDir);
            }
            getLogger().lifecycle("Generating {} output from {} to {}", outputType, input, outputDir);
            var generatedProgram = Generator.findGenerator(outputType).generate(linkedProgram);
            generatedProgram.modules().forEach(module -> writeCompiledModule(outputDir.toPath(), module.relativePath(), module.code()));
        });
    }

    private LinkedProgram readLinkedProgram(Path inputDir) {
        var linkedProgramFile = inputDir.resolve(LINKED_PROGRAM_FILE);
        if (!Files.exists(linkedProgramFile)) {
            throw new GradleException("Missing linked program file: " + linkedProgramFile);
        }
        try {
            ObjectMapper mapper = CompileCapybaraTask.objectMapper();
            return mapper.readValue(linkedProgramFile.toFile(), LinkedProgram.class);
        } catch (IOException e) {
            throw new UncheckedIOException("Unable to read linked program JSON: " + linkedProgramFile, e);
        }
    }

    private void writeCompiledModule(Path outputDir, Path relativePath, String code) {
        var target = outputDir.resolve(relativePath);
        try {
            var parent = target.getParent();
            if (parent != null) {
                Files.createDirectories(parent);
            }
            Files.writeString(
                    target,
                    code,
                    StandardOpenOption.CREATE,
                    StandardOpenOption.TRUNCATE_EXISTING,
                    StandardOpenOption.WRITE
            );
        } catch (IOException e) {
            throw new UncheckedIOException("Unable to write generated source: " + target, e);
        }
    }
}
