package pl.grzeslowski.capybara;

import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.jsontype.BasicPolymorphicTypeValidator;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import org.gradle.api.DefaultTask;
import org.gradle.api.GradleException;
import org.gradle.api.file.ConfigurableFileCollection;
import org.gradle.api.file.DirectoryProperty;
import org.gradle.api.tasks.InputDirectory;
import org.gradle.api.tasks.InputFiles;
import org.gradle.api.tasks.OutputDirectory;
import org.gradle.api.tasks.TaskAction;
import pl.grzeslowski.capybara.compiler.CapybaraCompiler;
import pl.grzeslowski.capybara.compiler.CompiledProgram;
import pl.grzeslowski.capybara.compiler.Result;
import pl.grzeslowski.capybara.compiler.parser.RawModule;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

public abstract class CompileCapybaraTask extends DefaultTask {

    @InputDirectory
    public abstract DirectoryProperty getInputDir();

    @InputFiles
    public abstract ConfigurableFileCollection getAdditionalInputDirs();

    @OutputDirectory
    public abstract DirectoryProperty getOutputDir();

    @TaskAction
    public void compile() {
        var input = getInputDir().get().getAsFile();
        var output = getOutputDir().get().getAsFile();

        getLogger().lifecycle("Compiling Capybara from {} to {}", input, output);
        if (!input.exists() || !input.isDirectory()) {
            throw new GradleException("Input directory does not exist: " + input);
        }
        if (!output.exists() && !output.mkdirs()) {
            throw new GradleException("Cannot create output directory: " + output);
        }

        var sourceRoots = new ArrayList<Path>();
        sourceRoots.add(input.toPath());
        getAdditionalInputDirs().getFiles().stream()
                .filter(file -> file.exists() && file.isDirectory())
                .map(java.io.File::toPath)
                .forEach(sourceRoots::add);

        var sourceFiles = sourceRoots.stream()
                .flatMap(root -> listSourceFiles(root).stream().map(path -> new SourceFile(root, path)))
                .filter(sourceFile -> sourceFile.path().getFileName().toString().endsWith(".cfun"))
                .toList();
        getLogger().lifecycle("Found {} Capybara source files", sourceFiles.size());

        var rawModules = new ArrayList<RawModule>();
        var parsingFailures = new ArrayList<String>();
        for (var sourceFile : sourceFiles) {
            try {
                rawModules.add(buildModule(sourceFile.rootPath(), sourceFile.path()));
            } catch (RuntimeException e) {
                var msg = "Parsing failed for " + sourceFile.path() + ": " + e.getMessage();
                parsingFailures.add(msg);
                getLogger().error(msg, e);
            }
        }
        if (!parsingFailures.isEmpty()) {
            throw new GradleException("Parsing failed for " + parsingFailures.size() + " file(s). Check logs for details.");
        }

        var linking = CapybaraCompiler.INSTANCE.compile(rawModules, new java.util.TreeSet<>());
        if (linking instanceof Result.Error<CompiledProgram> error) {
            getLogger().error("Linking failed with {} error(s)", error.errors().size());
            error.errors().forEach(linkingError -> getLogger().error(linkingError.toString()));
            throw new GradleException("Linking failed with " + error.errors().size() + " error(s).");
        }

        var linkedProgram = ((Result.Success<CompiledProgram>) linking).value();
        writeLinkedJson(output.toPath(), linkedProgram);
    }

    private RawModule buildModule(Path rootPath, Path sourceFile) {
        getLogger().info("Parsing module: {}", sourceFile);
        var fileName = sourceFile.getFileName().toString();
        var moduleName = fileName.substring(0, fileName.lastIndexOf('.'));
        return new RawModule(moduleName, findModulePath(rootPath, sourceFile), readFile(sourceFile));
    }

    private String readFile(Path file) {
        try {
            return Files.readString(file);
        } catch (IOException e) {
            throw new UncheckedIOException("Unable to read file: " + file, e);
        }
    }

    private List<Path> listSourceFiles(Path directory) {
        try (var stream = Files.walk(directory)) {
            return stream
                    .filter(Files::isRegularFile)
                    .toList();
        } catch (IOException e) {
            throw new UncheckedIOException("Unable to list files under directory: " + directory, e);
        }
    }

    private String findModulePath(Path rootPath, Path sourceFile) {
        var parent = rootPath.relativize(sourceFile).getParent();
        return parent == null ? "" : parent.toString();
    }


    private void writeLinkedJson(Path outputDir, CompiledProgram program) {
        var mapper = objectMapper();
        try {
            Files.createDirectories(outputDir);
            var fullProgramFile = outputDir.resolve("linked-program.json");
            mapper.writerWithDefaultPrettyPrinter().writeValue(fullProgramFile.toFile(), program);
            getLogger().lifecycle("Wrote linked program: {}", fullProgramFile);

            for (var module : program.modules()) {
                var modulePath = module.path().replace('\\', '/');
                var moduleJson = (modulePath.isBlank()
                        ? outputDir.resolve(module.name() + ".linked.json")
                        : outputDir.resolve(modulePath).resolve(module.name() + ".linked.json"));
                Files.createDirectories(moduleJson.getParent());
                mapper.writerWithDefaultPrettyPrinter().writeValue(moduleJson.toFile(), module);
            }
        } catch (IOException e) {
            throw new UncheckedIOException("Unable to write linked JSON output to " + outputDir, e);
        }
    }

    static ObjectMapper objectMapper() {
        var mapper = new ObjectMapper();
        mapper.registerModule(new Jdk8Module());
        mapper.activateDefaultTyping(
                BasicPolymorphicTypeValidator.builder()
                        .allowIfSubType("pl.grzeslowski.capybara")
                        .allowIfSubType("java.util.")
                        .build(),
                ObjectMapper.DefaultTyping.NON_FINAL,
                JsonTypeInfo.As.PROPERTY
        );
        return mapper;
    }


    private record SourceFile(Path rootPath, Path path) {
    }
}
