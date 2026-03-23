package pl.grzeslowski.capybara;

import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.jsontype.BasicPolymorphicTypeValidator;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import pl.grzeslowski.capybara.compiler.CapybaraCompiler;
import pl.grzeslowski.capybara.compiler.CompiledModule;
import pl.grzeslowski.capybara.compiler.CompiledProgram;
import pl.grzeslowski.capybara.compiler.OutputType;
import pl.grzeslowski.capybara.compiler.Result;
import pl.grzeslowski.capybara.compiler.parser.RawModule;
import pl.grzeslowski.capybara.generator.Generator;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.time.OffsetDateTime;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

public class Capy {
    private static final Logger log = Logger.getLogger(Capy.class.getName());
    public static final String EXTENSION = ".json";
    private static final String BUILD_INFO_FILE = "build-info.json";
    private static final String VERSION_RESOURCE = "/capybara-version.txt";

    public static void main(String[] args) throws IOException {
        var filteredArgs = Arrays.stream(args)
                .filter(arg -> !"--debug".equals(arg))
                .toArray(String[]::new);

        if (filteredArgs.length != args.length) {
            enableDebugLogging();
        }

        int exitCode = execute(filteredArgs);
        System.exit(exitCode);
    }

    private static int execute(String[] args) throws IOException {
        if (args.length == 0) {
            throw new IllegalArgumentException(
                    "Usage: LINK <input1> <input2> ... <linkedOutputDir> | "
                    + "GENERATE {JAVA,PYTHON,JAVASCRIPT} <linkedInputDir> <generatedOutputDir>"
            );
        }
        if ("LINK".equalsIgnoreCase(args[0])) {
            return linkCommand(args);
        }
        if ("GENERATE".equalsIgnoreCase(args[0])) {
            return generateCommand(args);
        }
        throw new IllegalArgumentException(
                "Unknown command `" + args[0] + "`. "
                + "Use: LINK <input1> <input2> ... <linkedOutputDir> or "
                + "GENERATE {JAVA,PYTHON,JAVASCRIPT} <linkedInputDir> <generatedOutputDir>"
        );
    }

    private static int linkCommand(String[] args) throws IOException {
        if (args.length < 3) {
            throw new IllegalArgumentException("Usage: LINK <input1> <input2> ... <linkedOutputDir>");
        }
        var inputs = Arrays.stream(args, 1, args.length - 1).map(Path::of).toList();
        var linkedOutputDir = Path.of(args[args.length - 1]);
        return link(inputs, linkedOutputDir);
    }

    private static int generateCommand(String[] args) throws IOException {
        if (args.length != 4) {
            throw new IllegalArgumentException(
                    "Usage: GENERATE {JAVA,PYTHON,JAVASCRIPT} <linkedInputDir> <generatedOutputDir>"
            );
        }
        var outputType = OutputType.valueOf(args[1].toUpperCase());
        var linkedInputDir = Path.of(args[2]);
        var generatedOutputDir = Path.of(args[3]);
        return generate(outputType, linkedInputDir, generatedOutputDir);
    }

    private static int link(List<Path> inputs, Path linkedOutputDir) throws IOException {
        if (Files.notExists(linkedOutputDir)) {
            Files.createDirectories(linkedOutputDir);
        } else if (!Files.isDirectory(linkedOutputDir)) {
            throw new IllegalArgumentException("Linked output path is not a directory: " + linkedOutputDir);
        }
        for (var input : inputs) {
            if (Files.notExists(input)) {
                throw new IllegalArgumentException("Input path does not exist: " + input);
            }
            if (!Files.isDirectory(input)) {
                throw new IllegalArgumentException("Input path is not a directory: " + input);
            }
        }

        log.info("Linking files from: " + inputs.stream().map(Path::toString).toList());
        var rawModules = inputs.stream()
                .map(Capy::listSourceFiles)
                .flatMap(Collection::stream)
                .filter(sourceFile -> sourceFile.path().getFileName().toString().endsWith(".cfun"))
                .map(Capy::buildModule)
                .toList();

        var linking = CapybaraCompiler.INSTANCE.compile(rawModules, new java.util.TreeSet<>());
        if (linking instanceof Result.Error<CompiledProgram> error) {
            System.err.println("Linking failed with " + error.errors().size() + " error(s):");
            error.errors().forEach(System.err::println);
            return 100;
        }

        var linkedProgram = ((Result.Success<CompiledProgram>) linking).value();
        writeLinkedModules(linkedOutputDir, linkedProgram);
        writeBuildInfo(linkedOutputDir);
        return 0;
    }

    private static int generate(OutputType outputType, Path linkedInputDir, Path generatedOutputDir) throws IOException {
        if (Files.notExists(generatedOutputDir)) {
            Files.createDirectories(generatedOutputDir);
        } else if (!Files.isDirectory(generatedOutputDir)) {
            throw new IllegalArgumentException("Generated output path is not a directory: " + generatedOutputDir);
        }
        if (Files.notExists(linkedInputDir) || !Files.isDirectory(linkedInputDir)) {
            throw new IllegalArgumentException("Linked input path is not a directory: " + linkedInputDir);
        }

        var linkedProgram = readLinkedModules(linkedInputDir);
        var compiledProgram = Generator.findGenerator(outputType).generate(linkedProgram);
        compiledProgram.modules().forEach(module -> writeCompiledModule(generatedOutputDir, module.relativePath(), module.code()));
        return 0;
    }

    private static void writeCompiledModule(Path outputDir, Path relativePath, String code) {
        var absolutePath = outputDir.resolve(relativePath);
        try {
            var parent = absolutePath.getParent();
            if (parent != null) {
                Files.createDirectories(parent);
            }
            log.info("Writing module to file: " + absolutePath);
            Files.writeString(
                    absolutePath,
                    code,
                    StandardOpenOption.CREATE,
                    StandardOpenOption.TRUNCATE_EXISTING,
                    StandardOpenOption.WRITE
            );
        } catch (IOException e) {
            throw new UncheckedIOException("Unable to write file: " + absolutePath, e);
        }
    }

    private static void writeLinkedModules(Path outputDir, CompiledProgram program) {
        var mapper = objectMapper();
        try {
            Files.createDirectories(outputDir);
            for (var module : program.modules()) {
                var modulePath = module.path().replace('\\', '/');
                var moduleJson = modulePath.isBlank()
                        ? outputDir.resolve(module.name() + EXTENSION)
                        : outputDir.resolve(modulePath).resolve(module.name() + EXTENSION);
                Files.createDirectories(moduleJson.getParent());
                mapper.writerWithDefaultPrettyPrinter().writeValue(moduleJson.toFile(), module);
                log.info("Writing linked module to file: " + moduleJson);
            }
        } catch (IOException e) {
            throw new UncheckedIOException("Unable to write linked JSON output to " + outputDir, e);
        }
    }

    private static void writeBuildInfo(Path outputDir) {
        var buildInfo = Map.of(
                "build_date_time", OffsetDateTime.now().toString(),
                "capybara_compiler_version", readCompilerVersion()
        );
        try {
            Files.createDirectories(outputDir);
            var buildInfoFile = outputDir.resolve(BUILD_INFO_FILE);
            objectMapper().writerWithDefaultPrettyPrinter().writeValue(buildInfoFile.toFile(), buildInfo);
            log.info("Writing build info to file: " + buildInfoFile);
        } catch (IOException e) {
            throw new UncheckedIOException("Unable to write build info JSON to " + outputDir, e);
        }
    }

    private static String readCompilerVersion() {
        try (InputStream stream = Capy.class.getResourceAsStream(VERSION_RESOURCE)) {
            if (stream == null) {
                throw new IllegalStateException("Missing version resource: " + VERSION_RESOURCE);
            }
            return new String(stream.readAllBytes(), StandardCharsets.UTF_8).trim();
        } catch (IOException e) {
            throw new UncheckedIOException("Unable to read compiler version resource", e);
        }
    }

    private static CompiledProgram readLinkedModules(Path linkedInputDir) {
        try (var files = Files.walk(linkedInputDir)) {
            var modules = files
                    .filter(Files::isRegularFile)
                    .filter(path -> path.getFileName().toString().endsWith(EXTENSION))
                    .filter(path -> !path.getFileName().toString().equals(BUILD_INFO_FILE))
                    .map(Capy::readLinkedModule)
                    .toList();
            if (modules.isEmpty()) {
                throw new IllegalArgumentException("Missing linked module files in directory: " + linkedInputDir);
            }
            return new CompiledProgram(modules);
        } catch (IOException e) {
            throw new UncheckedIOException("Unable to read linked module JSONs from: " + linkedInputDir, e);
        }
    }

    private static CompiledModule readLinkedModule(Path linkedModuleFile) {
        try {
            return objectMapper().readValue(linkedModuleFile.toFile(), CompiledModule.class);
        } catch (IOException e) {
            throw new UncheckedIOException("Unable to read linked module JSON: " + linkedModuleFile, e);
        }
    }

    private static ObjectMapper objectMapper() {
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

    private static RawModule buildModule(SourceFile sourceFile) {
        log.info("Building module from file: " + sourceFile.path());
        var fileName = sourceFile.path().getFileName().toString();
        var fileNameWithoutExtension = fileName.substring(0, fileName.lastIndexOf('.'));
        return new RawModule(
                fileNameWithoutExtension,
                findModulePath(sourceFile),
                readFile(sourceFile.path())
        );
    }

    private static List<SourceFile> listSourceFiles(Path directory) {
        try (var stream = Files.walk(directory)) {
            return stream
                    .filter(Files::isRegularFile)
                    .map(path -> new SourceFile(directory, path))
                    .toList();
        } catch (IOException e) {
            throw new UncheckedIOException("Unable to list files for directory: " + directory, e);
        }
    }

    private static String findModulePath(SourceFile sourceFile) {
        var relativePath = sourceFile.rootPath().relativize(sourceFile.path()).getParent();
        return relativePath == null ? "" : relativePath.toString();
    }

    private static String readFile(Path path) {
        try {
            return Files.readString(path);
        } catch (IOException e) {
            throw new UnsupportedOperationException("Unable to read file: " + path, e);
        }
    }

    private static void enableDebugLogging() {
        var rootLogger = Logger.getLogger("");
        rootLogger.setLevel(Level.FINE);
        for (Handler handler : rootLogger.getHandlers()) {
            handler.setLevel(Level.FINE);
        }
    }

    private record SourceFile(Path rootPath, Path path) {
    }
}

