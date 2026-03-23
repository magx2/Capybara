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
import java.io.PrintStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.time.OffsetDateTime;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TreeSet;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

public class Capy {
    private static final Logger log = Logger.getLogger(Capy.class.getName());
    public static final String EXTENSION = ".json";
    private static final String BUILD_INFO_FILE = "build-info.json";
    private static final String VERSION_RESOURCE = "/capybara-version.txt";
    private static final int EXIT_SUCCESS = 0;
    private static final int EXIT_USAGE = 1;
    private static final int EXIT_FAILURE = 2;
    private static final int EXIT_COMPILATION_ERROR = 100;

    public static void main(String[] args) {
        System.exit(execute(args, System.out, System.err));
    }

    static int execute(String[] args, PrintStream out, PrintStream err) {
        try {
            return executeOrThrow(args, out, err);
        } catch (CliException e) {
            err.println(e.getMessage());
            return EXIT_USAGE;
        } catch (Exception e) {
            var message = e.getMessage() == null || e.getMessage().isBlank() ? e.getClass().getSimpleName() : e.getMessage();
            err.println(message);
            log.log(Level.SEVERE, message, e);
            return EXIT_FAILURE;
        }
    }

    public static int compile(Path input, Path linkedOutputDir, TreeSet<CompiledModule> libraries, PrintStream err) {
        return compile(input, linkedOutputDir, libraries, err, readCompilerVersion());
    }

    public static int compile(
            Path input,
            Path linkedOutputDir,
            TreeSet<CompiledModule> libraries,
            PrintStream err,
            String compilerVersion
    ) {
        try {
            return compileOrThrow(input, linkedOutputDir, libraries, err, compilerVersion);
        } catch (CliException e) {
            err.println(e.getMessage());
            return EXIT_USAGE;
        } catch (Exception e) {
            var message = e.getMessage() == null || e.getMessage().isBlank() ? e.getClass().getSimpleName() : e.getMessage();
            err.println(message);
            log.log(Level.SEVERE, message, e);
            return EXIT_FAILURE;
        }
    }

    public static int generate(OutputType outputType, Path linkedInputDir, Path generatedOutputDir, PrintStream err) {
        try {
            return generateOrThrow(outputType, linkedInputDir, generatedOutputDir);
        } catch (CliException e) {
            err.println(e.getMessage());
            return EXIT_USAGE;
        } catch (Exception e) {
            var message = e.getMessage() == null || e.getMessage().isBlank() ? e.getClass().getSimpleName() : e.getMessage();
            err.println(message);
            log.log(Level.SEVERE, message, e);
            return EXIT_FAILURE;
        }
    }

    private static int executeOrThrow(String[] args, PrintStream out, PrintStream err) throws IOException {
        if (args.length == 0) {
            throw new CliException(helpText());
        }

        if (isVersionCommand(args[0])) {
            out.println(versionText());
            return EXIT_SUCCESS;
        }
        if (isHelpCommand(args[0])) {
            out.println(versionText());
            out.println();
            out.println(helpText());
            return EXIT_SUCCESS;
        }

        var command = args[0].toLowerCase(Locale.ROOT);
        return switch (command) {
            case "compile" -> executeCompile(Arrays.copyOfRange(args, 1, args.length), err);
            case "generate" -> executeGenerate(Arrays.copyOfRange(args, 1, args.length), err);
            default -> throw new CliException("Unknown command `" + args[0] + "`.\n\n" + helpText());
        };
    }

    private static boolean isVersionCommand(String arg) {
        return "-v".equals(arg) || "--version".equals(arg);
    }

    private static boolean isHelpCommand(String arg) {
        return "-h".equals(arg) || "--help".equals(arg);
    }

    private static int executeCompile(String[] args, PrintStream err) {
        var options = parseNamedOptions(args, true);
        configureLogging(options.logLevel());

        var input = requiredPath(options.values(), "input", "compile");
        var output = requiredPath(options.values(), "output", "compile");
        var libraries = readLibraryModules(options.values().get("libs"));

        return compile(input, output, libraries, err);
    }

    private static int executeGenerate(String[] args, PrintStream err) {
        if (args.length == 0) {
            throw new CliException("Missing output type for `generate`.\n\n" + helpText());
        }

        var outputType = parseOutputType(args[0]);
        var options = parseNamedOptions(Arrays.copyOfRange(args, 1, args.length), false);
        configureLogging(options.logLevel());

        var input = options.values().containsKey("input") ? Path.of(options.values().get("input")) : Path.of(".");
        var output = requiredPath(options.values(), "output", "generate");
        return generate(outputType, input, output, err);
    }

    private static NamedOptions parseNamedOptions(String[] args, boolean allowLibs) {
        var values = new java.util.LinkedHashMap<String, String>();
        var logLevel = Level.INFO;

        for (int i = 0; i < args.length; i++) {
            var arg = args[i];
            switch (arg) {
                case "--log" -> {
                    var value = nextValue(args, i, arg);
                    logLevel = parseLogLevel(value);
                    i++;
                }
                case "-i", "--input" -> {
                    var value = nextValue(args, i, arg);
                    values.put("input", value);
                    i++;
                }
                case "-o", "--output" -> {
                    var value = nextValue(args, i, arg);
                    values.put("output", value);
                    i++;
                }
                case "-l", "--libs" -> {
                    if (!allowLibs) {
                        throw new CliException("Option `" + arg + "` is supported only for `compile`.");
                    }
                    var value = nextValue(args, i, arg);
                    values.put("libs", value);
                    i++;
                }
                default -> throw new CliException("Unknown option `" + arg + "`.\n\n" + helpText());
            }
        }

        return new NamedOptions(values, logLevel);
    }

    private static String nextValue(String[] args, int index, String option) {
        if (index + 1 >= args.length) {
            throw new CliException("Missing value for `" + option + "`.");
        }
        return args[index + 1];
    }

    private static Path requiredPath(Map<String, String> values, String key, String command) {
        var value = values.get(key);
        if (value == null || value.isBlank()) {
            throw new CliException("Missing required option `--" + key + "` for `" + command + "`.\n\n" + helpText());
        }
        return Path.of(value);
    }

    private static Level parseLogLevel(String value) {
        return switch (value.toUpperCase(Locale.ROOT)) {
            case "DEBUG" -> Level.FINE;
            case "INFO" -> Level.INFO;
            case "WARN" -> Level.WARNING;
            case "ERROR" -> Level.SEVERE;
            default -> throw new CliException("Unknown log level `" + value + "`. Use DEBUG, INFO, WARN, or ERROR.");
        };
    }

    private static OutputType parseOutputType(String value) {
        return switch (value.toUpperCase(Locale.ROOT)) {
            case "JAVA" -> OutputType.JAVA;
            case "PYTHON" -> OutputType.PYTHON;
            case "JAVASCRIPT", "JS" -> OutputType.JAVASCRIPT;
            default -> throw new CliException("Unknown output type `" + value + "`. Use java, python, javascript, or js.");
        };
    }

    private static int compileOrThrow(
            Path input,
            Path linkedOutputDir,
            TreeSet<CompiledModule> libraries,
            PrintStream err,
            String compilerVersion
    ) throws IOException {
        validateInputDirectory(input);
        validateEmptyExistingDirectory(linkedOutputDir, "Compile output path");

        log.info("Compiling files from: " + input);
        var rawModules = listSourceFiles(input).stream()
                .filter(sourceFile -> sourceFile.path().getFileName().toString().endsWith(".cfun"))
                .map(Capy::buildModule)
                .toList();

        var linking = CapybaraCompiler.INSTANCE.compile(rawModules, libraries);
        if (linking instanceof Result.Error<CompiledProgram> error) {
            err.println("Compilation failed with " + error.errors().size() + " error(s):");
            error.errors().forEach(err::println);
            return EXIT_COMPILATION_ERROR;
        }

        var linkedProgram = ((Result.Success<CompiledProgram>) linking).value();
        writeLinkedModules(linkedOutputDir, linkedProgram);
        writeBuildInfo(linkedOutputDir, compilerVersion);
        return EXIT_SUCCESS;
    }

    private static int generateOrThrow(OutputType outputType, Path linkedInputDir, Path generatedOutputDir) throws IOException {
        validateInputDirectory(linkedInputDir);
        if (Files.notExists(generatedOutputDir)) {
            Files.createDirectories(generatedOutputDir);
        } else if (!Files.isDirectory(generatedOutputDir)) {
            throw new CliException("Generated output path is not a directory: " + generatedOutputDir);
        }

        var linkedProgram = readLinkedProgram(linkedInputDir, true);
        var compiledProgram = Generator.findGenerator(outputType).generate(linkedProgram);
        compiledProgram.modules().forEach(module -> writeCompiledModule(generatedOutputDir, module.relativePath(), module.code()));
        return EXIT_SUCCESS;
    }

    private static TreeSet<CompiledModule> readLibraryModules(String libsOption) {
        if (libsOption == null || libsOption.isBlank()) {
            return new TreeSet<>();
        }

        var libraries = libsOption.split(",");
        var modules = new TreeSet<CompiledModule>();
        for (var library : libraries) {
            var trimmed = library.trim();
            if (trimmed.isEmpty()) {
                continue;
            }
            var libraryDir = Path.of(trimmed);
            validateInputDirectory(libraryDir);
            modules.addAll(readLinkedProgram(libraryDir, false).modules());
        }
        return modules;
    }

    private static void validateInputDirectory(Path directory) {
        if (Files.notExists(directory)) {
            throw new CliException("Input path does not exist: " + directory);
        }
        if (!Files.isDirectory(directory)) {
            throw new CliException("Input path is not a directory: " + directory);
        }
    }

    private static void validateEmptyExistingDirectory(Path directory, String label) throws IOException {
        if (Files.notExists(directory)) {
            throw new CliException(label + " does not exist: " + directory);
        }
        if (!Files.isDirectory(directory)) {
            throw new CliException(label + " is not a directory: " + directory);
        }
        try (var files = Files.list(directory)) {
            if (files.findAny().isPresent()) {
                throw new CliException(label + " must be empty: " + directory);
            }
        }
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

    private static void writeBuildInfo(Path outputDir, String compilerVersion) {
        var buildInfo = Map.of(
                "build_date_time", OffsetDateTime.now().toString(),
                "capybara_compiler_version", compilerVersion
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

    static String readCompilerVersion() {
        try (InputStream stream = Capy.class.getResourceAsStream(VERSION_RESOURCE)) {
            if (stream == null) {
                throw new IllegalStateException("Missing version resource: " + VERSION_RESOURCE);
            }
            return new String(stream.readAllBytes(), StandardCharsets.UTF_8).trim();
        } catch (IOException e) {
            throw new UncheckedIOException("Unable to read compiler version resource", e);
        }
    }

    private static String versionText() {
        return "Capybara compiler version: " + readCompilerVersion();
    }

    private static String helpText() {
        return String.join(System.lineSeparator(),
                "Usage:",
                "  capy -v | --version",
                "  capy -h | --help",
                "  capy compile [-l|--libs <dir1,dir2,...>] -i|--input <dir> -o|--output <dir> [--log <DEBUG|INFO|WARN|ERROR>]",
                "  capy generate <java|python|javascript|js> [-i|--input <dir>] -o|--output <dir> [--log <DEBUG|INFO|WARN|ERROR>]",
                "",
                "Notes:",
                "  compile output directory must already exist and be empty.",
                "  generate input directory defaults to the current directory."
        );
    }

    private static CompiledProgram readLinkedProgram(Path linkedInputDir, boolean requireModules) {
        try (var files = Files.walk(linkedInputDir)) {
            var modules = files
                    .filter(Files::isRegularFile)
                    .filter(path -> path.getFileName().toString().endsWith(EXTENSION))
                    .filter(path -> !path.getFileName().toString().equals(BUILD_INFO_FILE))
                    .map(Capy::readLinkedModule)
                    .toList();
            if (requireModules && modules.isEmpty()) {
                throw new CliException("Missing linked module files in directory: " + linkedInputDir);
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

    private static void configureLogging(Level level) {
        var rootLogger = Logger.getLogger("");
        rootLogger.setLevel(level);
        for (Handler handler : rootLogger.getHandlers()) {
            handler.setLevel(level);
        }
    }

    private record SourceFile(Path rootPath, Path path) {
    }

    private record NamedOptions(Map<String, String> values, Level logLevel) {
    }
}
