package dev.capylang;

import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.fasterxml.jackson.databind.jsontype.BasicPolymorphicTypeValidator;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import org.yaml.snakeyaml.DumperOptions;
import org.yaml.snakeyaml.LoaderOptions;
import org.yaml.snakeyaml.Yaml;
import dev.capylang.compiler.CapybaraCompiler;
import dev.capylang.compiler.CollectionLinkedType;
import dev.capylang.compiler.CompiledFunction;
import dev.capylang.compiler.CompiledModule;
import dev.capylang.compiler.CompiledProgram;
import dev.capylang.compiler.OutputType;
import dev.capylang.compiler.Result;
import dev.capylang.compiler.expression.CompiledExpression;
import dev.capylang.compiler.expression.CompiledFunctionCall;
import dev.capylang.compiler.expression.CompiledInfixExpression;
import dev.capylang.compiler.expression.CompiledNewList;
import dev.capylang.compiler.parser.RawModule;
import dev.capylang.compiler.parser.InfixOperator;
import dev.capylang.generator.Generator;
import dev.capylang.generator.GeneratedProgram;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.PrintStream;
import java.io.UncheckedIOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.net.URL;
import java.net.URLClassLoader;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.DirectoryNotEmptyException;
import java.nio.file.FileSystemAlreadyExistsException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.time.Duration;
import java.time.OffsetDateTime;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeSet;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;
import javax.tools.DiagnosticCollector;
import javax.tools.JavaFileObject;
import javax.tools.ToolProvider;

public class Capy {
    private static final Logger log = Logger.getLogger(Capy.class.getName());

    private static final String BUILD_INFO_FILE = "build-info.json";
    private static final String OUTPUT_MANIFEST_FILE = ".capy-output-manifest";
    private static final String PACKAGE_FILE = "capy.cbin";
    private static final String PROGRAM_FILE = "program.json";
    private static final String MODULE_FILE = "capy.yml";
    private static final String VERSION_RESOURCE = "/capybara-version.txt";
    private static final String JAVA_LIB_RESOURCE_DIR = "/java-lib-src";
    private static final String CAPYBARA_SOURCE_EXTENSION = ".cfun";
    private static final ModuleRef CAP_TEST_RUNTIME_MODULE = new ModuleRef("CapyTestRuntime", "capy/test");
    private static final int EXIT_SUCCESS = 0;
    private static final int EXIT_USAGE = 1;
    private static final int EXIT_FAILURE = 2;
    private static final int EXIT_COMPILATION_ERROR = 100;
    private static final ObjectMapper OBJECT_MAPPER = createObjectMapper();
    private static final ObjectWriter PRETTY_JSON_WRITER = OBJECT_MAPPER.writerWithDefaultPrettyPrinter();

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
        return compile(input, linkedOutputDir, libraries, false, err, readCompilerVersion());
    }

    public static int compile(
            Path input,
            Path linkedOutputDir,
            TreeSet<CompiledModule> libraries,
            boolean compileTests,
            PrintStream err,
            String compilerVersion
    ) {
        try {
            return compileOrThrow(input, linkedOutputDir, libraries, compileTests, err, compilerVersion);
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
        return generate(outputType, linkedInputDir, generatedOutputDir, true, err);
    }

    public static int generate(
            OutputType outputType,
            Path linkedInputDir,
            Path generatedOutputDir,
            boolean includeJavaLibResources,
            PrintStream err
    ) {
        try {
            return generateOrThrow(outputType, linkedInputDir, generatedOutputDir, includeJavaLibResources);
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

    public static int compileGenerate(
            OutputType outputType,
            Path input,
            Path generatedOutputDir,
            Path linkedOutputDir,
            Path testInput,
            Path testGeneratedOutputDir,
            TreeSet<CompiledModule> libraries,
            boolean compileTests,
            boolean includeJavaLibResources,
            PrintStream err
    ) {
        return compileGenerate(
                outputType,
                input,
                generatedOutputDir,
                linkedOutputDir,
                testInput,
                testGeneratedOutputDir,
                libraries,
                compileTests,
                includeJavaLibResources,
                readCompilerVersion(),
                err
        );
    }

    public static int compileGenerate(
            OutputType outputType,
            Path input,
            Path generatedOutputDir,
            Path linkedOutputDir,
            Path testInput,
            Path testGeneratedOutputDir,
            TreeSet<CompiledModule> libraries,
            boolean compileTests,
            boolean includeJavaLibResources,
            String compilerVersion,
            PrintStream err
    ) {
        try {
            return compileGenerateOrThrow(
                    outputType,
                    input,
                    generatedOutputDir,
                    linkedOutputDir,
                    testInput,
                    testGeneratedOutputDir,
                    libraries,
                    compileTests,
                    includeJavaLibResources,
                    compilerVersion,
                    err
            );
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
            case "compile-generate" -> executeCompileGenerate(Arrays.copyOfRange(args, 1, args.length), err);
            case "generate" -> executeGenerate(Arrays.copyOfRange(args, 1, args.length), err);
            case "package" -> executePackage(Arrays.copyOfRange(args, 1, args.length), err);
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
        var options = parseNamedOptions(args, true, false, false, false);
        configureLogging(options.logLevel());

        var input = requiredPath(options.values(), "input", "compile");
        var output = requiredPath(options.values(), "output", "compile");
        var libraries = readLibraryModules(options.values().get("libs"));
        var compileTests = options.values().containsKey("compile-tests");

        return compile(input, output, libraries, compileTests, err, readCompilerVersion());
    }

    private static int executeCompileGenerate(String[] args, PrintStream err) {
        if (args.length == 0) {
            throw new CliException("Missing output type for `compile-generate`.\n\n" + helpText());
        }

        var outputType = parseOutputType(args[0]);
        var options = parseNamedOptions(Arrays.copyOfRange(args, 1, args.length), true, true, true, true);
        configureLogging(options.logLevel());

        var input = requiredPath(options.values(), "input", "compile-generate");
        var output = requiredPath(options.values(), "output", "compile-generate");
        var linkedOutput = optionalPath(options.values(), "linked-output");
        var testInput = optionalPath(options.values(), "test-input");
        var testOutput = optionalPath(options.values(), "test-output");
        var libraries = readLibraryModules(options.values().get("libs"));
        var compileTests = options.values().containsKey("compile-tests");
        var includeJavaLibResources = !options.values().containsKey("skip-java-lib");

        return compileGenerate(
                outputType,
                input,
                output,
                linkedOutput,
                testInput,
                testOutput,
                libraries,
                compileTests,
                includeJavaLibResources,
                readCompilerVersion(),
                err
        );
    }

    private static int executeGenerate(String[] args, PrintStream err) {
        if (args.length == 0) {
            throw new CliException("Missing output type for `generate`.\n\n" + helpText());
        }

        var outputType = parseOutputType(args[0]);
        var options = parseNamedOptions(Arrays.copyOfRange(args, 1, args.length), false, true, false, false);
        configureLogging(options.logLevel());

        var input = options.values().containsKey("input") ? Path.of(options.values().get("input")) : Path.of(".");
        var output = requiredPath(options.values(), "output", "generate");
        var includeJavaLibResources = !options.values().containsKey("skip-java-lib");
        return generate(outputType, input, output, includeJavaLibResources, err);
    }

    private static int executePackage(String[] args, PrintStream err) {
        var options = parsePackageOptions(args);
        configureLogging(options.logLevel());
        return packageCode(options, err);
    }

    private static NamedOptions parseNamedOptions(
            String[] args,
            boolean allowLibs,
            boolean allowSkipJavaLib,
            boolean allowLinkedOutput,
            boolean allowTestOutputs
    ) {
        var values = new LinkedHashMap<String, String>();
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
                case "--linked-output" -> {
                    if (!allowLinkedOutput) {
                        throw new CliException("Option `" + arg + "` is supported only for `compile-generate`.");
                    }
                    var value = nextValue(args, i, arg);
                    values.put("linked-output", value);
                    i++;
                }
                case "--test-input", "--test-output" -> {
                    if (!allowTestOutputs) {
                        throw new CliException("Option `" + arg + "` is supported only for `compile-generate`.");
                    }
                    var value = nextValue(args, i, arg);
                    values.put(arg.substring(2), value);
                    i++;
                }
                case "--type" -> {
                    var value = nextValue(args, i, arg);
                    values.put("type", value);
                    i++;
                }
                case "--runtime" -> {
                    var value = nextValue(args, i, arg);
                    values.put("runtime", value);
                    i++;
                }
                case "-l", "--libs" -> {
                    if (!allowLibs) {
                        throw new CliException("Option `" + arg + "` is supported only for `compile` and `compile-generate`.");
                    }
                    var value = nextValue(args, i, arg);
                    values.put("libs", value);
                    i++;
                }
                case "--compile-tests" -> {
                    if (!allowLibs) {
                        throw new CliException("Option `" + arg + "` is supported only for `compile` and `compile-generate`.");
                    }
                    values.put("compile-tests", "true");
                }
                case "--skip-java-lib" -> {
                    if (!allowSkipJavaLib) {
                        throw new CliException("Option `" + arg + "` is supported only for `generate` and `compile-generate`.");
                    }
                    values.put("skip-java-lib", "true");
                }
                default -> throw new CliException("Unknown option `" + arg + "`.\n\n" + helpText());
            }
        }

        return new NamedOptions(values, logLevel);
    }

    private static PackageOptions parsePackageOptions(String[] args) {
        var values = new LinkedHashMap<String, String>();
        var capyOverrides = new LinkedHashMap<String, String>();
        var logLevel = Level.INFO;

        for (int i = 0; i < args.length; i++) {
            var arg = args[i];
            switch (arg) {
                case "--log" -> {
                    var value = nextValue(args, i, arg);
                    logLevel = parseLogLevel(value);
                    i++;
                }
                case "-ci", "--compiled-input" -> {
                    var value = nextValue(args, i, arg);
                    values.put("compiled-input", value);
                    i++;
                }
                case "-i", "--input" -> {
                    var value = nextValue(args, i, arg);
                    values.put("input", value);
                    i++;
                }
                case "-m", "--module" -> {
                    var value = nextValue(args, i, arg);
                    values.put("module", value);
                    i++;
                }
                default -> {
                    if (!arg.startsWith("--capy.")) {
                        throw new CliException("Unknown option `" + arg + "`.\n\n" + helpText());
                    }
                    var value = nextValue(args, i, arg);
                    capyOverrides.put(arg.substring("--capy.".length()), value);
                    i++;
                }
            }
        }

        var compiledInput = values.containsKey("compiled-input") ? Path.of(values.get("compiled-input")) : null;
        var input = values.containsKey("input") ? Path.of(values.get("input")) : null;
        var module = requiredPath(values, "module", "package");
        if (compiledInput == null && input == null) {
            throw new CliException("Package requires either `--compiled-input` or `--input`.\n\n" + helpText());
        }
        if (compiledInput != null && input != null) {
            throw new CliException("Package accepts only one of `--compiled-input` or `--input`.\n\n" + helpText());
        }
        return new PackageOptions(compiledInput, input, module, capyOverrides, logLevel);
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

    private static String requiredValue(Map<String, String> values, String key, String command) {
        var value = values.get(key);
        if (value == null || value.isBlank()) {
            throw new CliException("Missing required option `--" + key + "` for `" + command + "`.\n\n" + helpText());
        }
        return value;
    }

    private static Path optionalPath(Map<String, String> values, String key) {
        var value = values.get(key);
        if (value == null || value.isBlank()) {
            return null;
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

    private static int packageCode(PackageOptions options, PrintStream err) {
        try {
            return packageOrThrow(options, err);
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

    private static int compileOrThrow(
            Path input,
            Path linkedOutputDir,
            TreeSet<CompiledModule> libraries,
            boolean compileTests,
            PrintStream err,
            String compilerVersion
    ) throws IOException {
        validateOutputDirectory(linkedOutputDir, "Compile output path");
        var compilation = compileSources(input, libraries, compileTests, err);
        if (compilation == null) {
            return EXIT_COMPILATION_ERROR;
        }
        writeCompilationOutput(linkedOutputDir, compilation, compilerVersion);
        return EXIT_SUCCESS;
    }

    private static int packageOrThrow(PackageOptions options, PrintStream err) throws IOException {
        validateModuleFile(options.module());
        var compiledInput = findCompiledInput(options, err);
        validateInputDirectory(compiledInput);

        var packageDefinition = readModuleDefinition(options.module());
        packageDefinition.put("capybara_compiler_version", readCompilerVersion());
        packageDefinition.put("build_date_time", OffsetDateTime.now().toString());
        packageDefinition.put("os", System.getProperty("os.name"));
        packageDefinition.putAll(options.capyOverrides());

        writePackageArchive(options.module().resolveSibling(PACKAGE_FILE), compiledInput, packageDefinition);
        return EXIT_SUCCESS;
    }

    private static int generateOrThrow(
            OutputType outputType,
            Path linkedInputDir,
            Path generatedOutputDir,
            boolean includeJavaLibResources
    ) throws IOException {
        validateInputDirectory(linkedInputDir);
        validateOutputDirectory(generatedOutputDir, "Generated output path");

        var generationInput = selectGenerationInput(linkedInputDir, readLinkedProgram(linkedInputDir, true), includeJavaLibResources);
        generateProgram(outputType, generatedOutputDir, generationInput);
        return EXIT_SUCCESS;
    }

    private static int compileGenerateOrThrow(
            OutputType outputType,
            Path input,
            Path generatedOutputDir,
            Path linkedOutputDir,
            Path testInput,
            Path testGeneratedOutputDir,
            TreeSet<CompiledModule> libraries,
            boolean compileTests,
            boolean includeJavaLibResources,
            String compilerVersion,
            PrintStream err
    ) throws IOException {
        if ((testInput == null) != (testGeneratedOutputDir == null)) {
            throw new CliException("`compile-generate` requires both `--test-input` and `--test-output` when either is provided.");
        }
        var compilation = compileSources(input, libraries, compileTests, err);
        if (compilation == null) {
            return EXIT_COMPILATION_ERROR;
        }
        if (linkedOutputDir != null) {
            validateOutputDirectory(linkedOutputDir, "Compile output path");
            writeCompilationOutput(linkedOutputDir, compilation, compilerVersion);
        }
        validateOutputDirectory(generatedOutputDir, "Generated output path");
        var mainGenerationInput = selectGenerationInput(compilation.program(), compilation.sourceModules(), includeJavaLibResources);
        if (testInput != null) {
            var testCompilation = compileSources(testInput, mergeLibraries(libraries, compilation), true, err);
            if (testCompilation == null) {
                return EXIT_COMPILATION_ERROR;
            }
            validateOutputDirectory(testGeneratedOutputDir, "Generated test output path");
            var testGenerationInput = selectGenerationInput(
                    testCompilation.program(),
                    testCompilation.sourceModules(),
                    includeJavaLibResources
            );
            if (generatedOutputDir.equals(testGeneratedOutputDir)) {
                generatePrograms(outputType, generatedOutputDir, List.of(mainGenerationInput, testGenerationInput));
            } else {
                generateProgram(outputType, generatedOutputDir, mainGenerationInput);
                generateProgram(outputType, testGeneratedOutputDir, testGenerationInput);
            }
            return EXIT_SUCCESS;
        }
        generateProgram(outputType, generatedOutputDir, mainGenerationInput);
        return EXIT_SUCCESS;
    }

    private static TreeSet<CompiledModule> mergeLibraries(TreeSet<CompiledModule> libraries, CompilationArtifacts compilation) {
        var mergedLibraries = new TreeSet<>(libraries);
        mergedLibraries.addAll(compilation.program().modules());
        return mergedLibraries;
    }

    static CompilationArtifacts compileSources(Path input, TreeSet<CompiledModule> libraries, boolean compileTests, PrintStream err) throws IOException {
        validateInputDirectory(input);

        log.info("Compiling files from: " + input);
        var rawModuleBuildStartedAt = System.nanoTime();
        var rawModules = listSourceFiles(input).stream()
                .map(Capy::buildModule)
                .toList();
        log.info("Built " + rawModules.size() + " raw modules from " + input + " in " + Duration.ofNanos(System.nanoTime() - rawModuleBuildStartedAt));

        var linkingStartedAt = System.nanoTime();
        var linking = CapybaraCompiler.INSTANCE.compile(rawModules, libraries);
        log.info("Linked " + rawModules.size() + " modules from " + input + " in " + Duration.ofNanos(System.nanoTime() - linkingStartedAt));
        if (linking instanceof Result.Error<CompiledProgram> error) {
            err.println("Compilation failed with " + error.errors().size() + " error(s):");
            error.errors().forEach(err::println);
            return null;
        }

        var linkedProgram = ((Result.Success<CompiledProgram>) linking).value();
        var testAugmentationStartedAt = System.nanoTime();
        var outputProgram = compileTests ? prepareCompiledTests(linkedProgram) : linkedProgram;
        if (compileTests) {
            log.info("Prepared compiled tests for " + input + " in " + Duration.ofNanos(System.nanoTime() - testAugmentationStartedAt));
        }
        var sourceModules = rawModules.stream()
                .map(module -> new ModuleRef(module.name(), normalizeModulePath(module.path())))
                .collect(java.util.stream.Collectors.toCollection(java.util.TreeSet::new));
        if (compileTests) {
            sourceModules.add(CAP_TEST_RUNTIME_MODULE);
        }
        return new CompilationArtifacts(outputProgram, List.copyOf(sourceModules));
    }

    static void writeCompilationOutput(Path outputDir, CompilationArtifacts compilation, String compilerVersion) {
        writeLinkedModules(outputDir, compilation.program());
        writeBuildInfo(outputDir, compilerVersion, compilation.sourceModules());
    }

    static void generateCompiledProgram(OutputType outputType, Path generatedOutputDir, CompilationArtifacts compilation) throws IOException {
        generateCompiledProgram(outputType, generatedOutputDir, compilation, true);
    }

    static void generateCompiledProgram(
            OutputType outputType,
            Path generatedOutputDir,
            CompilationArtifacts compilation,
            boolean includeJavaLibResources
    ) throws IOException {
        validateOutputDirectory(generatedOutputDir, "Generated output path");
        generateProgram(outputType, generatedOutputDir, selectGenerationInput(compilation.program(), compilation.sourceModules(), includeJavaLibResources));
    }
    private static Path findCompiledInput(PackageOptions options, PrintStream err) throws IOException {
        if (options.compiledInput() != null) {
            return options.compiledInput();
        }

        var tempDir = Files.createTempDirectory("capy-package-");
        var exitCode = compileOrThrow(options.input(), tempDir, new TreeSet<>(), false, err, readCompilerVersion());
        if (exitCode != EXIT_SUCCESS) {
            throw new CliException("Unable to compile package input from: " + options.input());
        }
        return tempDir;
    }

    private static Set<Path> copyJavaLibResources(Path generatedOutputDir) {
        log.info("Copying bundled java-lib sources to: " + generatedOutputDir);
        var startedAt = System.nanoTime();
        try {
            var resourceUri = Capy.class.getResource(JAVA_LIB_RESOURCE_DIR).toURI();
            if ("jar".equals(resourceUri.getScheme())) {
                try (var openedFileSystem = openJarResourceFileSystem(resourceUri)) {
                    var copiedFiles = copyDirectoryContents(openedFileSystem.fileSystem().getPath(JAVA_LIB_RESOURCE_DIR), generatedOutputDir);
                    var duration = Duration.ofNanos(System.nanoTime() - startedAt);
                    log.info("Copied bundled java-lib sources to: " + generatedOutputDir + " in " + duration);
                    return copiedFiles;
                }
            } else {
                var copiedFiles = copyDirectoryContents(Path.of(resourceUri), generatedOutputDir);
                var duration = Duration.ofNanos(System.nanoTime() - startedAt);
                log.info("Copied bundled java-lib sources to: " + generatedOutputDir + " in " + duration);
                return copiedFiles;
            }
        } catch (URISyntaxException | IOException e) {
            throw new UncheckedIOException("Unable to copy bundled java-lib sources", e instanceof IOException io ? io : new IOException(e));
        }
    }

    private static OpenedFileSystem openJarResourceFileSystem(java.net.URI resourceUri) throws IOException {
        try {
            var fileSystem = FileSystems.newFileSystem(resourceUri, Map.of());
            return new OpenedFileSystem(fileSystem, fileSystem);
        } catch (FileSystemAlreadyExistsException ignored) {
            var fileSystem = FileSystems.getFileSystem(resourceUri);
            return new OpenedFileSystem(fileSystem, () -> {
            });
        }
    }

    private record OpenedFileSystem(java.nio.file.FileSystem fileSystem, java.io.Closeable closeAction) implements AutoCloseable {
        @Override
        public void close() throws IOException {
            closeAction.close();
        }
    }

    private static Set<Path> copyDirectoryContents(Path sourceDir, Path targetDir) throws IOException {
        var copiedFiles = new HashSet<Path>();
        try (var files = Files.walk(sourceDir)) {
            try {
                files.forEach(source -> {
                    var relative = sourceDir.relativize(source);
                    var target = targetDir.resolve(relative.toString());
                    try {
                        if (Files.isDirectory(source)) {
                            Files.createDirectories(target);
                        } else {
                            var parent = target.getParent();
                            if (parent != null) {
                                Files.createDirectories(parent);
                            }
                            copyFileIfChanged(source, target);
                            copiedFiles.add(relative.normalize());
                        }
                    } catch (IOException e) {
                        throw new UncheckedIOException(e);
                    }
                });
            } catch (UncheckedIOException e) {
                throw e.getCause();
            }
        }
        return copiedFiles;
    }

    private static Set<Path> writeGeneratedProgram(Path outputDir, GeneratedProgram program) {
        var writtenFiles = new HashSet<Path>();
        program.modules().forEach(module -> {
            writeCompiledModule(outputDir, module.relativePath(), module.code());
            writtenFiles.add(module.relativePath().normalize());
        });
        return writtenFiles;
    }

    private static void generateProgram(OutputType outputType, Path generatedOutputDir, GenerationInput generationInput) {
        generatePrograms(outputType, generatedOutputDir, List.of(generationInput));
    }

    private static void generatePrograms(OutputType outputType, Path generatedOutputDir, List<GenerationInput> generationInputs) {
        log.info("Generating " + outputType + " sources");
        var generationStartedAt = System.nanoTime();
        var generatedPrograms = generationInputs.stream()
                .map(generationInput -> Generator.findGenerator(outputType).generate(generationInput.program()))
                .toList();
        log.info("Generated " + outputType + " sources in " + Duration.ofNanos(System.nanoTime() - generationStartedAt));

        log.info("Writing generated " + outputType + " sources to: " + generatedOutputDir);
        var writeStartedAt = System.nanoTime();
        var writtenFiles = new HashSet<Path>();
        for (var generatedProgram : generatedPrograms) {
            writtenFiles.addAll(writeGeneratedProgram(generatedOutputDir, generatedProgram));
        }
        log.info("Wrote generated " + outputType + " sources to: " + generatedOutputDir + " in " + Duration.ofNanos(System.nanoTime() - writeStartedAt));
        if (outputType == OutputType.JAVA && generationInputs.stream().anyMatch(GenerationInput::includeJavaLibResources)) {
            writtenFiles.addAll(copyJavaLibResources(generatedOutputDir));
        }
        deleteStaleFiles(generatedOutputDir, writtenFiles);
    }

    private static void compileGeneratedProgram(Path sourceDir, Path classesDir) throws IOException {
        var compiler = ToolProvider.getSystemJavaCompiler();
        if (compiler == null) {
            throw new IllegalStateException("Java compiler is not available. Use a JDK to run `capy test`.");
        }

        Files.createDirectories(classesDir);
        List<Path> javaFiles;
        try (var files = Files.walk(sourceDir)) {
            javaFiles = files
                    .filter(Files::isRegularFile)
                    .filter(path -> path.getFileName().toString().endsWith(".java"))
                    .toList();
        }

        if (javaFiles.isEmpty()) {
            throw new CliException("No generated Java sources found for test execution.");
        }

        var diagnostics = new DiagnosticCollector<JavaFileObject>();
        var output = new StringWriter();
        try (var fileManager = compiler.getStandardFileManager(diagnostics, Locale.ROOT, StandardCharsets.UTF_8)) {
            var compilationUnits = fileManager.getJavaFileObjectsFromFiles(javaFiles.stream().map(Path::toFile).toList());
            var options = List.of(
                    "--release", "21",
                    "-classpath", System.getProperty("java.class.path"),
                    "-d", classesDir.toString()
            );
            var success = compiler.getTask(new PrintWriter(output), fileManager, diagnostics, options, null, compilationUnits).call();
            if (!Boolean.TRUE.equals(success)) {
                var details = diagnostics.getDiagnostics().stream()
                        .map(Objects::toString)
                        .collect(java.util.stream.Collectors.joining(System.lineSeparator()));
                var compilerOutput = output.toString();
                throw new CliException("Unable to compile generated Capybara tests."
                                       + System.lineSeparator()
                                       + details
                                       + (compilerOutput.isEmpty() ? "" : System.lineSeparator() + compilerOutput));
            }
        }
    }

    private static List<SuiteResult> executeTestMethods(GeneratedProgram program, Path classesDir) throws IOException {
        var classNames = program.modules().stream()
                .map(module -> toClassName(module.relativePath()))
                .filter(name -> !name.isBlank())
                .distinct()
                .toList();

        try (var classLoader = new URLClassLoader(new URL[]{classesDir.toUri().toURL()}, Capy.class.getClassLoader())) {
            var results = new java.util.ArrayList<SuiteResult>();
            for (var className : classNames) {
                var type = classLoader.loadClass(className);
                for (var method : type.getDeclaredMethods()) {
                    if (!isTestMethod(method)) {
                        continue;
                    }
                    results.addAll(invokeTestMethod(method));
                }
            }
            return mergeSuiteResults(results);
        } catch (ReflectiveOperationException e) {
            throw new IllegalStateException("Unable to execute generated Capybara tests", e);
        }
    }

    private static String toClassName(Path relativePath) {
        var normalized = relativePath.toString().replace('\\', '/');
        if (!normalized.endsWith(".java")) {
            return "";
        }
        return normalized.substring(0, normalized.length() - ".java".length()).replace('/', '.');
    }

    private static boolean isTestMethod(Method method) {
        if (!Modifier.isPublic(method.getModifiers())
            || !Modifier.isStatic(method.getModifiers())
            || method.getParameterCount() != 0
            || !"tests".equals(method.getName())) {
            return false;
        }
        var returnType = method.getReturnType();
        return "TestSuiteResults".equals(returnType.getSimpleName())
               || "TestSuites".equals(returnType.getSimpleName())
               || "TestFile".equals(returnType.getSimpleName())
               || List.class.isAssignableFrom(returnType);
    }

    private static List<SuiteResult> invokeTestMethod(Method method) throws InvocationTargetException, IllegalAccessException {
        var value = method.invoke(null);
        if (value == null) {
            return List.of();
        }
        if (value instanceof List<?> list) {
            return list.stream()
                    .flatMap(item -> toSuiteResults(item).stream())
                    .toList();
        }
        return toSuiteResults(value);
    }

    private static List<SuiteResult> toSuiteResults(Object value) {
        if (isSuiteResultObject(value)) {
            return List.of(toSuiteResult(value));
        }
        if (isTestSuitesObject(value)) {
            return toSuiteResultsFromSuites(value);
        }
        if (isTestFileObject(value)) {
            return List.of(toSuiteResultFromTestFile(value));
        }
        return List.of();
    }

    private static boolean isSuiteResultObject(Object value) {
        return value != null && "TestSuiteResults".equals(value.getClass().getSimpleName());
    }

    private static boolean isTestSuitesObject(Object value) {
        return value != null && "TestSuites".equals(value.getClass().getSimpleName());
    }

    private static boolean isTestFileObject(Object value) {
        return value != null && "TestFile".equals(value.getClass().getSimpleName());
    }

    private static SuiteResult toSuiteResult(Object suiteResult) {
        var suite = (String) invokeAccessor(suiteResult, "suite");
        var testResults = ((List<?>) invokeAccessor(suiteResult, "results")).stream()
                .map(Capy::toTestResult)
                .toList();
        return new SuiteResult(suite, testResults);
    }

    private static TestResultRow toTestResult(Object testResult) {
        var suite = (String) invokeAccessor(testResult, "suite");
        var name = (String) invokeAccessor(testResult, "name");
        var passed = (Boolean) invokeAccessor(testResult, "passed");
        var failures = ((List<?>) invokeAccessor(testResult, "failures")).stream().map(String::valueOf).toList();
        return new TestResultRow(suite, name, passed, failures);
    }

    private static List<SuiteResult> toSuiteResultsFromSuites(Object testSuites) {
        var modules = (List<?>) invokeAccessor(testSuites, "modules");
        return modules.stream()
                .filter(Capy::isTestFileObject)
                .map(Capy::toSuiteResultFromTestFile)
                .toList();
    }

    private static SuiteResult toSuiteResultFromTestFile(Object testFile) {
        var fileName = String.valueOf(invokeAccessor(testFile, "file_name"));
        var suite = suiteNameFromTestFile(fileName);
        var testCases = (List<?>) invokeAccessor(testFile, "test_cases");
        var results = testCases.stream()
                .map(testCase -> toTestResultFromTestCase(suite, testCase))
                .toList();
        return new SuiteResult(suite, results);
    }

    private static TestResultRow toTestResultFromTestCase(String suite, Object testCase) {
        var name = String.valueOf(invokeAccessor(testCase, "name"));
        var asserts = (List<?>) invokeAccessor(testCase, "asserts");
        var failures = asserts.stream()
                .flatMap(assertion -> assertionFailures(assertion).stream())
                .toList();
        return new TestResultRow(suite, name, failures.isEmpty(), failures);
    }

    private static List<String> assertionFailures(Object assertion) {
        var assertions = (List<?>) invokeAccessor(assertion, "assertions");
        return assertions.stream()
                .filter(item -> !Boolean.TRUE.equals(invokeAccessor(item, "result")))
                .map(item -> String.valueOf(invokeAccessor(item, "message")))
                .toList();
    }

    private static String suiteNameFromTestFile(String fileName) {
        var normalized = fileName.replace('\\', '/');
        if (normalized.endsWith(".cfun")) {
            normalized = normalized.substring(0, normalized.length() - ".cfun".length());
        }
        while (normalized.startsWith("/")) {
            normalized = normalized.substring(1);
        }
        return normalized.replace('/', '.');
    }

    private static Object invokeAccessor(Object target, String name) {
        try {
            return target.getClass().getMethod(name).invoke(target);
        } catch (ReflectiveOperationException e) {
            throw new IllegalStateException("Unable to read `" + name + "` from `" + target.getClass().getName() + "`", e);
        }
    }

    private static List<SuiteResult> mergeSuiteResults(List<SuiteResult> results) {
        var merged = new LinkedHashMap<String, java.util.ArrayList<TestResultRow>>();
        for (var result : results) {
            merged.computeIfAbsent(result.suite(), ignored -> new java.util.ArrayList<>()).addAll(result.results());
        }
        return merged.entrySet().stream()
                .map(entry -> new SuiteResult(entry.getKey(), List.copyOf(entry.getValue())))
                .toList();
    }

    private static int writeJUnitResults(Path outputDir, List<SuiteResult> suiteResults, PrintStream err) throws IOException {
        Files.createDirectories(outputDir);
        var failedTests = 0;
        var totalTests = 0;
        for (var suiteResult : suiteResults) {
            failedTests += (int) suiteResult.results().stream().filter(test -> !test.passed()).count();
            totalTests += suiteResult.results().size();
            var report = outputDir.resolve("TEST-" + suiteResult.suite() + ".xml");
            Files.writeString(report, renderJUnitSuite(suiteResult), StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE);
        }

        if (failedTests > 0) {
            err.println("FAIL Some tests failed (" + failedTests + "/" + totalTests + ")");
            suiteResults.stream()
                    .flatMap(suite -> suite.results().stream())
                    .filter(test -> !test.passed())
                    .forEach(test -> {
                        err.println("FAIL " + test.suite() + "." + test.name());
                        test.failures().forEach(failure -> err.println("  " + failure));
                    });
            return EXIT_FAILURE;
        }

        return EXIT_SUCCESS;
    }

    private static String renderJUnitSuite(SuiteResult suiteResult) {
        var failures = suiteResult.results().stream().filter(test -> !test.passed()).count();
        var testcases = suiteResult.results().stream()
                .map(Capy::renderJUnitTestCase)
                .collect(java.util.stream.Collectors.joining(System.lineSeparator()));
        return "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
               + "<testsuite name=\"" + xmlEscape(suiteResult.suite()) + "\" tests=\"" + suiteResult.results().size()
               + "\" failures=\"" + failures + "\" errors=\"0\" skipped=\"0\">\n"
               + testcases + "\n"
               + "</testsuite>\n";
    }

    private static String renderJUnitTestCase(TestResultRow testResult) {
        if (testResult.passed()) {
            return "  <testcase classname=\"" + xmlEscape(testResult.suite()) + "\" name=\"" + xmlEscape(testResult.name()) + "\"/>";
        }
        var failureText = String.join("\n", testResult.failures());
        return "  <testcase classname=\"" + xmlEscape(testResult.suite()) + "\" name=\"" + xmlEscape(testResult.name()) + "\">\n"
               + "    <failure message=\"" + xmlEscape(testResult.name()) + "\">"
               + xmlEscape(failureText)
               + "</failure>\n"
               + "  </testcase>";
    }

    private static String xmlEscape(String value) {
        return value
                .replace("&", "&amp;")
                .replace("<", "&lt;")
                .replace(">", "&gt;")
                .replace("\"", "&quot;")
                .replace("'", "&apos;");
    }

    private static Map<String, Object> readModuleDefinition(Path moduleFile) {
        try {
            var loaded = yaml().load(Files.readString(moduleFile));
            if (loaded == null) {
                return new LinkedHashMap<>();
            }
            if (!(loaded instanceof Map<?, ?> map)) {
                throw new CliException("Module file must contain a YAML object: " + moduleFile);
            }
            var definition = new LinkedHashMap<String, Object>();
            map.forEach((key, value) -> definition.put(String.valueOf(key), value));
            return definition;
        } catch (IOException e) {
            throw new UncheckedIOException("Unable to read module YAML: " + moduleFile, e);
        }
    }

    private static void writePackageArchive(Path outputFile, Path compiledInputDir, Map<String, Object> moduleDefinition) {
        try {
            var parent = outputFile.getParent();
            if (parent != null) {
                Files.createDirectories(parent);
            }
            try (var zip = new ZipOutputStream(Files.newOutputStream(outputFile))) {
                writeZipEntry(zip, MODULE_FILE, yaml().dump(moduleDefinition));
                try (var files = Files.walk(compiledInputDir)) {
                    for (var path : files.filter(Files::isRegularFile).toList()) {
                        var entryName = compiledInputDir.relativize(path).toString().replace('\\', '/');
                        zip.putNextEntry(new ZipEntry(entryName));
                        Files.copy(path, zip);
                        zip.closeEntry();
                    }
                }
            }
            log.info("Writing package archive to file: " + outputFile);
        } catch (IOException e) {
            throw new UncheckedIOException("Unable to write package archive: " + outputFile, e);
        }
    }

    private static void writeZipEntry(ZipOutputStream zip, String entryName, String content) throws IOException {
        zip.putNextEntry(new ZipEntry(entryName));
        zip.write(content.getBytes(StandardCharsets.UTF_8));
        zip.closeEntry();
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

    private static void validateModuleFile(Path moduleFile) {
        if (Files.notExists(moduleFile)) {
            throw new CliException("Module file does not exist: " + moduleFile);
        }
        if (!Files.isRegularFile(moduleFile)) {
            throw new CliException("Module path is not a file: " + moduleFile);
        }
    }

    private static void validateOutputDirectory(Path directory, String label) throws IOException {
        if (Files.notExists(directory)) {
            Files.createDirectories(directory);
            return;
        }
        if (!Files.isDirectory(directory)) {
            throw new CliException(label + " is not a directory: " + directory);
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
            var startedAt = System.nanoTime();
            writeStringIfChanged(absolutePath, code);
            var duration = Duration.ofNanos(System.nanoTime() - startedAt);
            log.info("Wrote module to file: " + absolutePath + " in " + duration);
        } catch (IOException e) {
            throw new UncheckedIOException("Unable to write file: " + absolutePath, e);
        }
    }

    private static void writeLinkedModules(Path outputDir, CompiledProgram program) {
        log.info("Writing " + program.modules().size() + " linked modules to: " + outputDir);
        var totalStartedAt = System.nanoTime();
        try {
            Files.createDirectories(outputDir);
            var writtenFiles = new HashSet<Path>();
            var programFile = outputDir.resolve(PROGRAM_FILE);
            log.info("Writing linked program to file: " + programFile);
            var programStartedAt = System.nanoTime();
            writeJsonIfChanged(programFile, PRETTY_JSON_WRITER.writeValueAsBytes(program));
            log.info("Wrote linked program to file: " + programFile + " in " + Duration.ofNanos(System.nanoTime() - programStartedAt));
            writtenFiles.add(Path.of(PROGRAM_FILE));
            for (var module : program.modules()) {
                var modulePath = module.path().replace('\\', '/');
                var moduleJson = modulePath.isBlank()
                        ? outputDir.resolve(module.name() + CompiledModule.EXTENSION)
                        : outputDir.resolve(modulePath).resolve(module.name() + CompiledModule.EXTENSION);
                Files.createDirectories(moduleJson.getParent());
                log.info("Writing linked module to file: " + moduleJson);
                var startedAt = System.nanoTime();
                writeJsonIfChanged(moduleJson, PRETTY_JSON_WRITER.writeValueAsBytes(module));
                var duration = Duration.ofNanos(System.nanoTime() - startedAt);
                log.info("Wrote linked module to file: " + moduleJson + " in " + duration);
                writtenFiles.add(outputDir.relativize(moduleJson).normalize());
            }
            writtenFiles.add(Path.of(BUILD_INFO_FILE));
            deleteStaleFiles(outputDir, writtenFiles);
            log.info("Wrote " + program.modules().size() + " linked modules to: " + outputDir + " in " + Duration.ofNanos(System.nanoTime() - totalStartedAt));
        } catch (IOException e) {
            throw new UncheckedIOException("Unable to write linked JSON output to " + outputDir, e);
        }
    }

    private static void deleteStaleFiles(Path outputDir, Set<Path> expectedFiles) {
        var manifestFile = outputDir.resolve(OUTPUT_MANIFEST_FILE);
        try {
            var staleFiles = readStaleFilesFromManifest(outputDir, manifestFile, expectedFiles);
            for (var staleFile : staleFiles) {
                deleteFileIfExists(staleFile);
                deleteEmptyParentDirectories(staleFile.getParent(), outputDir);
            }
            writeOutputManifest(manifestFile, expectedFiles);
        } catch (IOException e) {
            throw new UncheckedIOException("Unable to prune stale output files: " + outputDir, e);
        }
    }

    private static List<Path> readStaleFilesFromManifest(Path outputDir, Path manifestFile, Set<Path> expectedFiles) throws IOException {
        if (Files.exists(manifestFile)) {
            try (var lines = Files.lines(manifestFile, StandardCharsets.UTF_8)) {
                return lines
                        .map(String::trim)
                        .filter(line -> !line.isEmpty())
                        .map(Path::of)
                        .map(Capy::normalizeRelativeOutputPath)
                        .filter(path -> !expectedFiles.contains(path))
                        .map(outputDir::resolve)
                        .toList();
            }
        }

        try (var paths = Files.walk(outputDir)) {
            return paths
                    .filter(path -> !path.equals(outputDir))
                    .filter(Files::isRegularFile)
                    .filter(path -> !path.equals(manifestFile))
                    .filter(path -> !expectedFiles.contains(normalizeRelativeOutputPath(outputDir.relativize(path))))
                    .sorted(Comparator.reverseOrder())
                    .toList();
        }
    }

    private static void writeOutputManifest(Path manifestFile, Set<Path> expectedFiles) throws IOException {
        var parent = manifestFile.getParent();
        if (parent != null) {
            Files.createDirectories(parent);
        }
        var manifestContents = expectedFiles.stream()
                .map(Capy::normalizeRelativeOutputPath)
                .map(Capy::normalizeRelativeOutputPathString)
                .sorted()
                .collect(java.util.stream.Collectors.joining(System.lineSeparator()));
        writeStringIfChanged(
                manifestFile,
                manifestContents.isEmpty() ? "" : manifestContents + System.lineSeparator()
        );
    }

    private static Path normalizeRelativeOutputPath(Path path) {
        return Path.of(normalizeRelativeOutputPathString(path));
    }

    private static String normalizeRelativeOutputPathString(Path path) {
        return path.normalize().toString().replace('\\', '/');
    }

    private static void deleteFileIfExists(Path file) {
        try {
            Files.deleteIfExists(file);
        } catch (IOException e) {
            throw new UncheckedIOException("Unable to delete stale output file: " + file, e);
        }
    }

    private static void deleteEmptyParentDirectories(Path directory, Path outputDir) {
        var current = directory;
        while (current != null && !current.equals(outputDir)) {
            try {
                Files.deleteIfExists(current);
            } catch (DirectoryNotEmptyException ignored) {
                return;
            } catch (IOException e) {
                throw new UncheckedIOException("Unable to delete stale output directory: " + current, e);
            }
            current = current.getParent();
        }
    }

    private static CompiledProgram prepareCompiledTests(CompiledProgram linkedProgram) {
        log.info("Preparing compiled tests for " + linkedProgram.modules().size() + " linked modules");
        var totalStartedAt = System.nanoTime();
        var discoverStartedAt = System.nanoTime();
        var testFunctions = discoverTestFunctions(linkedProgram);
        log.info("Discovered " + testFunctions.size() + " Capybara test producers in " + Duration.ofNanos(System.nanoTime() - discoverStartedAt));
        if (testFunctions.isEmpty()) {
            throw new CliException("No Capybara functions returning TestFile or list[TestFile] were found.");
        }

        var outputModules = new java.util.ArrayList<>(linkedProgram.modules());
        outputModules.removeIf(module -> module.name().equals(CAP_TEST_RUNTIME_MODULE.name()) && normalizeModulePath(module.path()).equals(CAP_TEST_RUNTIME_MODULE.path()));
        var runtimeModuleStartedAt = System.nanoTime();
        outputModules.add(createCapyTestRuntimeModule(testFunctions));
        log.info("Created CapyTestRuntime module in " + Duration.ofNanos(System.nanoTime() - runtimeModuleStartedAt));
        log.info("Prepared compiled tests in " + Duration.ofNanos(System.nanoTime() - totalStartedAt));
        return new CompiledProgram(outputModules);
    }

    private static List<TestFunctionRef> discoverTestFunctions(CompiledProgram linkedProgram) {
        return linkedProgram.modules().stream()
                .flatMap(module -> module.functions().stream()
                        .filter(function -> function.parameters().isEmpty())
                        .filter(function -> isTestProducerType(function.returnType()))
                        .map(function -> new TestFunctionRef(module, function)))
                .toList();
    }

    private static boolean isTestProducerType(dev.capylang.compiler.CompiledType returnType) {
        return isTestFileType(returnType.name()) || isTestFileListType(returnType);
    }

    private static boolean isTestFileType(String typeName) {
        return "TestFile".equals(typeName)
               || typeName.endsWith("/TestFile")
               || typeName.endsWith(".TestFile");
    }

    private static boolean isTestFileListType(dev.capylang.compiler.CompiledType returnType) {
        return returnType instanceof CollectionLinkedType.CompiledList listType
               && isTestFileType(listType.elementType().name());
    }

    private static CompiledModule createCapyTestRuntimeModule(List<TestFunctionRef> testFunctions) {
        return new CompiledModule(
                CAP_TEST_RUNTIME_MODULE.name(),
                CAP_TEST_RUNTIME_MODULE.path(),
                java.util.Map.of(),
                List.of(new CompiledFunction(
                "gather_tests",
                new CollectionLinkedType.CompiledList(testFileType()),
                List.of(),
                buildGatherTestsExpression(testFunctions),
                List.of(),
                false
        )),
                List.of(new CompiledModule.StaticImport("capy.test_.CapyTest", "TestFile"))
        );
    }

    private static CompiledExpression buildGatherTestsExpression(List<TestFunctionRef> testFunctions) {
        var testFileListType = new CollectionLinkedType.CompiledList(testFileType());
        return testFunctions.stream()
                .map(Capy::toGatherTestsExpression)
                .reduce((left, right) -> new CompiledInfixExpression(left, InfixOperator.PLUS, right, testFileListType))
                .orElseGet(() -> new CompiledNewList(List.of(), testFileListType));
    }

    private static CompiledExpression toGatherTestsExpression(TestFunctionRef testFunction) {
        var functionCall = new CompiledFunctionCall(
                moduleJavaClassName(testFunction.module()) + "." + testFunction.function().name(),
                List.of(),
                testFunction.function().returnType()
        );
        if (isTestFileListType(testFunction.function().returnType())) {
            return functionCall;
        }
        return new CompiledNewList(List.of(functionCall), new CollectionLinkedType.CompiledList(testFileType()));
    }

    private static dev.capylang.compiler.CompiledDataType testFileType() {
        return new dev.capylang.compiler.CompiledDataType("TestFile", List.of(), List.of(), List.of(), false);
    }

    private static String moduleJavaClassName(CompiledModule module) {
        var packageName = normalizeModulePath(module.path()).replace('/', '.');
        var className = module.types().containsKey(module.name()) ? module.name() + "Module" : module.name();
        return packageName.isBlank() ? className : packageName + "." + className;
    }

    private static String normalizeModulePath(String path) {
        return path.replace('\\', '/');
    }

    private static void writeBuildInfo(Path outputDir, String compilerVersion, List<ModuleRef> sourceModules) {
        var buildInfo = new BuildInfo(
                null,
                compilerVersion,
                sourceModules
        );
        try {
            Files.createDirectories(outputDir);
            var buildInfoFile = outputDir.resolve(BUILD_INFO_FILE);
            writeJsonIfChanged(buildInfoFile, PRETTY_JSON_WRITER.writeValueAsBytes(buildInfo));
            log.info("Writing build info to file: " + buildInfoFile);
        } catch (IOException e) {
            throw new UncheckedIOException("Unable to write build info JSON to " + outputDir, e);
        }
    }

    private static void writeJsonIfChanged(Path outputFile, byte[] content) throws IOException {
        writeBytesIfChanged(outputFile, content);
    }

    private static void writeStringIfChanged(Path outputFile, String content) throws IOException {
        writeBytesIfChanged(outputFile, content.getBytes(StandardCharsets.UTF_8));
    }

    private static void writeBytesIfChanged(Path outputFile, byte[] content) throws IOException {
        if (Files.isRegularFile(outputFile)) {
            if (Files.size(outputFile) == content.length) {
                var existing = Files.readAllBytes(outputFile);
                if (java.util.Arrays.equals(existing, content)) {
                    return;
                }
            }
        }
        Files.write(
                outputFile,
                content,
                StandardOpenOption.CREATE,
                StandardOpenOption.TRUNCATE_EXISTING,
                StandardOpenOption.WRITE
        );
    }

    private static void copyFileIfChanged(Path source, Path target) throws IOException {
        if (Files.isRegularFile(target) && Files.size(source) == Files.size(target) && Files.mismatch(source, target) == -1) {
            return;
        }
        Files.copy(source, target, java.nio.file.StandardCopyOption.REPLACE_EXISTING);
    }

    private static BuildInfo readBuildInfo(Path linkedInputDir) {
        var buildInfoFile = linkedInputDir.resolve(BUILD_INFO_FILE);
        if (Files.notExists(buildInfoFile)) {
            return null;
        }

        try (var input = Files.newInputStream(buildInfoFile)) {
            return objectMapper().readValue(input, BuildInfo.class);
        } catch (IOException e) {
            throw new UncheckedIOException("Unable to read build info JSON: " + buildInfoFile, e);
        }
    }

    private static GenerationInput selectGenerationInput(Path linkedInputDir, CompiledProgram linkedProgram, boolean includeJavaLibResources) {
        var buildInfo = readBuildInfo(linkedInputDir);
        return selectGenerationInput(linkedProgram, buildInfo == null ? null : buildInfo.source_modules(), includeJavaLibResources);
    }

    private static GenerationInput selectGenerationInput(
            CompiledProgram linkedProgram,
            List<ModuleRef> sourceModules,
            boolean includeJavaLibResources
    ) {
        if (sourceModules == null || sourceModules.isEmpty()) {
            return new GenerationInput(linkedProgram, includeJavaLibResources);
        }

        var sourceModuleRefs = new TreeSet<>(sourceModules);
        var filteredProgram = new CompiledProgram(linkedProgram.modules().stream()
                .filter(module -> sourceModuleRefs.contains(new ModuleRef(module.name(), normalizeModulePath(module.path()))))
                .toList());
        var shouldCopyJavaLibResources = includeJavaLibResources && !filteredProgram.modules().isEmpty();
        return new GenerationInput(filteredProgram, shouldCopyJavaLibResources);
    }

    static ObjectMapper objectMapper() {
        return OBJECT_MAPPER;
    }

    private static ObjectMapper createObjectMapper() {
        var mapper = new ObjectMapper();
        mapper.registerModule(new Jdk8Module());
        mapper.activateDefaultTyping(
                BasicPolymorphicTypeValidator.builder()
                        .allowIfSubType("dev.capylang")
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
                "  capy compile [-l|--libs <dir1,dir2,...>] [--compile-tests] -i|--input <dir> -o|--output <dir> [--log <DEBUG|INFO|WARN|ERROR>]",
                "  capy compile-generate <java|python|javascript|js> [-l|--libs <dir1,dir2,...>] [--compile-tests] -i|--input <dir> -o|--output <dir> [--linked-output <dir>] [--test-input <dir> --test-output <dir>] [--skip-java-lib] [--log <DEBUG|INFO|WARN|ERROR>]",
                "  capy generate <java|python|javascript|js> [-i|--input <dir>] -o|--output <dir> [--skip-java-lib] [--log <DEBUG|INFO|WARN|ERROR>]",
                "  capy package (-ci|--compiled-input <dir> | -i|--input <dir>) -m|--module <capy.yml> [--capy.<field> <value>] [--log <DEBUG|INFO|WARN|ERROR>]",
                "",
                "Notes:",
                "  compile output directory may be reused; stale generated files are pruned automatically.",
                "  compile-generate compiles Capybara sources directly to generated output without writing linked intermediates unless --linked-output is provided.",
                "  compile-generate --test-input/--test-output also compiles test Capybara sources against the freshly compiled main program in the same invocation.",
                "  compile --compile-tests writes bundled stdlib modules and injects discovered TestFile/list[TestFile] producers into capy/test/CapyTestRuntime.gather_tests.",
                "  generate input directory defaults to the current directory.",
                "  generate --skip-java-lib omits bundled Java runtime sources when the caller already has them on the compile classpath.",
                "  package writes `capy.cbin` next to the provided `capy.yml`."
        );
    }

    private static Yaml yaml() {
        var loaderOptions = new LoaderOptions();
        var dumperOptions = new DumperOptions();
        dumperOptions.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK);
        dumperOptions.setPrettyFlow(true);
        return new Yaml(loaderOptions, dumperOptions);
    }

    static CompiledProgram readLinkedProgram(Path linkedInputDir, boolean requireModules) {
        var programFile = linkedInputDir.resolve(PROGRAM_FILE);
        if (Files.isRegularFile(programFile)) {
            var program = readLinkedProgramFile(programFile);
            if (requireModules && program.modules().isEmpty()) {
                throw new CliException("Missing linked module files in directory: " + linkedInputDir);
            }
            return program;
        }
        try (var files = Files.walk(linkedInputDir)) {
            var modules = files
                    .filter(Files::isRegularFile)
                    .filter(path -> path.getFileName().toString().endsWith(CompiledModule.EXTENSION))
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

    private static CompiledProgram readLinkedProgramFile(Path linkedProgramFile) {
        try (var input = Files.newInputStream(linkedProgramFile)) {
            return objectMapper().readValue(input, CompiledProgram.class);
        } catch (IOException e) {
            throw new UncheckedIOException("Unable to read linked program JSON: " + linkedProgramFile, e);
        }
    }

    private static List<CompiledModule> readBundledLinkedModules() {
        try {
            var resource = Capy.class.getResource("/capy");
            if (resource == null) {
                return List.of();
            }
            var resourceUri = resource.toURI();
            if ("jar".equals(resourceUri.getScheme())) {
                try (var fileSystem = findOrCreateFileSystem(resourceUri)) {
                    return readBundledLinkedModules(fileSystem.getPath("/capy"));
                }
            }
            return readBundledLinkedModules(Path.of(resourceUri));
        } catch (URISyntaxException | IOException e) {
            throw new UncheckedIOException("Unable to read bundled linked Capybara libraries", e instanceof IOException io ? io : new IOException(e));
        }
    }

    private static java.nio.file.FileSystem findOrCreateFileSystem(java.net.URI resourceUri) throws IOException {
        try {
            return FileSystems.newFileSystem(resourceUri, Map.of());
        } catch (java.nio.file.FileSystemAlreadyExistsException ignored) {
            return FileSystems.getFileSystem(resourceUri);
        }
    }

    private static List<CompiledModule> readBundledLinkedModules(Path root) throws IOException {
        try (var files = Files.walk(root)) {
            return files
                    .filter(Files::isRegularFile)
                    .filter(path -> path.getFileName().toString().endsWith(CompiledModule.EXTENSION))
                    .map(Capy::readLinkedModule)
                    .toList();
        }
    }

    private static CompiledModule readLinkedModule(Path linkedModuleFile) {
        try (var input = Files.newInputStream(linkedModuleFile)) {
            return objectMapper().readValue(input, CompiledModule.class);
        } catch (IOException e) {
            throw new UncheckedIOException("Unable to read linked module JSON: " + linkedModuleFile, e);
        }
    }

    private static RawModule buildModule(SourceFile sourceFile) {
        log.info("Building module from file: " + sourceFile.path());
        var startedAt = System.nanoTime();
        var fileName = sourceFile.path().getFileName().toString();
        var fileNameWithoutExtension = fileName.substring(0, fileName.lastIndexOf('.'));
        var module = new RawModule(
                fileNameWithoutExtension,
                findModulePath(sourceFile),
                readFile(sourceFile.path())
        );
        var duration = Duration.ofNanos(System.nanoTime() - startedAt);
        log.info("Built module from file: " + sourceFile.path() + " in " + duration);
        return module;
    }

    private static List<SourceFile> listSourceFiles(Path directory) {
        try (var stream = Files.walk(directory)) {
            return stream
                    .filter(Files::isRegularFile)
                    .filter(Capy::isCapybaraSourceFile)
                    .map(path -> new SourceFile(directory, path))
                    .toList();
        } catch (IOException e) {
            throw new UncheckedIOException("Unable to list files for directory: " + directory, e);
        }
    }

    private static boolean isCapybaraSourceFile(Path path) {
        return path.getFileName().toString().endsWith(CAPYBARA_SOURCE_EXTENSION);
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

    private record PackageOptions(
            Path compiledInput,
            Path input,
            Path module,
            Map<String, String> capyOverrides,
            Level logLevel
    ) {
    }

    private record BuildInfo(
            String build_date_time,
            String capybara_compiler_version,
            List<ModuleRef> source_modules
    ) {
    }

    private record ModuleRef(String name, String path) implements Comparable<ModuleRef> {
        @Override
        public int compareTo(ModuleRef other) {
            var byPath = path.compareTo(other.path);
            return byPath != 0 ? byPath : name.compareTo(other.name);
        }
    }

    private record SuiteResult(String suite, List<TestResultRow> results) {
    }

    private record TestResultRow(String suite, String name, boolean passed, List<String> failures) {
    }

    private record GenerationInput(CompiledProgram program, boolean includeJavaLibResources) {
    }

    private record TestFunctionRef(CompiledModule module, CompiledFunction function) {
    }

    record CompilationArtifacts(CompiledProgram program, List<ModuleRef> sourceModules) {
    }
}














