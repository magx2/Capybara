package dev.capylang.cli;

import javax.tools.ToolProvider;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;

final class TestCommand {
    private static final String JAVASCRIPT_RUNNER = "/test-runner/js/run-capybara-tests.js";
    private static final String PYTHON_RUNNER = "/test-runner/python/run-capybara-tests.py";

    private TestCommand() {
    }

    static int execute(String[] args) {
        if (Arrays.asList(args).contains("--help") || Arrays.asList(args).contains("-h")) {
            printHelp();
            return 0;
        }
        try {
            var options = Options.parse(args);
            return switch (options.backend()) {
                case JAVA -> runJava(options);
                case JAVASCRIPT -> runScript(options, "node", JAVASCRIPT_RUNNER, ".js");
                case PYTHON -> runScript(options, "python3", PYTHON_RUNNER, ".py");
            };
        } catch (CommandException | IllegalArgumentException exception) {
            System.err.println(exception.getMessage());
            return 2;
        }
    }

    private static int runScript(
            Options options,
            String executable,
            String resource,
            String suffix
    ) throws CommandException {
        Path temporaryDirectory = null;
        try {
            temporaryDirectory = Files.createTempDirectory("capy-test-runner-");
            var runner = temporaryDirectory.resolve("run-capybara-tests" + suffix);
            extractResource(resource, runner);
            var command = new ArrayList<String>();
            command.add(executable);
            if ("python3".equals(executable)) {
                command.add("-u");
            }
            command.add(runner.toString());
            command.addAll(options.runnerArguments(true));
            return start(command, executable);
        } catch (IOException exception) {
            throw new CommandException("Unable to prepare the packaged %s test runner: %s"
                    .formatted(options.backend().displayName(), exception.getMessage()));
        } finally {
            deleteRecursively(temporaryDirectory);
        }
    }

    private static int runJava(Options options) throws CommandException {
        Path temporaryDirectory = null;
        try {
            var classPathRoot = options.generatedDir();
            var javaSources = regularFiles(options.generatedDir(), ".java");
            if (!javaSources.isEmpty()) {
                temporaryDirectory = Files.createTempDirectory("capy-test-java-");
                classPathRoot = temporaryDirectory.resolve("classes");
                Files.createDirectories(classPathRoot);
                compileJava(javaSources, classPathRoot, options.generatedDir());
            }

            if (options.outputDir() != null) {
                Files.createDirectories(options.outputDir());
            }
            var command = new ArrayList<String>();
            command.add(javaExecutable().toString());
            command.add("-cp");
            command.add(javaClassPath(classPathRoot, options.generatedDir()));
            command.add("dev.capylang.test.TestRunner");
            command.addAll(options.runnerArguments(false));
            return start(command, "java");
        } catch (IOException exception) {
            throw new CommandException("Unable to prepare generated Java tests: " + exception.getMessage());
        } finally {
            deleteRecursively(temporaryDirectory);
        }
    }

    private static void compileJava(List<Path> sources, Path output, Path generatedDir) throws CommandException, IOException {
        var compiler = ToolProvider.getSystemJavaCompiler();
        if (compiler == null) {
            throw new CommandException("Generated Java tests contain source files, but no Java compiler is available. Run Capybara with a JDK or pass a directory containing compiled test classes.");
        }
        try (var fileManager = compiler.getStandardFileManager(null, Locale.ROOT, null)) {
            var units = fileManager.getJavaFileObjectsFromPaths(sources);
            var options = List.of(
                    "-d", output.toString(),
                    "-classpath", System.getProperty("java.class.path") + java.io.File.pathSeparator + generatedDir
            );
            var succeeded = compiler.getTask(null, fileManager, null, options, null, units).call();
            if (!Boolean.TRUE.equals(succeeded)) {
                throw new CommandException("Unable to compile generated Java tests.");
            }
        }
    }

    private static int start(List<String> command, String runtime) throws CommandException {
        try {
            return new ProcessBuilder(command)
                    .inheritIO()
                    .start()
                    .waitFor();
        } catch (IOException exception) {
            throw new CommandException("Required %s runtime is unavailable. Ensure `%s` is installed and available on PATH."
                    .formatted(runtimeName(runtime), runtime));
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            throw new CommandException("Interrupted while waiting for the %s test runner.".formatted(runtimeName(runtime)));
        }
    }

    private static String runtimeName(String executable) {
        return switch (executable) {
            case "node" -> "Node.js";
            case "python3" -> "Python";
            default -> "Java";
        };
    }

    private static void extractResource(String resource, Path target) throws IOException, CommandException {
        try (InputStream input = TestCommand.class.getResourceAsStream(resource)) {
            if (input == null) {
                throw new CommandException("Packaged test runner resource is missing: " + resource);
            }
            Files.copy(input, target);
        }
    }

    private static List<Path> regularFiles(Path directory, String suffix) throws IOException {
        try (var paths = Files.walk(directory)) {
            return paths.filter(Files::isRegularFile)
                    .filter(path -> path.getFileName().toString().endsWith(suffix))
                    .sorted()
                    .toList();
        }
    }

    private static String javaClassPath(Path classes, Path generatedDir) {
        return classes + java.io.File.pathSeparator
                + generatedDir + java.io.File.pathSeparator
                + System.getProperty("java.class.path");
    }

    private static Path javaExecutable() {
        var executable = System.getProperty("os.name", "").toLowerCase(Locale.ROOT).contains("windows")
                ? "java.exe"
                : "java";
        return Path.of(System.getProperty("java.home"), "bin", executable);
    }

    private static void deleteRecursively(Path directory) {
        if (directory == null || Files.notExists(directory)) {
            return;
        }
        try {
            Files.walkFileTree(directory, new SimpleFileVisitor<>() {
                @Override
                public FileVisitResult visitFile(Path file, BasicFileAttributes attributes) throws IOException {
                    Files.deleteIfExists(file);
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult postVisitDirectory(Path dir, IOException exception) throws IOException {
                    if (exception != null) {
                        throw exception;
                    }
                    Files.deleteIfExists(dir);
                    return FileVisitResult.CONTINUE;
                }
            });
        } catch (IOException ignored) {
            // Temporary runner cleanup must not hide the test process exit code.
        }
    }

    private static void printHelp() {
        System.out.println("""
                Usage:
                  capy test <java|js|python> --generated-dir <directory> [options]

                Options:
                  --generated-dir <directory>  Generated test sources or classes (required)
                  --output-dir <directory>     Test report output directory (required unless --available-tests)
                  --report-type <type>         JUNIT, CTRF, or JEST (required unless --available-tests)
                  --log <type>                 NONE, LOG, TC, or TEAM_CITY (default: NONE)
                  --tests <selector>           Run matching tests; may be repeated
                  --available-tests            Print available test selectors without running tests
                  -h, --help                   Show this help
                """);
    }

    private enum Backend {
        JAVA("Java"),
        JAVASCRIPT("JavaScript"),
        PYTHON("Python");

        private final String displayName;

        Backend(String displayName) {
            this.displayName = displayName;
        }

        String displayName() {
            return displayName;
        }

        static Backend parse(String value) {
            return switch (value.toLowerCase(Locale.ROOT)) {
                case "java" -> JAVA;
                case "js", "javascript" -> JAVASCRIPT;
                case "py", "python" -> PYTHON;
                default -> throw new IllegalArgumentException("Unknown test backend `%s`. Use java, js, or python.".formatted(value));
            };
        }
    }

    private record Options(
            Backend backend,
            Path generatedDir,
            Path outputDir,
            String reportType,
            String logType,
            List<String> tests,
            boolean availableTests
    ) {
        static Options parse(String[] args) {
            if (args.length == 0) {
                throw new IllegalArgumentException("Missing test backend. Use java, js, or python.");
            }
            var backend = Backend.parse(args[0]);
            Path generatedDir = null;
            Path outputDir = null;
            String reportType = null;
            String logType = "NONE";
            var tests = new ArrayList<String>();
            var availableTests = false;
            for (var index = 1; index < args.length; index++) {
                switch (args[index]) {
                    case "--generated-dir" -> generatedDir = Path.of(nextValue(args, ++index, "--generated-dir"));
                    case "--output-dir" -> outputDir = Path.of(nextValue(args, ++index, "--output-dir"));
                    case "--report-type" -> reportType = nextValue(args, ++index, "--report-type").toUpperCase(Locale.ROOT);
                    case "--log" -> logType = nextValue(args, ++index, "--log").toUpperCase(Locale.ROOT);
                    case "--tests" -> tests.add(nextValue(args, ++index, "--tests"));
                    case "--available-tests" -> availableTests = true;
                    default -> throw new IllegalArgumentException("Unknown test option: " + args[index]);
                }
            }
            if (generatedDir == null) {
                throw new IllegalArgumentException("Missing --generated-dir");
            }
            generatedDir = generatedDir.toAbsolutePath().normalize();
            if (!Files.isDirectory(generatedDir)) {
                throw new IllegalArgumentException("Generated test directory does not exist or is not a directory: " + generatedDir);
            }
            if (!availableTests && outputDir == null) {
                throw new IllegalArgumentException("Missing --output-dir");
            }
            if (!availableTests && reportType == null) {
                throw new IllegalArgumentException("Missing --report-type");
            }
            if (reportType != null && !List.of("JUNIT", "CTRF", "JEST").contains(reportType)) {
                throw new IllegalArgumentException("Unknown report type `%s`. Use JUNIT, CTRF, or JEST.".formatted(reportType));
            }
            if (!List.of("NONE", "LOG", "TC", "TEAM_CITY").contains(logType)) {
                throw new IllegalArgumentException("Unknown log type `%s`. Use NONE, LOG, TC, or TEAM_CITY.".formatted(logType));
            }
            for (var test : tests) {
                if (test.isBlank()) {
                    throw new IllegalArgumentException("Test selector must not be blank");
                }
            }
            outputDir = outputDir == null ? null : outputDir.toAbsolutePath().normalize();
            return new Options(backend, generatedDir, outputDir, reportType, logType, List.copyOf(tests), availableTests);
        }

        List<String> runnerArguments(boolean includeGeneratedDir) {
            var arguments = new ArrayList<String>();
            if (includeGeneratedDir) {
                arguments.add("--generated-dir");
                arguments.add(generatedDir.toString());
            }
            if (outputDir != null) {
                arguments.add("--output-dir");
                arguments.add(outputDir.toString());
            }
            if (reportType != null) {
                arguments.add("--report-type");
                arguments.add(reportType);
            }
            if (!"NONE".equals(logType)) {
                arguments.add("--log");
                arguments.add(logType);
            }
            for (var test : tests) {
                arguments.add("--tests");
                arguments.add(test);
            }
            if (availableTests) {
                arguments.add("--available-tests");
            }
            return arguments;
        }

        private static String nextValue(String[] args, int index, String option) {
            if (index >= args.length) {
                throw new IllegalArgumentException("Missing value for " + option);
            }
            return args[index];
        }
    }

    private static final class CommandException extends Exception {
        private CommandException(String message) {
            super(message);
        }
    }
}
