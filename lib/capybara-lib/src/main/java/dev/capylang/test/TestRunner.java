package dev.capylang.test;

import capy.test.CapyTest;
import capy.test.CapyTest.TestOutput;
import dev.capylang.PathUtil;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.DirectoryNotEmptyException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.logging.ConsoleHandler;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

public class TestRunner {

    public static final String CAPY_TEST_RUNTIME_CLASS = "capy.test.CapyTestRuntime";
    public static final String CAPY_TEST_CLASS = "capy.test.CapyTest";
    public static final String GATHER_TESTS_METHOD_NAME = "gatherTests";
    public static final String RUN_TESTS_METHOD_NAME = "runTests";
    static final String OUTPUT_MANIFEST_FILE = ".capy-test-output-manifest";
    private static final Logger LOG = Logger.getLogger(TestRunner.class.getName());

    public static void main(String[] args) {
        try {
            System.exit(runTests(parseArguments(args)));
        } catch (Exception e) {
            System.err.println(e.getMessage());
            System.exit(2);
        }
    }

    public static int runTests(Arguments arguments) {
        configureLogging(arguments.logLevel());
        LOG.info(() -> "Starting test runner with report type `%s`, output directory `%s`, and log level `%s`"
                .formatted(arguments.reportType(), arguments.outputDir(), arguments.logLevel().getName()));
        var capyTestRuntimeClass = loadCapyTestRuntime();
        var gatherTestsMethod = loadGatherTestsMethod(capyTestRuntimeClass);
        var testFiles = invokeGatherTests(gatherTestsMethod);
        LOG.info(() -> "Collected `%d` test files".formatted(testFiles.size()));
        var testOutputs = invokeRunTests(arguments.reportType(), testFiles);
        LOG.info(() -> "Generated `%d` test outputs".formatted(testOutputs.size()));
        var writtenFiles = new HashSet<Path>();
        testOutputs.forEach(testOutput -> writtenFiles.add(writeTestOutputToFile(testOutput, arguments)));
        deleteStaleOutputs(arguments.outputDir(), writtenFiles);
        return testOutputs.parallelStream()
                .filter(TestOutput::failed)
                .findAny()
                .map(__ -> 1)
                .orElse(0);
    }

    public static Arguments parseArguments(String[] args) {
        Path outputDir = null;
        ReportType reportType = null;
        Level logLevel = Level.INFO;
        for (int i = 0; i < args.length; i++) {
            switch (args[i]) {
                case "-o", "--output-dir" -> {
                    if (i + 1 >= args.length) {
                        throw new IllegalArgumentException("Missing value for " + args[i]);
                    }
                    outputDir = Path.of(args[++i]);
                }
                case "-rt", "--report-type" -> {
                    if (i + 1 >= args.length) {
                        throw new IllegalArgumentException("Missing value for " + args[i]);
                    }
                    reportType = ReportType.valueOf(args[++i].toUpperCase(Locale.ROOT));
                }
                case "-ll", "--log-level" -> {
                    if (i + 1 >= args.length) {
                        throw new IllegalArgumentException("Missing value for " + args[i]);
                    }
                    logLevel = parseLogLevel(args[++i]);
                }
                case "-h", "--help" -> {
                    printHelp();
                    System.exit(0);
                }
                default -> throw new IllegalArgumentException("Unknown argument: " + args[i]);
            }
        }
        return new Arguments(outputDir, reportType, logLevel);
    }

    public record Arguments(Path outputDir, ReportType reportType, Level logLevel) {
        public Arguments {
            if (outputDir == null) {
                throw new IllegalArgumentException("Output directory not specified");
            }
            if (!Files.exists(outputDir)) {
                throw new IllegalArgumentException("Output directory `%s` doesn't exist".formatted(outputDir));
            }
            if (!Files.isDirectory(outputDir)) {
                throw new IllegalArgumentException("Output directory `%s` is not a directory".formatted(outputDir));
            }
            if (reportType == null) {
                throw new IllegalArgumentException("Report type is null");
            }
            if (logLevel == null) {
                throw new IllegalArgumentException("Log level is null");
            }
        }
    }

    public enum ReportType {
        JUNIT
    }

    private static void configureLogging(Level logLevel) {
        var handler = new ConsoleHandler();
        handler.setFormatter(new SimpleFormatter());
        LOG.setUseParentHandlers(false);
        LOG.setLevel(logLevel);
        handler.setLevel(logLevel);
        for (var existingHandler : LOG.getHandlers()) {
            LOG.removeHandler(existingHandler);
            existingHandler.close();
        }
        LOG.addHandler(handler);
    }

    private static Level parseLogLevel(String value) {
        return switch (value.toUpperCase(Locale.ROOT)) {
            case "DEBUG" -> Level.FINE;
            case "INFO" -> Level.INFO;
            case "WARN" -> Level.WARNING;
            case "ERROR" -> Level.SEVERE;
            case "OFF" -> Level.OFF;
            default ->
                    throw new IllegalArgumentException("Unknown log level: " + value + ". Use DEBUG, INFO, WARN, ERROR, or OFF.");
        };
    }

    private static void printHelp() {
        System.out.println("""
                Usage: java -jar test-runner.jar [options]
                Options:
                  -o, --output-dir <dir>    Output directory for test reports (required)
                  -rt, --report-type <type> Report type (required, e.g., JUNIT)
                  -ll, --log-level <level>  Log level: DEBUG, INFO, WARN, ERROR, OFF (optional, default INFO)
                  -h, --help                Show this help message
                """);
    }

    private static Class<?> loadCapyTestRuntime() {
        try {
            return Class.forName(CAPY_TEST_RUNTIME_CLASS, true, contextClassLoader());
        } catch (ClassNotFoundException e) {
            LOG.log(Level.SEVERE, "Cannot load class `%s`".formatted(CAPY_TEST_RUNTIME_CLASS), e);
            throw new IllegalStateException("Cannot load class `%s`".formatted(CAPY_TEST_RUNTIME_CLASS), e);
        }
    }

    private static Method loadGatherTestsMethod(Class<?> capyTestRuntimeClass) {
        try {
            return capyTestRuntimeClass.getMethod(GATHER_TESTS_METHOD_NAME);
        } catch (NoSuchMethodException e) {
            LOG.log(Level.SEVERE, "Class `%s` does not have method `%s()`"
                    .formatted(capyTestRuntimeClass.getCanonicalName(), GATHER_TESTS_METHOD_NAME), e);
            throw new IllegalStateException("Class `%s` does not have method `%s()`".formatted(capyTestRuntimeClass.getCanonicalName(), GATHER_TESTS_METHOD_NAME), e);
        }
    }

    private static List<?> invokeGatherTests(Method gatherTestsMethod) {
        try {
            var result = gatherTestsMethod.invoke(null);
            if (!(result instanceof List<?> list)) {
                var resultType = result == null ? "null" : result.getClass().getCanonicalName();
                LOG.severe(() -> "Method `%s()` returned `%s` instead of `List<TestFile>`"
                        .formatted(GATHER_TESTS_METHOD_NAME, resultType));
                throw new IllegalStateException("Method `%s()` should return `List<TestFile>`, but it returned `%s`"
                        .formatted(GATHER_TESTS_METHOD_NAME, resultType));
            }
            return list;
        } catch (IllegalAccessException e) {
            LOG.log(Level.SEVERE, "Method `%s()` should be public".formatted(GATHER_TESTS_METHOD_NAME), e);
            throw new IllegalStateException("Method `%s()` should be public".formatted(GATHER_TESTS_METHOD_NAME), e);
        } catch (InvocationTargetException e) {
            LOG.log(Level.SEVERE, "Cannot invoke static method `%s()`".formatted(GATHER_TESTS_METHOD_NAME), e);
            throw new IllegalStateException("Cannot invoke static method `%s()`".formatted(GATHER_TESTS_METHOD_NAME), e);
        }
    }

    @SuppressWarnings({"rawtypes", "unchecked"})
    private static List<TestOutput> invokeRunTests(ReportType reportType, List<?> testFiles) {
        return CapyTest.runTests(capy.test.CapyTest.ReportType.valueOf(reportType.name()), (List) testFiles);
    }

    static Path writeTestOutputToFile(TestOutput testOutput, Arguments arguments) {
        try {
            var javaPath = PathUtil.toJavaPath(testOutput.path());
            var finalPath = arguments.outputDir().resolve(javaPath);
            var parent = finalPath.getParent();
            if (parent != null) {
                Files.createDirectories(parent);
            }
            writeStringIfChanged(finalPath, testOutput.content());
            LOG.info(() -> "Wrote test output to `%s`".formatted(finalPath));
            return arguments.outputDir().relativize(finalPath.normalize());
        } catch (IOException e) {
            LOG.log(Level.SEVERE, "Cannot write test output file", e);
            throw new UncheckedIOException("Cannot write test output file", e);
        }
    }

    static void deleteStaleOutputs(Path outputDir, Set<Path> expectedFiles) {
        try {
            if (Files.notExists(outputDir)) {
                return;
            }
            var manifestFile = outputDir.resolve(OUTPUT_MANIFEST_FILE);
            for (var staleFile : staleFiles(outputDir, manifestFile, expectedFiles)) {
                Files.deleteIfExists(staleFile);
                LOG.info(() -> "Deleted stale test output `%s`".formatted(staleFile));
                deleteEmptyParentDirectories(staleFile.getParent(), outputDir);
            }
            writeOutputManifest(manifestFile, expectedFiles);
        } catch (IOException e) {
            LOG.log(Level.SEVERE, "Cannot prune stale test output files", e);
            throw new UncheckedIOException("Cannot prune stale test output files", e);
        }
    }

    private static List<Path> staleFiles(Path outputDir, Path manifestFile, Set<Path> expectedFiles) throws IOException {
        if (Files.exists(manifestFile)) {
            try (var lines = Files.lines(manifestFile, StandardCharsets.UTF_8)) {
                return lines
                        .map(String::trim)
                        .filter(line -> !line.isEmpty())
                        .map(Path::of)
                        .map(Path::normalize)
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
                    .filter(path -> !expectedFiles.contains(outputDir.relativize(path).normalize()))
                    .sorted(Comparator.reverseOrder())
                    .toList();
        }
    }

    private static void deleteEmptyParentDirectories(Path directory, Path outputDir) {
        var current = directory;
        while (current != null && !current.equals(outputDir)) {
            try {
                Files.deleteIfExists(current);
                LOG.info(() -> "Deleted empty test output directory `%s`".formatted(current));
            } catch (DirectoryNotEmptyException ignored) {
                return;
            } catch (IOException e) {
                throw new UncheckedIOException(e);
            }
            current = current.getParent();
        }
    }

    private static void writeOutputManifest(Path manifestFile, Set<Path> expectedFiles) throws IOException {
        var manifestContents = expectedFiles.stream()
                .map(Path::normalize)
                .map(Path::toString)
                .sorted()
                .collect(java.util.stream.Collectors.joining(System.lineSeparator()));
        Files.writeString(
                manifestFile,
                manifestContents.isEmpty() ? "" : manifestContents + System.lineSeparator(),
                StandardCharsets.UTF_8
        );
    }

    private static ClassLoader contextClassLoader() {
        var classLoader = Thread.currentThread().getContextClassLoader();
        return classLoader == null ? TestRunner.class.getClassLoader() : classLoader;
    }

    private static void writeStringIfChanged(Path outputFile, String content) throws IOException {
        if (Files.isRegularFile(outputFile) && content.equals(Files.readString(outputFile))) {
            return;
        }
        Files.writeString(outputFile, content);
    }
}
