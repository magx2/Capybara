package dev.capylang.test;

import capy.test.CapyTest;
import capy.test.CapyTest.TestOutput;
import dev.capylang.PathUtil;

import java.io.IOException;
import java.io.PrintStream;
import java.io.UncheckedIOException;
import java.io.StringReader;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.DirectoryNotEmptyException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.logging.ConsoleHandler;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;
import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilderFactory;
import org.xml.sax.InputSource;

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
        var hasFailures = false;
        for (var testOutput : testOutputs) {
            writtenFiles.add(writeTestOutputToFile(testOutput, arguments));
            hasFailures |= testOutput.failed();
        }
        printFailureSummary(testOutputs, System.out);
        deleteStaleOutputs(arguments.outputDir(), writtenFiles);
        return hasFailures ? 1 : 0;
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
            LOG.fine(() -> "Wrote test output to `%s`".formatted(finalPath));
            return normalizeRelativePath(arguments.outputDir().relativize(finalPath.normalize()));
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
                var resolvedStaleFile = outputDir.resolve(staleFile).normalize();
                deleteStaleFile(resolvedStaleFile);
                deleteEmptyParentDirectories(resolvedStaleFile.getParent(), outputDir);
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
                        .map(TestRunner::normalizeRelativePath)
                        .filter(path -> !expectedFiles.contains(path))
                        .toList();
            }
        }

        try (var paths = Files.walk(outputDir)) {
            return paths
                    .filter(path -> !path.equals(outputDir))
                    .filter(Files::isRegularFile)
                    .filter(path -> !path.equals(manifestFile))
                    .map(outputDir::relativize)
                    .map(TestRunner::normalizeRelativePath)
                    .filter(path -> !expectedFiles.contains(path))
                    .toList();
        }
    }

    private static void deleteStaleFile(Path path) {
        try {
            Files.deleteIfExists(path);
            LOG.fine(() -> "Deleted stale test output `%s`".formatted(path));
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    private static void deleteEmptyParentDirectories(Path directory, Path outputDir) {
        var current = directory;
        while (current != null && !current.equals(outputDir)) {
            try {
                Files.deleteIfExists(current);
                var deletedDirectory = current;
                LOG.fine(() -> "Deleted empty test output directory `%s`".formatted(deletedDirectory));
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
                .map(TestRunner::normalizeRelativePath)
                .map(TestRunner::normalizeRelativePathString)
                .sorted()
                .collect(java.util.stream.Collectors.joining(System.lineSeparator()));
        writeStringIfChanged(
                manifestFile,
                manifestContents.isEmpty() ? "" : manifestContents + System.lineSeparator()
        );
    }

    private static ClassLoader contextClassLoader() {
        var classLoader = Thread.currentThread().getContextClassLoader();
        return classLoader == null ? TestRunner.class.getClassLoader() : classLoader;
    }

    private static Path normalizeRelativePath(Path path) {
        return Path.of(normalizeRelativePathString(path));
    }

    private static String normalizeRelativePathString(Path path) {
        return path.normalize().toString().replace('\\', '/');
    }

    private static void writeStringIfChanged(Path outputFile, String content) throws IOException {
        var contentBytes = content.getBytes(StandardCharsets.UTF_8);
        if (Files.isRegularFile(outputFile) && Files.size(outputFile) == contentBytes.length) {
            if (java.util.Arrays.equals(Files.readAllBytes(outputFile), contentBytes)) {
                return;
            }
        }
        Files.write(outputFile, contentBytes);
    }

    static void printFailureSummary(List<TestOutput> testOutputs, PrintStream output) {
        var failures = testOutputs.stream()
                .filter(TestOutput::failed)
                .toList();
        if (failures.isEmpty()) {
            return;
        }

        output.println();
        output.println("Failures:");
        output.println();
        for (var testOutput : failures) {
            printTestOutputFailures(testOutput, output);
        }
    }

    private static void printTestOutputFailures(TestOutput testOutput, PrintStream output) {
        try {
            var documentBuilderFactory = DocumentBuilderFactory.newInstance();
            documentBuilderFactory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);
            documentBuilderFactory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "");
            documentBuilderFactory.setAttribute(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "");
            var documentBuilder = documentBuilderFactory.newDocumentBuilder();
            var document = documentBuilder.parse(new InputSource(new StringReader(testOutput.content())));
            var testCases = document.getElementsByTagName("testcase");
            for (int i = 0; i < testCases.getLength(); i++) {
                var testCase = testCases.item(i);
                var failureNodes = testCase.getChildNodes();
                for (int j = 0; j < failureNodes.getLength(); j++) {
                    var failureNode = failureNodes.item(j);
                    if (!"failure".equals(failureNode.getNodeName())) {
                        continue;
                    }
                    var attributes = testCase.getAttributes();
                    var className = attributes.getNamedItem("classname").getNodeValue();
                    var testName = attributes.getNamedItem("name").getNodeValue();
                    var failureMessage = failureNode.getTextContent();
                    output.printf("  %s > %s()%n", className, testName);
                    output.println(failureMessage);
                    output.println();
                }
            }
        } catch (Exception e) {
            LOG.log(Level.WARNING, "Cannot print failure summary for `%s`".formatted(testOutput.path()), e);
            output.printf("  %s%n", testOutput.path());
            output.println(testOutput.content());
            output.println();
        }
    }
}
