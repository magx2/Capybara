package dev.capylang.test;

import capy.test.CapyTest;
import capy.lang.Result;
import capy.test.CapyTest.TestOutput;
import capy.test.CapyTest.TestRun;
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
import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilderFactory;
import org.xml.sax.InputSource;

public class TestRunner {

    public static final String CAPY_TEST_RUNTIME_CLASS = "capy.test.CapyTestRuntime";
    public static final String CAPY_TEST_CLASS = "capy.test.CapyTest";
    public static final String GATHER_TESTS_METHOD_NAME = "gatherTests";
    public static final String RUN_TESTS_METHOD_NAME = "runTests";
    static final String OUTPUT_MANIFEST_FILE = ".capy-test-output-manifest";

    public static void main(String[] args) {
        try {
            System.exit(runTests(parseArguments(args)));
        } catch (Exception e) {
            System.err.println(e.getMessage());
            System.exit(2);
        }
    }

    public static int runTests(Arguments arguments) {
        var capyTestRuntimeClass = loadCapyTestRuntime();
        var gatherTestsMethod = loadGatherTestsMethod(capyTestRuntimeClass);
        var testFiles = invokeGatherTests(gatherTestsMethod);
        var testRun = invokeRunTests(arguments.reportType(), arguments.outputDir(), testFiles);
        var writtenFiles = toJavaRelativePathSet(testRun.written_files());
        printFailureSummary(testRun.outputs(), System.out);
        deleteStaleOutputs(arguments.outputDir(), writtenFiles);
        return testRun.failed() ? 1 : 0;
    }

    public static Arguments parseArguments(String[] args) {
        Path outputDir = null;
        ReportType reportType = null;
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
                case "-h", "--help" -> {
                    printHelp();
                    System.exit(0);
                }
                default -> throw new IllegalArgumentException("Unknown argument: " + args[i]);
            }
        }
        return new Arguments(outputDir, reportType);
    }

    public record Arguments(Path outputDir, ReportType reportType) {
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
        }
    }

    public enum ReportType {
        JUNIT
    }

    private static void printHelp() {
        System.out.println("""
                Usage: java -jar test-runner.jar [options]
                Options:
                  -o, --output-dir <dir>    Output directory for test reports (required)
                  -rt, --report-type <type> Report type (required, e.g., JUNIT)
                  -h, --help                Show this help message
                """);
    }

    private static Class<?> loadCapyTestRuntime() {
        try {
            return Class.forName(CAPY_TEST_RUNTIME_CLASS, true, contextClassLoader());
        } catch (ClassNotFoundException e) {
            throw new IllegalStateException("Cannot load class `%s`".formatted(CAPY_TEST_RUNTIME_CLASS), e);
        }
    }

    private static Method loadGatherTestsMethod(Class<?> capyTestRuntimeClass) {
        try {
            return capyTestRuntimeClass.getMethod(GATHER_TESTS_METHOD_NAME);
        } catch (NoSuchMethodException e) {
            throw new IllegalStateException("Class `%s` does not have method `%s()`".formatted(capyTestRuntimeClass.getCanonicalName(), GATHER_TESTS_METHOD_NAME), e);
        }
    }

    private static List<?> invokeGatherTests(Method gatherTestsMethod) {
        try {
            var result = gatherTestsMethod.invoke(null);
            var root = unsafeRunEffect(result);
            if (!(root instanceof List<?> rootList)) {
                var resultType = result == null ? "null" : result.getClass().getCanonicalName();
                throw new IllegalStateException("Method `%s()` should return `List<TestFile>` or `Effect[List<TestFile]]`, but it returned `%s`"
                        .formatted(GATHER_TESTS_METHOD_NAME, resultType));
            }
            return flattenTestValues(rootList);
        } catch (IllegalAccessException e) {
            throw new IllegalStateException("Method `%s()` should be public".formatted(GATHER_TESTS_METHOD_NAME), e);
        } catch (InvocationTargetException e) {
            throw new IllegalStateException("Cannot invoke static method `%s()`".formatted(GATHER_TESTS_METHOD_NAME), e);
        }
    }

    private static List<?> flattenTestValues(Object value) {
        value = unsafeRunEffect(value);
        if (value == null) {
            return List.of();
        }
        if (value instanceof List<?> list) {
            return list.stream()
                    .flatMap(item -> flattenTestValues(item).stream())
                    .toList();
        }
        return List.of(value);
    }

    private static Object unsafeRunEffect(Object value) {
        if (value == null || !isEffectClass(value.getClass())) {
            return value;
        }
        try {
            return value.getClass().getMethod("unsafeRun").invoke(value);
        } catch (ReflectiveOperationException e) {
            throw new IllegalStateException("Unable to run Capybara Effect returned by test function", e);
        }
    }

    private static boolean isEffectClass(Class<?> type) {
        if (type == null) {
            return false;
        }
        if ("capy.lang.Effect".equals(type.getCanonicalName()) || "Effect".equals(type.getSimpleName())) {
            return true;
        }
        for (var interfaceType : type.getInterfaces()) {
            if (isEffectClass(interfaceType)) {
                return true;
            }
        }
        return isEffectClass(type.getSuperclass());
    }

    @SuppressWarnings({"rawtypes", "unchecked"})
    private static TestRun invokeRunTests(ReportType reportType, Path outputDir, List<?> testFiles) {
        var result = (Result<TestRun>) CapyTest.runTests(
                capy.test.CapyTest.ReportType.valueOf(reportType.name()),
                PathUtil.fromJavaPath(outputDir),
                (List) testFiles
        ).unsafeRun();
        return unwrapResult(result, "Cannot run Capybara tests");
    }

    static Path writeTestOutputToFile(TestOutput testOutput, Arguments arguments) {
        var writtenFiles = writeTestOutputs(arguments.outputDir(), List.of(testOutput));
        if (writtenFiles.size() != 1) {
            throw new IllegalStateException("Expected to write one test output file, but wrote `%d`".formatted(writtenFiles.size()));
        }
        return writtenFiles.getFirst();
    }

    private static List<Path> writeTestOutputs(Path outputDir, List<TestOutput> testOutputs) {
        var result = CapyTest.writeTestOutputs(PathUtil.fromJavaPath(outputDir), testOutputs).unsafeRun();
        var writtenFiles = unwrapResult(result, "Cannot write test output file");
        return toJavaRelativePaths(writtenFiles);
    }

    private static Set<Path> toJavaRelativePathSet(List<capy.io.Path> paths) {
        return new HashSet<>(toJavaRelativePaths(paths));
    }

    private static List<Path> toJavaRelativePaths(List<capy.io.Path> paths) {
        return paths.stream()
                .map(PathUtil::toJavaPath)
                .map(TestRunner::normalizeRelativePath)
                .toList();
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
        } catch (IOException e) {
            throw new UncheckedIOException(e);
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
                throw new UncheckedIOException(e);
            }
            current = current.getParent();
        }
    }

    private static void writeOutputManifest(Path manifestFile, Set<Path> expectedFiles) {
        var manifestContents = expectedFiles.stream()
                .map(TestRunner::normalizeRelativePath)
                .map(TestRunner::normalizeRelativePathString)
                .sorted()
                .collect(java.util.stream.Collectors.joining(System.lineSeparator()));
        var result = CapyTest.writeTextIfChanged(
                PathUtil.fromJavaPath(manifestFile),
                manifestContents.isEmpty() ? "" : manifestContents + System.lineSeparator()
        ).unsafeRun();
        unwrapResult(result, "Cannot write test output manifest");
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

    @SuppressWarnings("unchecked")
    private static <T> T unwrapResult(Result<T> result, String message) {
        if (result instanceof Result.Success<?> success) {
            return (T) success.value();
        }
        if (result instanceof Result.Error<?> error) {
            var cause = error.ex();
            var detail = cause == null ? "unknown error" : cause.getMessage();
            throw new IllegalStateException(message + ": " + detail, cause);
        }
        throw new IllegalStateException(message + ": unknown result type `%s`".formatted(result == null ? "null" : result.getClass().getCanonicalName()));
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
                    var failureMessage = normalizeFailureMessage(failureNode.getTextContent());
                    output.printf("  %s > %s()%n", className, testName);
                    output.println(failureMessage);
                    output.println();
                }
            }
        } catch (Exception e) {
            output.printf("  %s%n", testOutput.path());
            output.println(testOutput.content());
            output.println();
        }
    }

    static String normalizeFailureMessage(String message) {
        return normalizeFailureMessage(message, System.lineSeparator());
    }

    static String normalizeFailureMessage(String message, String lineSeparator) {
        return message
                .replace("\\r\\n", "\n")
                .replace("\\n", "\n")
                .replace("\\r", "\n")
                .replace("\r\n", "\n")
                .replace("\r", "\n")
                .replace("\n", lineSeparator)
                .replace("\\t", "\t");
    }
}
