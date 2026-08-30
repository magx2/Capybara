package dev.capylang.test;

import capy.test.CapyTest;
import dev.capylang.PathUtil;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.function.Supplier;

public class TestRunner {

    public static final String CAPY_TEST_RUNTIME_CLASS = "capy.test.CapyTestRuntime";
    public static final String CAPY_TEST_CLASS = "capy.test.CapyTest";
    public static final String GATHER_TESTS_METHOD_NAME = "gatherTests";

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
        if (arguments.availableTests()) {
            TestSelection.availableTests(testFiles).forEach(System.out::println);
            return 0;
        }
        testFiles = TestSelection.filterTestFiles(testFiles, arguments.testSelectors());
        var testRun = invokeRunTests(arguments.reportType(), arguments.outputDir(), arguments.logType(), testFiles);
        return failed(testRun) ? 1 : 0;
    }

    public static Arguments parseArguments(String[] args) {
        Path outputDir = null;
        ReportType reportType = null;
        LogType logType = LogType.NONE;
        var testSelectors = new ArrayList<String>();
        var availableTests = false;
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
                case "-l", "--log" -> {
                    if (i + 1 >= args.length) {
                        throw new IllegalArgumentException("Missing value for " + args[i]);
                    }
                    logType = parseLogType(args[++i]);
                }
                case "--tests" -> {
                    if (i + 1 >= args.length) {
                        throw new IllegalArgumentException("Missing value for " + args[i]);
                    }
                    testSelectors.add(args[++i]);
                }
                case "--available-tests" -> availableTests = true;
                case "-h", "--help" -> {
                    printHelp();
                    System.exit(0);
                }
                default -> throw new IllegalArgumentException("Unknown argument: " + args[i]);
            }
        }
        return new Arguments(outputDir, reportType, logType, testSelectors, availableTests);
    }

    public record Arguments(
            Path outputDir,
            ReportType reportType,
            LogType logType,
            List<String> testSelectors,
            boolean availableTests
    ) {
        public Arguments(Path outputDir, ReportType reportType) {
            this(outputDir, reportType, LogType.NONE, List.of(), false);
        }

        public Arguments(Path outputDir, ReportType reportType, LogType logType) {
            this(outputDir, reportType, logType, List.of(), false);
        }

        public Arguments {
            if (!availableTests && outputDir == null) {
                throw new IllegalArgumentException("Output directory not specified");
            }
            if (outputDir != null && !Files.exists(outputDir)) {
                throw new IllegalArgumentException("Output directory `%s` doesn't exist".formatted(outputDir));
            }
            if (outputDir != null && !Files.isDirectory(outputDir)) {
                throw new IllegalArgumentException("Output directory `%s` is not a directory".formatted(outputDir));
            }
            if (!availableTests && reportType == null) {
                throw new IllegalArgumentException("Report type is null");
            }
            if (logType == null) {
                logType = LogType.NONE;
            }
            if (testSelectors == null) {
                testSelectors = List.of();
            } else {
                testSelectors = List.copyOf(testSelectors);
            }
            for (var testSelector : testSelectors) {
                if (testSelector == null || testSelector.isBlank()) {
                    throw new IllegalArgumentException("Test selector must not be blank");
                }
            }
        }
    }

    public enum ReportType {
        JUNIT,
        CTRF,
        JEST,
        ADOC
    }

    public enum LogType {
        NONE,
        LOG,
        TEAM_CITY
    }

    private static void printHelp() {
        System.out.println("""
                Usage: java -jar test-runner.jar [options]
                Options:
                  -o, --output-dir <dir>    Output directory for test reports (required)
                  -rt, --report-type <type> Report type (required, JUNIT, CTRF, JEST, ADOC)
                  -l, --log <type>          Log output type (optional, LOG, TC, TEAM_CITY)
                  --tests <selector>        Run only tests matching selector; can be repeated
                  --available-tests         Print available test selectors and exit
                  -h, --help                Show this help message
                """);
    }

    private static LogType parseLogType(String value) {
        return switch (value.toUpperCase(Locale.ROOT)) {
            case "NONE" -> LogType.NONE;
            case "LOG" -> LogType.LOG;
            case "TC", "TEAM_CITY" -> LogType.TEAM_CITY;
            default -> throw new IllegalArgumentException("Unknown log type `" + value + "`. Use NONE, LOG, TC, or TEAM_CITY.");
        };
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

    private static List<Object> invokeGatherTests(Method gatherTestsMethod) {
        try {
            var result = gatherTestsMethod.invoke(null);
            var root = unsafeRunEffect(result);
            if (!(root instanceof List<?> rootList)) {
                var resultType = result == null ? "null" : result.getClass().getCanonicalName();
                throw new IllegalStateException("Method `%s()` should return `List<TestFile>` or `Effect[List<TestFile]]`, but it returned `%s`"
                        .formatted(GATHER_TESTS_METHOD_NAME, resultType));
            }
            return flattenTestValues(rootList).stream()
                    .map(TestRunner::asTestFile)
                    .toList();
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

    private static Object asTestFile(Object value) {
        if (value instanceof CapyTest.TestFile) {
            return value;
        }
        if (value instanceof Map<?, ?> map && "TestFile".equals(map.get("__type"))) {
            return value;
        }
        var valueType = value == null ? "null" : value.getClass().getCanonicalName();
        throw new IllegalStateException("Method `%s()` should return `TestFile` values, but it returned `%s`"
                .formatted(GATHER_TESTS_METHOD_NAME, valueType));
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

    private static Object invokeRunTests(
            ReportType reportType,
            Path outputDir,
            LogType logType,
            List<Object> testFiles
    ) {
        if (reportType == ReportType.ADOC && !supportsAdocReportType()) {
            return invokeAdocRunTests(outputDir, logType, testFiles);
        }
        var result = invokeGeneratedCapyTest(
                "run_tests_and_print_summary",
                generatedConstant(reportType.name()),
                PathUtil.fromJavaPath(outputDir),
                generatedConstant(logType.name()),
                testFiles
        );
        return unwrapResult(unsafeRunEffect(result), "Cannot run Capybara tests");
    }

    private static Object invokeGeneratedCapyTest(String namePrefix, Object... arguments) {
        var method = findGeneratedMethod(CapyTest.class, namePrefix, arguments.length);
        try {
            return method.invoke(null, arguments);
        } catch (IllegalAccessException e) {
            throw new IllegalStateException("Generated method `%s` should be public".formatted(method.getName()), e);
        } catch (InvocationTargetException e) {
            throw new IllegalStateException("Cannot invoke generated method `%s`".formatted(method.getName()), e);
        }
    }

    private static Method findGeneratedMethod(Class<?> type, String namePrefix, int parameterCount) {
        for (var method : type.getMethods()) {
            if (method.getName().startsWith(namePrefix) && method.getParameterCount() == parameterCount) {
                return method;
            }
        }
        var camelCasePrefix = snakeToLowerCamel(namePrefix);
        for (var method : type.getMethods()) {
            if (method.getName().startsWith(camelCasePrefix) && method.getParameterCount() == parameterCount) {
                return method;
            }
        }
        throw new IllegalStateException("Cannot find generated method `%s` with %d parameters"
                .formatted(namePrefix, parameterCount));
    }

    private static String snakeToLowerCamel(String value) {
        var result = new StringBuilder();
        var uppercaseNext = false;
        for (var character : value.toCharArray()) {
            if (character == '_') {
                uppercaseNext = true;
            } else if (uppercaseNext) {
                result.append(Character.toUpperCase(character));
                uppercaseNext = false;
            } else {
                result.append(character);
            }
        }
        return result.toString();
    }

    @SuppressWarnings({"rawtypes", "unchecked"})
    private static Object generatedConstant(String name) {
        try {
            return CapyTest.class.getField(name).get(null);
        } catch (ReflectiveOperationException ignored) {
            for (var nestedType : CapyTest.class.getClasses()) {
                if (nestedType.isEnum()) {
                    try {
                        return Enum.valueOf((Class<? extends Enum>) nestedType, name);
                    } catch (IllegalArgumentException ignoredConstant) {
                        // The constant belongs to a different generated enum.
                    }
                }
            }
            throw new IllegalStateException("Cannot read generated CapyTest constant `%s`".formatted(name));
        }
    }

    private static Object invokeAdocRunTests(Path outputDir, LogType logType, List<Object> testFiles) {
        var executedTestFiles = executeTestFiles(testFiles);
        printTestLog(logType, executedTestFiles);

        var writtenFiles = writeAdocReports(outputDir, executedTestFiles);
        var summary = failureSummary(executedTestFiles);
        if (!summary.isEmpty()) {
            System.out.print(summary);
        }

        var failed = executedTestFiles.stream().anyMatch(TestRunner::testFileFailed);
        var testRun = new LinkedHashMap<String, Object>();
        testRun.put("__type", "TestRun");
        testRun.put("outputs", List.of());
        testRun.put("written_files", writtenFiles);
        testRun.put("failure_summary", summary);
        testRun.put("failed", failed);
        return testRun;
    }

    private static List<Object> executeTestFiles(List<Object> testFiles) {
        return testFiles.stream().map(testFile -> {
            var executedCases = listField(testFile, "test_cases").stream()
                    .map(TestRunner::executeTestCase)
                    .toList();
            return (Object) dataValue("TestFile", Map.of(
                    "file_name", stringField(testFile, "file_name"),
                    "test_cases", executedCases,
                    "timestamp_millis", field(testFile, "timestamp_millis")
            ));
        }).toList();
    }

    private static Object executeTestCase(Object testCase) {
        var start = System.nanoTime();
        var assertionContainer = ((Supplier<?>) field(testCase, "assert_supplier")).get();
        var assertionSuppliers = listField(assertionContainer, "assertions");
        Object failedAssertion = null;
        for (var value : assertionSuppliers) {
            var assertion = ((Supplier<?>) value).get();
            if (!Boolean.TRUE.equals(field(assertion, "result"))) {
                failedAssertion = assertion;
                break;
            }
        }
        var result = failedAssertion == null
                ? dataValue("Passed", Map.of())
                : dataValue("Failed", Map.of(
                        "message", stringField(failedAssertion, "message"),
                        "type", stringField(failedAssertion, "type")
                ));
        return dataValue("TestCase", Map.of(
                "name", stringField(testCase, "name"),
                "result", result,
                "assertions_count", assertionSuppliers.size(),
                "execution_time", (System.nanoTime() - start) / 1_000_000_000.0,
                "assert_supplier", field(testCase, "assert_supplier")
        ));
    }

    private static Map<String, Object> dataValue(String type, Map<String, ?> fields) {
        var value = new LinkedHashMap<String, Object>();
        value.put("__type", type);
        value.putAll(fields);
        return value;
    }

    private static void printTestLog(LogType logType, List<Object> executedTestFiles) {
        if (logType == LogType.NONE) {
            return;
        }
        for (var testFile : executedTestFiles) {
            var suiteName = stringField(testFile, "file_name");
            System.out.println(logType == LogType.LOG
                    ? "Test suite started: " + suiteName
                    : "##teamcity[testSuiteStarted name='" + teamCityEscape(suiteName) + "']");
            for (var testCase : listField(testFile, "test_cases")) {
                var testName = stringField(testCase, "name");
                System.out.println(logType == LogType.LOG
                        ? "Test started: " + testName
                        : "##teamcity[testStarted name='" + teamCityEscape(testName) + "']");
                if (testCaseFailed(testCase)) {
                    var message = normalizeFailureMessage(stringField(field(testCase, "result"), "message"));
                    if (logType == LogType.LOG) {
                        System.out.println("Test failed: " + testName);
                        System.out.println(message);
                    } else {
                        System.out.println("##teamcity[testFailed name='" + teamCityEscape(testName)
                                + "' message='assertion failed' details='" + teamCityEscape(message) + "']");
                    }
                }
                System.out.println(logType == LogType.LOG
                        ? "Test finished: " + testName
                        : "##teamcity[testFinished name='" + teamCityEscape(testName) + "']");
            }
            System.out.println(logType == LogType.LOG
                    ? "Test suite finished: " + suiteName
                    : "##teamcity[testSuiteFinished name='" + teamCityEscape(suiteName) + "']");
        }
    }

    private static String failureSummary(List<Object> testFiles) {
        var failures = new StringBuilder();
        for (var testFile : testFiles) {
            for (var testCase : listField(testFile, "test_cases")) {
                if (testCaseFailed(testCase)) {
                    failures.append("  ").append(stringField(testFile, "file_name"))
                            .append(" > ").append(stringField(testCase, "name")).append("()\n")
                            .append(normalizeFailureMessage(stringField(field(testCase, "result"), "message")))
                            .append("\n\n");
                }
            }
        }
        return failures.isEmpty() ? "" : "\nFailures:\n\n" + failures;
    }

    private static String teamCityEscape(String value) {
        return value.replace("|", "||")
                .replace("'", "|'")
                .replace("\n", "|n")
                .replace("\r", "|r")
                .replace("[", "|[")
                .replace("]", "|]");
    }

    private static List<Path> writeAdocReports(Path outputDir, List<?> testFiles) {
        var writtenFiles = new ArrayList<Path>();
        for (var testFile : testFiles) {
            var relativePath = Path.of("TEST-" + adocFileName(stringField(testFile, "file_name")) + ".adoc");
            writeTextIfChanged(outputDir.resolve(relativePath), adocReport(testFile));
            writtenFiles.add(relativePath);
        }
        updateOutputManifest(outputDir, writtenFiles);
        return List.copyOf(writtenFiles);
    }

    private static void writeTextIfChanged(Path path, String content) {
        try {
            Files.createDirectories(path.getParent());
            if (!Files.exists(path) || !Files.readString(path).equals(content)) {
                Files.writeString(path, content, StandardCharsets.UTF_8);
            }
        } catch (java.io.IOException e) {
            throw new IllegalStateException("Cannot write Capybara test report `%s`".formatted(path), e);
        }
    }

    private static void updateOutputManifest(Path outputDir, List<Path> writtenFiles) {
        var manifest = outputDir.resolve(".capy-test-output-manifest");
        var normalizedOutputDir = outputDir.toAbsolutePath().normalize();
        var current = writtenFiles.stream().map(Path::toString).toList();
        try {
            if (Files.exists(manifest)) {
                for (var stale : Files.readAllLines(manifest)) {
                    if (stale.isBlank() || current.contains(stale)) {
                        continue;
                    }
                    var stalePath = normalizedOutputDir.resolve(stale).normalize();
                    if (stalePath.startsWith(normalizedOutputDir)) {
                        Files.deleteIfExists(stalePath);
                    }
                }
            }
            writeTextIfChanged(manifest, current.isEmpty() ? "" : String.join("\n", current) + "\n");
        } catch (java.io.IOException e) {
            throw new IllegalStateException("Cannot update Capybara test output manifest `%s`".formatted(manifest), e);
        }
    }

    static String adocReport(Object testFile) {
        var testCases = listField(testFile, "test_cases");
        var failures = (int) testCases.stream().filter(TestRunner::testCaseFailed).count();
        var rows = new StringBuilder();
        for (var testCase : testCases) {
            rows.append('|').append(adocTableCell(stringField(testCase, "name"))).append('\n')
                    .append('|').append(testCaseFailed(testCase) ? "FAIL" : "PASS").append('\n')
                    .append('|').append(field(testCase, "assertions_count")).append('\n')
                    .append('|').append(field(testCase, "execution_time")).append("\n\n");
        }

        var report = new StringBuilder()
                .append("= Test results: ").append(adocInline(stringField(testFile, "file_name"))).append("\n\n")
                .append("[cols=\"3,1,1,1\",options=\"header\"]\n")
                .append("|===\n")
                .append("|Test |Status |Assertions |Time (seconds)\n\n")
                .append(rows)
                .append("|===\n\n")
                .append("*Tests:* ").append(testCases.size()).append(" +\n")
                .append("*Passed:* ").append(testCases.size() - failures).append(" +\n")
                .append("*Failed:* ").append(failures).append('\n');

        var failureDetails = new StringBuilder();
        for (var testCase : testCases) {
            if (!testCaseFailed(testCase)) {
                continue;
            }
            var result = field(testCase, "result");
            failureDetails.append("=== ").append(adocInline(stringField(testCase, "name"))).append("\n\n")
                    .append("*Type:* `").append(adocInline(stringField(result, "type"))).append("`\n\n")
                    .append("[listing]\n....\n")
                    .append(adocListing(normalizeFailureMessage(stringField(result, "message"))))
                    .append("\n....\n\n");
        }
        if (!failureDetails.isEmpty()) {
            report.append("\n== Failures\n\n").append(failureDetails);
        }
        return report.toString();
    }

    private static String adocFileName(String fileName) {
        var withoutRoot = fileName.startsWith("/") ? fileName.substring(1) : fileName;
        var flattened = withoutRoot.replace('/', '.').replace('\\', '.');
        if (flattened.endsWith(".cfun")) {
            return flattened.substring(0, flattened.length() - 5);
        }
        if (flattened.endsWith(".coo")) {
            return flattened.substring(0, flattened.length() - 4);
        }
        return flattened;
    }

    private static String adocInline(String value) {
        return value.replace("\\", "\\\\")
                .replace("\r\n", " ")
                .replace('\n', ' ')
                .replace('\r', ' ')
                .replace("{", "\\{")
                .replace("}", "\\}");
    }

    private static String adocTableCell(String value) {
        return adocInline(value).replace("|", "\\|");
    }

    private static String adocListing(String value) {
        var escaped = value.replace("\n....", "\n\\....");
        return escaped.startsWith("....") ? "\\" + escaped : escaped;
    }

    private static String normalizeFailureMessage(String message) {
        return message.replace("\\r\\n", "\n")
                .replace("\\n", "\n")
                .replace("\\r", "\n")
                .replace("\r\n", "\n")
                .replace('\r', '\n')
                .replace("\\t", "\t");
    }

    private static boolean testFileFailed(Object testFile) {
        return listField(testFile, "test_cases").stream().anyMatch(TestRunner::testCaseFailed);
    }

    private static boolean testCaseFailed(Object testCase) {
        return "Failed".equals(field(field(testCase, "result"), "__type"));
    }

    @SuppressWarnings("unchecked")
    private static List<Object> listField(Object value, String name) {
        return (List<Object>) field(value, name);
    }

    private static String stringField(Object value, String name) {
        return String.valueOf(field(value, name));
    }

    @SuppressWarnings("unchecked")
    private static Object field(Object value, String name) {
        if (value instanceof Map<?, ?> map) {
            return ((Map<String, Object>) map).get(name);
        }
        try {
            return value.getClass().getMethod(name).invoke(value);
        } catch (ReflectiveOperationException e) {
            throw new IllegalStateException("Cannot read generated field `%s` from `%s`"
                    .formatted(name, value == null ? "null" : value.getClass().getCanonicalName()), e);
        }
    }

    private static boolean supportsAdocReportType() {
        try {
            CapyTest.class.getField("ADOC");
            return true;
        } catch (NoSuchFieldException e) {
            for (var nestedType : CapyTest.class.getClasses()) {
                if (!nestedType.isEnum()) {
                    continue;
                }
                for (var constant : nestedType.getEnumConstants()) {
                    if ("ADOC".equals(((Enum<?>) constant).name())) {
                        return true;
                    }
                }
            }
            return false;
        }
    }

    private static ClassLoader contextClassLoader() {
        var classLoader = Thread.currentThread().getContextClassLoader();
        return classLoader == null ? TestRunner.class.getClassLoader() : classLoader;
    }

    @SuppressWarnings("unchecked")
    private static Object unwrapResult(Object result, String message) {
        if (result instanceof Map<?, ?> map && "Success".equals(map.get("__type"))) {
            return ((Map<String, Object>) map).get("value");
        }
        if (result instanceof Map<?, ?> map && "Error".equals(map.get("__type"))) {
            var rawDetail = map.get("message");
            var detail = String.valueOf(rawDetail == null ? "unknown error" : rawDetail);
            throw new IllegalStateException(message + ": " + detail);
        }
        if (result != null && "Success".equals(result.getClass().getSimpleName())) {
            return field(result, "value");
        }
        if (result != null && "Error".equals(result.getClass().getSimpleName())) {
            var error = field(result, "ex");
            if (error instanceof Throwable throwable) {
                throw new IllegalStateException(message + ": " + throwable.getMessage(), throwable);
            }
            throw new IllegalStateException(message + ": " + error);
        }
        throw new IllegalStateException(message + ": unknown result type `%s`".formatted(result == null ? "null" : result.getClass().getCanonicalName()));
    }

    private static boolean failed(Object testRun) {
        return Boolean.TRUE.equals(field(testRun, "failed"));
    }
}
