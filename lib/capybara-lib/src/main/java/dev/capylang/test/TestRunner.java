package dev.capylang.test;

import capy.test.CapyTest;
import dev.capylang.PathUtil;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

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
        JEST
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
                  -rt, --report-type <type> Report type (required, JUNIT, CTRF, JEST)
                  -l, --log <type>          Log output type (optional, LOG, TC, TEAM_CITY)
                  --tests <selector>        Run only tests matching selector; can be repeated
                  --available-tests         Print available test selectors and exit
                  -h, --help                Show this help message
                """);
    }

    private static LogType parseLogType(String value) {
        return switch (value.toUpperCase(Locale.ROOT)) {
            case "LOG" -> LogType.LOG;
            case "TC", "TEAM_CITY" -> LogType.TEAM_CITY;
            default -> throw new IllegalArgumentException("Unknown log type `" + value + "`. Use LOG, TC, or TEAM_CITY.");
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
        throw new IllegalStateException("Cannot find generated method `%s` with %d parameters".formatted(namePrefix, parameterCount));
    }

    private static Object generatedConstant(String name) {
        try {
            return CapyTest.class.getField(name).get(null);
        } catch (ReflectiveOperationException e) {
            throw new IllegalStateException("Cannot read generated CapyTest constant `%s`".formatted(name), e);
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
        throw new IllegalStateException(message + ": unknown result type `%s`".formatted(result == null ? "null" : result.getClass().getCanonicalName()));
    }

    @SuppressWarnings("unchecked")
    private static boolean failed(Object testRun) {
        return Boolean.TRUE.equals(((Map<String, Object>) testRun).get("failed"));
    }
}
