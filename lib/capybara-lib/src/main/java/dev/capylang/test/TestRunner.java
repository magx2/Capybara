package dev.capylang.test;

import capy.test.CapyTest;
import capy.lang.Result;
import capy.test.CapyTest.TestRun;
import dev.capylang.PathUtil;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Locale;

public class TestRunner {

    public static final String CAPY_TEST_RUNTIME_CLASS = "capy.test.CapyTestRuntime";
    public static final String CAPY_TEST_CLASS = "capy.test.CapyTest";
    public static final String GATHER_TESTS_METHOD_NAME = "gatherTests";
    public static final String RUN_TESTS_METHOD_NAME = "runTests";

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
        if (!testRun.failure_summary().isEmpty()) {
            System.out.print(testRun.failure_summary());
        }
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

    private static ClassLoader contextClassLoader() {
        var classLoader = Thread.currentThread().getContextClassLoader();
        return classLoader == null ? TestRunner.class.getClassLoader() : classLoader;
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
}
