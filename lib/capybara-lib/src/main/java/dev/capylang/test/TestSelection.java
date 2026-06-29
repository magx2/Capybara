package dev.capylang.test;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public final class TestSelection {
    private TestSelection() {
    }

    static List<String> availableTests(List<Object> testFiles) {
        return testFiles.stream()
                .flatMap(testFile -> testCases(testFile).stream()
                        .map(testCase -> testId(testFile, testCase)))
                .toList();
    }

    static List<Object> filterTestFiles(
            List<Object> testFiles,
            List<String> testSelectors
    ) {
        if (testSelectors == null || testSelectors.isEmpty()) {
            return testFiles;
        }

        var selectors = testSelectors.stream()
                .map(Selector::parse)
                .toList();
        var matched = new boolean[selectors.size()];
        var filteredFiles = new ArrayList<Object>();

        for (var testFile : testFiles) {
            for (int i = 0; i < selectors.size(); i++) {
                if (!selectors.get(i).hasTestName() && selectors.get(i).matchesFile(fileName(testFile))) {
                    matched[i] = true;
                }
            }

            var filteredCases = new ArrayList<Object>();
            for (var testCase : testCases(testFile)) {
                var include = false;
                for (int i = 0; i < selectors.size(); i++) {
                    if (selectors.get(i).matches(fileName(testFile), testName(testCase))) {
                        matched[i] = true;
                        include = true;
                    }
                }
                if (include) {
                    filteredCases.add(testCase);
                }
            }

            if (!filteredCases.isEmpty()) {
                filteredFiles.add(Map.of(
                        "__type", "TestFile",
                        "file_name", fileName(testFile),
                        "test_cases", List.copyOf(filteredCases),
                        "timestamp_millis", field(testFile, "timestamp_millis")
                ));
            }
        }

        var missingSelectors = new ArrayList<String>();
        for (int i = 0; i < matched.length; i++) {
            if (!matched[i]) {
                missingSelectors.add(selectors.get(i).raw());
            }
        }
        if (missingSelectors.size() == 1) {
            throw new IllegalArgumentException("Test selector `%s` did not match any test".formatted(missingSelectors.getFirst()));
        }
        if (!missingSelectors.isEmpty()) {
            throw new IllegalArgumentException("Test selectors did not match any test: %s".formatted(
                    String.join(", ", missingSelectors.stream()
                            .map(selector -> "`" + selector + "`")
                            .toList())
            ));
        }

        return filteredFiles;
    }

    private static String testId(Object testFile, Object testCase) {
        return normalizedTestFileName(fileName(testFile)) + ".\"" + escapeTestName(testName(testCase)) + "\"";
    }

    private static String fileName(Object testFile) {
        return String.valueOf(field(testFile, "file_name"));
    }

    private static String testName(Object testCase) {
        return String.valueOf(field(testCase, "name"));
    }

    @SuppressWarnings("unchecked")
    private static List<Object> testCases(Object testFile) {
        return (List<Object>) field(testFile, "test_cases");
    }

    @SuppressWarnings("unchecked")
    private static Object field(Object value, String name) {
        return ((Map<String, Object>) value).get(name);
    }

    private static String normalizedTestFileName(String fileName) {
        if (fileName != null && fileName.endsWith(".cfun")) {
            return fileName.substring(0, fileName.length() - ".cfun".length());
        }
        return fileName;
    }

    private static String escapeTestName(String testName) {
        return testName
                .replace("\\", "\\\\")
                .replace("\"", "\\\"");
    }

    private static String unescapeTestName(String testName) {
        var result = new StringBuilder();
        var escaping = false;
        for (int i = 0; i < testName.length(); i++) {
            var current = testName.charAt(i);
            if (escaping) {
                result.append(current);
                escaping = false;
            } else if (current == '\\') {
                escaping = true;
            } else {
                result.append(current);
            }
        }
        if (escaping) {
            result.append('\\');
        }
        return result.toString();
    }

    record Selector(String raw, String fileName, String testName) {
        static Selector parse(String raw) {
            var selector = raw.trim();
            var testNameSeparator = selector.indexOf(".\"");
            if (testNameSeparator < 0) {
                return new Selector(raw, normalizedTestFileName(selector), null);
            }
            if (!selector.endsWith("\"")) {
                throw new IllegalArgumentException("Invalid test selector `%s`".formatted(raw));
            }
            var fileName = selector.substring(0, testNameSeparator);
            var testName = selector.substring(testNameSeparator + 2, selector.length() - 1);
            return new Selector(raw, normalizedTestFileName(fileName), unescapeTestName(testName));
        }

        boolean hasTestName() {
            return testName != null;
        }

        boolean matchesFile(String otherFileName) {
            return fileName.equals(normalizedTestFileName(otherFileName));
        }

        boolean matches(String otherFileName, String otherTestName) {
            return matchesFile(otherFileName) && (testName == null || testName.equals(otherTestName));
        }
    }
}
