package dev.capylang.test;

import capy.test.CapyTest;

import java.util.ArrayList;
import java.util.List;

public final class TestSelection {
    private TestSelection() {
    }

    static List<String> availableTests(List<CapyTest.TestFile> testFiles) {
        return testFiles.stream()
                .flatMap(testFile -> testFile.test_cases().stream()
                        .map(testCase -> testId(testFile, testCase)))
                .toList();
    }

    static List<CapyTest.TestFile> filterTestFiles(
            List<CapyTest.TestFile> testFiles,
            List<String> testSelectors
    ) {
        if (testSelectors == null || testSelectors.isEmpty()) {
            return testFiles;
        }

        var selectors = testSelectors.stream()
                .map(Selector::parse)
                .toList();
        var matched = new boolean[selectors.size()];
        var filteredFiles = new ArrayList<CapyTest.TestFile>();

        for (var testFile : testFiles) {
            for (int i = 0; i < selectors.size(); i++) {
                if (!selectors.get(i).hasTestName() && selectors.get(i).matchesFile(testFile.file_name())) {
                    matched[i] = true;
                }
            }

            var filteredCases = new ArrayList<CapyTest.TestCase>();
            for (var testCase : testFile.test_cases()) {
                var include = false;
                for (int i = 0; i < selectors.size(); i++) {
                    if (selectors.get(i).matches(testFile.file_name(), testCase.name())) {
                        matched[i] = true;
                        include = true;
                    }
                }
                if (include) {
                    filteredCases.add(testCase);
                }
            }

            if (!filteredCases.isEmpty()) {
                filteredFiles.add(new CapyTest.TestFile(
                        testFile.file_name(),
                        filteredCases,
                        testFile.timestamp_millis()
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

    private static String testId(CapyTest.TestFile testFile, CapyTest.TestCase testCase) {
        return normalizedTestFileName(testFile.file_name()) + ".\"" + escapeTestName(testCase.name()) + "\"";
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
