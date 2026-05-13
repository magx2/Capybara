package dev.capylang.test;

import capy.io.Path;
import capy.io.PathRoot;
import capy.lang.Result;
import capy.test.CapyTest;
import capy.test.CapyTest.TestCase;
import capy.test.CapyTest.TestFile;
import capy.test.CapyTest.TestOutput;
import dev.capylang.PathUtil;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.file.Files;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.*;

class TestRunnerTest {
    private static final String OUTPUT_MANIFEST_FILE = ".capy-test-output-manifest";

    @TempDir
    java.nio.file.Path tempDir;

    @Test
    void shouldWriteTestOutputAndReturnRelativePath() throws Exception {
        var output = new TestOutput(relativePath("reports", "TEST-capy.lang.MathTest.xml"), "<xml/>", false);

        var writtenPaths = successValue(CapyTest.writeTestOutputs(PathUtil.fromJavaPath(tempDir), List.of(output)).unsafeRun());

        assertEquals(List.of(relativePath("reports", "TEST-capy.lang.MathTest.xml")), writtenPaths);
        assertEquals("<xml/>", Files.readString(tempDir.resolve("reports").resolve("TEST-capy.lang.MathTest.xml")));
    }

    @Test
    void shouldDeleteOnlyStaleOutputsAndEmptyDirectories() throws Exception {
        var keptFile = tempDir.resolve("TEST-capy.lang.MathTest.xml");
        Files.writeString(keptFile, "<new/>");
        var staleFile = tempDir.resolve("stale").resolve("TEST-old.xml");
        Files.createDirectories(staleFile.getParent());
        Files.writeString(staleFile, "<old/>");

        var run = runMathTest();

        assertTrue(Files.exists(keptFile));
        assertFalse(Files.exists(staleFile));
        assertFalse(Files.exists(staleFile.getParent()));
        assertEquals(List.of(relativePath("TEST-capy.lang.MathTest.xml")), run.written_files());
        assertEquals("TEST-capy.lang.MathTest.xml\n", Files.readString(tempDir.resolve(OUTPUT_MANIFEST_FILE)));
    }

    @Test
    void shouldKeepSharedParentDirectoryWhenItStillContainsNonManifestFiles() throws Exception {
        var reportsDir = Files.createDirectories(tempDir.resolve("reports"));
        var keptFile = reportsDir.resolve("notes.txt");
        Files.writeString(keptFile, "keep");
        var staleFile = reportsDir.resolve("TEST-old.xml");
        Files.writeString(staleFile, "<old/>");
        Files.writeString(
                tempDir.resolve(OUTPUT_MANIFEST_FILE),
                String.join("\n", "TEST-capy.lang.MathTest.xml", "reports/TEST-old.xml") + "\n"
        );

        runMathTest();

        assertTrue(Files.exists(keptFile));
        assertFalse(Files.exists(staleFile));
        assertTrue(Files.exists(reportsDir));
    }

    @Test
    void shouldRemoveOldOutputsWhenCurrentRunWritesDifferentReports() throws Exception {
        var staleFile = tempDir.resolve("TEST-old.xml");
        Files.writeString(staleFile, "<old/>");

        runMathTest();

        assertTrue(Files.readString(tempDir.resolve("TEST-capy.lang.MathTest.xml")).contains("should_pass"));
        assertFalse(Files.exists(staleFile));
    }

    @Test
    void shouldNotRewriteIdenticalTestOutput() throws Exception {
        var output = new TestOutput(relativePath("reports", "TEST-capy.lang.MathTest.xml"), "<xml/>", false);

        var writtenPath = successValue(CapyTest.writeTestOutputs(PathUtil.fromJavaPath(tempDir), List.of(output)).unsafeRun()).getFirst();
        var report = tempDir.resolve(PathUtil.toJavaPath(writtenPath));
        var initialModifiedTime = Files.getLastModifiedTime(report);

        Thread.sleep(1100);

        successValue(CapyTest.writeTestOutputs(PathUtil.fromJavaPath(tempDir), List.of(output)).unsafeRun());

        assertEquals(initialModifiedTime, Files.getLastModifiedTime(report));
    }

    @Test
    void shouldRewriteChangedTestOutputWhenSizeChanges() throws Exception {
        var reportPath = relativePath("reports", "TEST-capy.lang.MathTest.xml");

        successValue(CapyTest.writeTestOutputs(
                PathUtil.fromJavaPath(tempDir),
                List.of(new TestOutput(reportPath, "<xml/>", false))
        ).unsafeRun());

        var report = tempDir.resolve("reports").resolve("TEST-capy.lang.MathTest.xml");
        var initialModifiedTime = Files.getLastModifiedTime(report);

        Thread.sleep(1100);

        successValue(CapyTest.writeTestOutputs(
                PathUtil.fromJavaPath(tempDir),
                List.of(new TestOutput(reportPath, "<xml>changed</xml>", false))
        ).unsafeRun());

        assertEquals("<xml>changed</xml>", Files.readString(report));
        assertTrue(Files.getLastModifiedTime(report).compareTo(initialModifiedTime) > 0);
    }

    @Test
    void shouldNotRewriteIdenticalOutputManifest() throws Exception {
        runMathTest();

        var manifestFile = tempDir.resolve(OUTPUT_MANIFEST_FILE);
        var initialModifiedTime = Files.getLastModifiedTime(manifestFile);

        Thread.sleep(1100);

        runMathTest();

        assertEquals(initialModifiedTime, Files.getLastModifiedTime(manifestFile));
    }

    @Test
    void shouldDeleteStaleOutputsFromManifestWithoutWalkingCurrentTree() throws Exception {
        var keptFile = tempDir.resolve("TEST-capy.lang.MathTest.xml");
        Files.writeString(keptFile, "<keep/>");
        var staleFile = tempDir.resolve("stale").resolve("TEST-old.xml");
        Files.createDirectories(staleFile.getParent());
        Files.writeString(staleFile, "<old/>");
        Files.writeString(
                tempDir.resolve(OUTPUT_MANIFEST_FILE),
                String.join("\n", "TEST-capy.lang.MathTest.xml", "stale/TEST-old.xml") + "\n"
        );

        Files.delete(staleFile);

        runMathTest();

        assertTrue(Files.exists(keptFile));
        assertFalse(Files.exists(tempDir.resolve("stale")));
        assertEquals("TEST-capy.lang.MathTest.xml\n", Files.readString(tempDir.resolve(OUTPUT_MANIFEST_FILE)));
    }

    @Test
    void shouldPruneSharedStaleDirectoryTreesInSingleCleanupPass() throws Exception {
        var keptFile = tempDir.resolve("TEST-capy.lang.MathTest.xml");
        Files.writeString(keptFile, "<keep/>");
        var staleDir = Files.createDirectories(tempDir.resolve("stale").resolve("nested"));
        var staleFileOne = staleDir.resolve("TEST-old-one.xml");
        var staleFileTwo = staleDir.resolve("TEST-old-two.xml");
        Files.writeString(staleFileOne, "<old-one/>");
        Files.writeString(staleFileTwo, "<old-two/>");

        runMathTest();

        assertTrue(Files.exists(keptFile));
        assertFalse(Files.exists(staleFileOne));
        assertFalse(Files.exists(staleFileTwo));
        assertFalse(Files.exists(staleDir));
        assertFalse(Files.exists(tempDir.resolve("stale")));
    }

    @Test
    void shouldPrintFailureMessagesToStandardOutput() {
        var summary = CapyTest.failureSummary(List.of(testFile(
                "/capy/lang/MathTest",
                failed("should_add_numbers", "Expected int:\n1\nto be equal to:\n2")
        )));

        assertEquals(
                String.join(
                        "\n",
                        "",
                        "Failures:",
                        "",
                        "  /capy/lang/MathTest > should_add_numbers()",
                        "Expected int:",
                        "1",
                        "to be equal to:",
                        "2",
                        "",
                        ""
                ),
                summary
        );
    }

    @Test
    void shouldRenderEscapedLineSeparatorsInFailureMessages() {
        var summary = CapyTest.failureSummary(List.of(testFile(
                "/capy/lang/Json.cfun",
                failed(
                        "should_deserialize_array_with_values",
                        "Expected result:\\nError { \"message\": boom }\\nto succeed with value:\\nJsonObject { \"value\": {} }"
                )
        )));

        assertEquals(
                String.join(
                        "\n",
                        "",
                        "Failures:",
                        "",
                        "  /capy/lang/Json.cfun > should_deserialize_array_with_values()",
                        "Expected result:",
                        "Error { \"message\": boom }",
                        "to succeed with value:",
                        "JsonObject { \"value\": {} }",
                        "",
                        ""
                ),
                summary
        );
    }

    @Test
    void shouldNormalizeActualAndEscapedNewlinesForWindowsOutput() {
        assertEquals(
                "Expected int:\n1\nto be equal to:\n2",
                CapyTest.normalizeFailureMessage("Expected int:\n1\\nto be equal to:\r\n2")
        );
    }

    @Test
    void shouldNotPrintAnythingWhenThereAreNoFailures() {
        var summary = CapyTest.failureSummary(List.of(testFile("/capy/lang/MathTest", passed("should_add_numbers"))));

        assertEquals("", summary);
    }

    @Test
    void shouldParseOptionalLogOutputType() {
        var logArguments = TestRunner.parseArguments(new String[]{
                "-o", tempDir.toString(),
                "-rt", "JUNIT",
                "-l", "LOG"
        });
        var teamCityArguments = TestRunner.parseArguments(new String[]{
                "-o", tempDir.toString(),
                "-rt", "JUNIT",
                "--log", "TC"
        });
        var longTeamCityArguments = TestRunner.parseArguments(new String[]{
                "-o", tempDir.toString(),
                "-rt", "JUNIT",
                "--log", "TEAM_CITY"
        });

        assertEquals(CapyTest.LogType.LOG, logArguments.logType());
        assertEquals(CapyTest.LogType.TEAM_CITY, teamCityArguments.logType());
        assertEquals(CapyTest.LogType.TEAM_CITY, longTeamCityArguments.logType());
    }

    @Test
    void shouldDefaultToNoLogOutput() {
        var arguments = TestRunner.parseArguments(new String[]{
                "-o", tempDir.toString(),
                "-rt", "JUNIT"
        });

        assertEquals(CapyTest.LogType.NONE, arguments.logType());
    }

    @Test
    void shouldParseCtrfReportOutputTypes() {
        var ctrfArguments = TestRunner.parseArguments(new String[]{
                "-o", tempDir.toString(),
                "-rt", "CTRF"
        });
        var junitCtrfArguments = TestRunner.parseArguments(new String[]{
                "-o", tempDir.toString(),
                "-rt", "JUNIT_CTRF"
        });
        var jestArguments = TestRunner.parseArguments(new String[]{
                "-o", tempDir.toString(),
                "-rt", "JEST"
        });
        var junitCtrfJestArguments = TestRunner.parseArguments(new String[]{
                "-o", tempDir.toString(),
                "-rt", "JUNIT_CTRF_JEST"
        });

        assertEquals(TestRunner.ReportType.CTRF, ctrfArguments.reportType());
        assertEquals(TestRunner.ReportType.JUNIT_CTRF, junitCtrfArguments.reportType());
        assertEquals(TestRunner.ReportType.JEST, jestArguments.reportType());
        assertEquals(TestRunner.ReportType.JUNIT_CTRF_JEST, junitCtrfJestArguments.reportType());
    }

    @Test
    void shouldParseRepeatedTestSelectors() {
        var arguments = TestRunner.parseArguments(new String[]{
                "-o", tempDir.toString(),
                "-rt", "JUNIT",
                "--tests", "/capy/lang/EffectTest",
                "--tests", "/capy/lang/EffectTest.\"should run pure value\""
        });

        assertEquals(
                List.of(
                        "/capy/lang/EffectTest",
                        "/capy/lang/EffectTest.\"should run pure value\""
                ),
                arguments.testSelectors()
        );
    }

    @Test
    void shouldParseAvailableTestsWithoutReportOutputOptions() {
        var arguments = TestRunner.parseArguments(new String[]{"--available-tests"});

        assertTrue(arguments.availableTests());
        assertTrue(arguments.testSelectors().isEmpty());
    }

    @Test
    void shouldListAvailableTestsAsRunnableSelectors() {
        var testFiles = List.of(
                testFile(
                        "/capy/lang/EffectTest.cfun",
                        passed("should run pure value"),
                        passed("should map \"quoted\" value")
                ),
                testFile("/capy/lang/StringTest.cfun", passed("should escape \\ values"))
        );

        assertEquals(
                List.of(
                        "/capy/lang/EffectTest.\"should run pure value\"",
                        "/capy/lang/EffectTest.\"should map \\\"quoted\\\" value\"",
                        "/capy/lang/StringTest.\"should escape \\\\ values\""
                ),
                TestRunner.availableTests(testFiles)
        );
    }

    @Test
    void shouldFilterAllTestsFromSelectedFile() {
        var testFiles = List.of(
                testFile(
                        "/capy/lang/EffectTest.cfun",
                        passed("should run pure value"),
                        passed("should map value")
                ),
                testFile("/capy/lang/StringTest.cfun", passed("should start with prefix"))
        );

        var filtered = TestRunner.filterTestFiles(testFiles, List.of("/capy/lang/EffectTest"));

        assertEquals(1, filtered.size());
        assertEquals("/capy/lang/EffectTest.cfun", filtered.getFirst().file_name());
        assertEquals(
                List.of("should run pure value", "should map value"),
                filtered.getFirst().test_cases().stream().map(TestCase::name).toList()
        );
    }

    @Test
    void shouldFilterOnlySelectedTestCase() {
        var testFiles = List.of(testFile(
                "/capy/lang/EffectTest.cfun",
                passed("should run pure value"),
                passed("should map value")
        ));

        var filtered = TestRunner.filterTestFiles(
                testFiles,
                List.of("/capy/lang/EffectTest.\"should run pure value\"")
        );

        assertEquals(1, filtered.size());
        assertEquals(
                List.of("should run pure value"),
                filtered.getFirst().test_cases().stream().map(TestCase::name).toList()
        );
    }

    @Test
    void shouldUnescapeSelectedTestCaseName() {
        var testFiles = List.of(testFile(
                "/capy/lang/EffectTest.cfun",
                passed("should map \"quoted\" value"),
                passed("should escape \\ values")
        ));

        var filtered = TestRunner.filterTestFiles(
                testFiles,
                List.of(
                        "/capy/lang/EffectTest.\"should map \\\"quoted\\\" value\"",
                        "/capy/lang/EffectTest.\"should escape \\\\ values\""
                )
        );

        assertEquals(
                List.of("should map \"quoted\" value", "should escape \\ values"),
                filtered.getFirst().test_cases().stream().map(TestCase::name).toList()
        );
    }

    @Test
    void shouldFailWhenSelectedTestDoesNotExist() {
        var testFiles = List.of(testFile("/capy/lang/EffectTest.cfun", passed("should run pure value")));

        var exception = assertThrows(
                IllegalArgumentException.class,
                () -> TestRunner.filterTestFiles(testFiles, List.of("/capy/lang/EffectTest.\"missing\""))
        );

        assertEquals(
                "Test selector `/capy/lang/EffectTest.\"missing\"` did not match any test",
                exception.getMessage()
        );
    }

    @Test
    void shouldNotExecuteUnselectedTestBodies() {
        var executed = new AtomicInteger();
        var previousSelection = TestSelection.setSelection(TestSelection.Selection.from(
                List.of("/capy/lang/SelectionTest.\"selected\""),
                false
        ));
        try {
            var testFile = CapyTest.testFile(
                    "/capy/lang/SelectionTest.cfun",
                    List.of(
                            CapyTest.test("selected", () -> {
                                executed.incrementAndGet();
                                return capy.test.Assert.assertThat("capybara").startsWith("capy");
                            }),
                            CapyTest.test("skipped", () -> {
                                executed.addAndGet(100);
                                return capy.test.Assert.assertThat("capybara").startsWith("capy");
                            })
                    )
            ).unsafeRun();

            assertEquals(1, executed.get());
            assertEquals(
                    List.of("selected"),
                    testFile.test_cases().stream().map(TestCase::name).toList()
            );
        } finally {
            TestSelection.restoreSelection(previousSelection);
        }
    }

    @Test
    void shouldListAvailableTestsWithoutExecutingBodies() {
        var previousSelection = TestSelection.setSelection(TestSelection.Selection.from(List.of(), true));
        try {
            var testFile = CapyTest.testFile(
                    "/capy/lang/SelectionTest.cfun",
                    List.of(
                            CapyTest.test("first", () -> {
                                throw new AssertionError("available tests should not execute bodies");
                            }),
                            CapyTest.test("second", () -> {
                                throw new AssertionError("available tests should not execute bodies");
                            })
                    )
            ).unsafeRun();

            assertEquals(
                    List.of(
                            "/capy/lang/SelectionTest.\"first\"",
                            "/capy/lang/SelectionTest.\"second\""
                    ),
                    TestRunner.availableTests(List.of(testFile))
            );
        } finally {
            TestSelection.restoreSelection(previousSelection);
        }
    }

    @Test
    void shouldRenderTeamCityMessagesForTestRun() {
        var messages = CapyTest.teamCityMessages(List.of(testFile(
                "/capy/lang/StringTest.cfun",
                passed("starts_with should pass"),
                failed("starts_with should fail", "Expected string:\ncapybara\nto start with:\nbara", "assertion failed")
        )));

        assertEquals(
                List.of(
                        "##teamcity[testSuiteStarted name='/capy/lang/StringTest.cfun']",
                        "##teamcity[testStarted name='starts_with should pass']",
                        "##teamcity[testFinished name='starts_with should pass']",
                        "##teamcity[testStarted name='starts_with should fail']",
                        "##teamcity[testFailed name='starts_with should fail' message='assertion failed' details='Expected string:|ncapybara|nto start with:|nbara']",
                        "##teamcity[testFinished name='starts_with should fail']",
                        "##teamcity[testSuiteFinished name='/capy/lang/StringTest.cfun']"
                ),
                messages
        );
    }

    @Test
    void shouldRenderPlainLogLinesForTestRun() {
        var lines = CapyTest.testLogLines(List.of(testFile(
                "/capy/lang/StringTest.cfun",
                failed("starts_with should fail", "Expected string:\ncapybara\nto start with:\nbara", "assertion failed")
        )));

        assertEquals(
                List.of(
                        "Test suite started: /capy/lang/StringTest.cfun",
                        "Test started: starts_with should fail",
                        "Test failed: starts_with should fail",
                        "Expected string:\ncapybara\nto start with:\nbara",
                        "Test finished: starts_with should fail",
                        "Test suite finished: /capy/lang/StringTest.cfun"
                ),
                lines
        );
    }

    @Test
    void shouldEscapeTeamCityMessageValues() {
        assertEquals("|'|||n|r|[|]", CapyTest.teamCityEscape("'|\n\r[]"));
    }

    @Test
    void shouldRenderCtrfReportForTestRun() {
        var report = CapyTest.ctrfReport(List.of(testFile(
                "/capy/lang/StringTest.cfun",
                passed("starts_with should pass"),
                failed("starts_with should fail", "Expected string:\ncapybara\nto start with:\nbara", "assertion failed")
        )));

        assertTrue(report.contains("\"reportFormat\":\"CTRF\""));
        assertTrue(report.contains("\"specVersion\":\"0.0.0\""));
        assertTrue(report.contains("\"tool\":{\"name\":\"Capybara\"}"));
        assertTrue(report.contains("\"summary\":{\"tests\":2,\"passed\":1,\"failed\":1,\"skipped\":0,\"pending\":0,\"other\":0,\"suites\":1,\"start\":0,\"stop\":0,\"duration\":0}"));
        assertTrue(report.contains("\"name\":\"starts_with should pass\",\"status\":\"passed\",\"duration\":0,\"suite\":[\"/capy/lang/StringTest.cfun\"],\"filePath\":\"/capy/lang/StringTest.cfun\""));
        assertTrue(report.contains("\"name\":\"starts_with should fail\",\"status\":\"failed\",\"duration\":0,\"suite\":[\"/capy/lang/StringTest.cfun\"],\"filePath\":\"/capy/lang/StringTest.cfun\",\"message\":\"Expected string:\\ncapybara\\nto start with:\\nbara\",\"trace\":\"Expected string:\\ncapybara\\nto start with:\\nbara\",\"extra\":{\"failureType\":\"assertion failed\"}"));
    }

    @Test
    void shouldEscapeJsonControlCharactersInCtrfReport() {
        var controlCharacters = controlCharacters();
        var escapedControlCharacters = escapedControlCharacters();
        var message = "line" + (char) 0 + "\b\fbreak";
        var report = CapyTest.ctrfReport(List.of(testFile(
                "/capy/lang/StringTest.cfun",
                failed("control" + controlCharacters + "name", message, "assertion failed")
        )));

        assertFalse(report.chars().anyMatch(character -> character < 0x20));
        assertTrue(report.contains("\"name\":\"control" + escapedControlCharacters + "name\""));
        assertTrue(report.contains("\"message\":\"line\\u0000\\b\\fbreak\""));
        assertTrue(report.contains("\"trace\":\"line\\u0000\\b\\fbreak\""));
    }

    @Test
    void shouldRenderJestReportForTestRun() {
        var report = CapyTest.jestReport(List.of(testFile(
                "/capy/lang/StringTest.cfun",
                passed("starts_with should pass"),
                failed("starts_with should fail", "Expected string:\ncapybara\nto start with:\nbara", "assertion failed")
        )));

        assertTrue(report.contains("\"success\":false"));
        assertTrue(report.contains("\"numTotalTestSuites\":1"));
        assertTrue(report.contains("\"numPassedTestSuites\":0"));
        assertTrue(report.contains("\"numFailedTestSuites\":1"));
        assertTrue(report.contains("\"numTotalTests\":2"));
        assertTrue(report.contains("\"numPassedTests\":1"));
        assertTrue(report.contains("\"numFailedTests\":1"));
        assertTrue(report.contains("\"startTime\":0"));
        assertTrue(report.contains("\"name\":\"/capy/lang/StringTest.cfun\",\"status\":\"failed\",\"startTime\":0,\"endTime\":0,\"assertionResults\""));
        assertTrue(report.contains("\"ancestorTitles\":[\"/capy/lang/StringTest.cfun\"],\"title\":\"starts_with should pass\",\"status\":\"passed\",\"duration\":0,\"failureMessages\":[],\"location\":null"));
        assertTrue(report.contains("\"title\":\"starts_with should fail\",\"status\":\"failed\",\"duration\":0,\"failureMessages\":[\"Expected string:\\ncapybara\\nto start with:\\nbara\"],\"location\":null"));
    }

    @Test
    void shouldEscapeJsonControlCharactersInJestReport() {
        var controlCharacters = controlCharacters();
        var escapedControlCharacters = escapedControlCharacters();
        var message = "line" + (char) 0 + "\b\fbreak";
        var report = CapyTest.jestReport(List.of(testFile(
                "/capy/lang/StringTest.cfun",
                failed("control" + controlCharacters + "name", message, "assertion failed")
        )));

        assertFalse(report.chars().anyMatch(character -> character < 0x20));
        assertTrue(report.contains("\"title\":\"control" + escapedControlCharacters + "name\""));
        assertTrue(report.contains("\"failureMessages\":[\"line\\u0000\\b\\fbreak\"]"));
    }

    @Test
    void shouldWriteJUnitCtrfAndJestReportsTogether() throws Exception {
        var run = successValue(CapyTest.runTests(
                CapyTest.ReportType.JUNIT_CTRF_JEST,
                PathUtil.fromJavaPath(tempDir),
                List.of(testFile("/capy/lang/MathTest", passed("should_pass")))
        ).unsafeRun());

        assertEquals(
                List.of(
                        relativePath("TEST-capy.lang.MathTest.xml"),
                        relativePath("ctrf-report.json"),
                        relativePath("jest-report.json")
                ),
                run.written_files()
        );
        assertTrue(Files.exists(tempDir.resolve("TEST-capy.lang.MathTest.xml")));
        assertTrue(Files.readString(tempDir.resolve("ctrf-report.json")).contains("\"reportFormat\":\"CTRF\""));
        assertTrue(Files.readString(tempDir.resolve("jest-report.json")).contains("\"success\":true"));
        assertEquals("TEST-capy.lang.MathTest.xml\nctrf-report.json\njest-report.json\n", Files.readString(tempDir.resolve(OUTPUT_MANIFEST_FILE)));
    }

    @Test
    void shouldPrintTeamCityMessagesWhileExecutingTestFile() {
        var originalOut = System.out;
        var previousLogType = TestLog.currentLogType();
        var stdout = new ByteArrayOutputStream();
        try {
            System.setOut(new PrintStream(stdout));
            TestLog.setLogType(CapyTest.LogType.TEAM_CITY);

            CapyTest.testFile(
                    "/capy/lang/StringTest.cfun",
                    List.of(CapyTest.test("starts_with should fail", () -> {
                        System.out.println("ASSERTION_BODY");
                        return capy.test.Assert.assertThat("capybara").startsWith("bara");
                    }))
            ).unsafeRun();
        } finally {
            TestLog.setLogType(previousLogType);
            System.setOut(originalOut);
        }

        var output = stdout.toString();
        var suiteStarted = output.indexOf("##teamcity[testSuiteStarted name='/capy/lang/StringTest.cfun']");
        var testStarted = output.indexOf("##teamcity[testStarted name='starts_with should fail']");
        var assertionBody = output.indexOf("ASSERTION_BODY");
        var testFailed = output.indexOf("##teamcity[testFailed name='starts_with should fail' message='assertion failed' details='Expected string:|ncapybara|nto start with:|nbara']");
        var testFinished = output.indexOf("##teamcity[testFinished name='starts_with should fail']");
        var suiteFinished = output.indexOf("##teamcity[testSuiteFinished name='/capy/lang/StringTest.cfun']");

        assertTrue(suiteStarted >= 0);
        assertTrue(suiteStarted < testStarted);
        assertTrue(testStarted < assertionBody);
        assertTrue(assertionBody < testFailed);
        assertTrue(testFailed < testFinished);
        assertTrue(testFinished < suiteFinished);
    }

    private CapyTest.TestRun runMathTest() {
        return successValue(CapyTest.runTests(
                CapyTest.ReportType.JUNIT,
                PathUtil.fromJavaPath(tempDir),
                List.of(testFile("/capy/lang/MathTest", passed("should_pass")))
        ).unsafeRun());
    }

    private static TestFile testFile(String fileName, TestCase... testCases) {
        return new TestFile(fileName, List.of(testCases), 0L);
    }

    private static TestCase passed(String name) {
        return new TestCase(name, CapyTest.Passed.INSTANCE, 1, 0.0);
    }

    private static TestCase failed(String name, String message) {
        return failed(name, message, "Assert.type");
    }

    private static TestCase failed(String name, String message, String type) {
        return new TestCase(name, new CapyTest.Failed(message, type), 1, 0.0);
    }

    private static String controlCharacters() {
        var controlCharacters = new StringBuilder();
        for (var character = 0; character < 0x20; character++) {
            controlCharacters.append((char) character);
        }
        return controlCharacters.toString();
    }

    private static String escapedControlCharacters() {
        return "\\u0000\\u0001\\u0002\\u0003\\u0004\\u0005\\u0006\\u0007"
               + "\\b\\t\\n\\u000b\\f\\r\\u000e\\u000f"
               + "\\u0010\\u0011\\u0012\\u0013\\u0014\\u0015\\u0016\\u0017"
               + "\\u0018\\u0019\\u001a\\u001b\\u001c\\u001d\\u001e\\u001f";
    }

    private static Path relativePath(String... segments) {
        return new Path(PathRoot.RELATIVE, Optional.empty(), List.of(segments));
    }

    @SuppressWarnings("unchecked")
    private static <T> T successValue(Result<T> result) {
        var success = assertInstanceOf(Result.Success.class, result);
        return (T) success.value();
    }
}
