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
        return new TestFile(fileName, List.of(testCases), "1970-01-01T00:00:00Z");
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

    private static Path relativePath(String... segments) {
        return new Path(PathRoot.RELATIVE, Optional.empty(), List.of(segments));
    }

    @SuppressWarnings("unchecked")
    private static <T> T successValue(Result<T> result) {
        var success = assertInstanceOf(Result.Success.class, result);
        return (T) success.value();
    }
}
