package dev.capylang.test;

import capy.io.Path;
import capy.io.PathRoot;
import capy.test.CapyTest.TestOutput;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.file.Files;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.logging.Level;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class TestRunnerTest {
    @TempDir
    java.nio.file.Path tempDir;

    @Test
    void shouldWriteTestOutputAndReturnRelativePath() throws Exception {
        var arguments = new TestRunner.Arguments(tempDir, TestRunner.ReportType.JUNIT, Level.INFO);
        var output = new TestOutput(relativePath("reports", "TEST-capy.lang.MathTest.xml"), "<xml/>", false);

        var writtenPath = TestRunner.writeTestOutputToFile(output, arguments);

        assertEquals(java.nio.file.Path.of("reports", "TEST-capy.lang.MathTest.xml"), writtenPath);
        assertEquals("<xml/>", Files.readString(tempDir.resolve(writtenPath)));
    }

    @Test
    void shouldDeleteOnlyStaleOutputsAndEmptyDirectories() throws Exception {
        var keptFile = Files.createDirectories(tempDir.resolve("reports")).resolve("TEST-keep.xml");
        Files.writeString(keptFile, "<new/>");
        var staleFile = tempDir.resolve("stale").resolve("TEST-old.xml");
        Files.createDirectories(staleFile.getParent());
        Files.writeString(staleFile, "<old/>");

        TestRunner.deleteStaleOutputs(tempDir, Set.of(java.nio.file.Path.of("reports", "TEST-keep.xml")));

        assertTrue(Files.exists(keptFile));
        assertFalse(Files.exists(staleFile));
        assertFalse(Files.exists(staleFile.getParent()));
        assertEquals("reports/TEST-keep.xml" + System.lineSeparator(),
                Files.readString(tempDir.resolve(TestRunner.OUTPUT_MANIFEST_FILE)));
    }

    @Test
    void shouldKeepSharedParentDirectoryWhenItStillContainsExpectedOutputs() throws Exception {
        var reportsDir = Files.createDirectories(tempDir.resolve("reports"));
        var keptFile = reportsDir.resolve("TEST-keep.xml");
        Files.writeString(keptFile, "<keep/>");
        var staleFile = reportsDir.resolve("TEST-old.xml");
        Files.writeString(staleFile, "<old/>");

        TestRunner.deleteStaleOutputs(tempDir, Set.of(java.nio.file.Path.of("reports", "TEST-keep.xml")));

        assertTrue(Files.exists(keptFile));
        assertFalse(Files.exists(staleFile));
        assertTrue(Files.exists(reportsDir));
    }

    @Test
    void shouldRemoveOldOutputsWhenCurrentRunWritesDifferentReports() throws Exception {
        var arguments = new TestRunner.Arguments(tempDir, TestRunner.ReportType.JUNIT, Level.INFO);
        var staleFile = tempDir.resolve("TEST-old.xml");
        Files.writeString(staleFile, "<old/>");
        var currentOutput = new TestOutput(relativePath("TEST-new.xml"), "<new/>", false);

        var writtenFiles = Set.of(TestRunner.writeTestOutputToFile(currentOutput, arguments));
        TestRunner.deleteStaleOutputs(tempDir, writtenFiles);

        assertEquals("<new/>", Files.readString(tempDir.resolve("TEST-new.xml")));
        assertFalse(Files.exists(staleFile));
    }

    @Test
    void shouldNotRewriteIdenticalTestOutput() throws Exception {
        var arguments = new TestRunner.Arguments(tempDir, TestRunner.ReportType.JUNIT, Level.INFO);
        var output = new TestOutput(relativePath("reports", "TEST-capy.lang.MathTest.xml"), "<xml/>", false);

        var writtenPath = TestRunner.writeTestOutputToFile(output, arguments);
        var report = tempDir.resolve(writtenPath);
        var initialModifiedTime = Files.getLastModifiedTime(report);

        Thread.sleep(1100);

        TestRunner.writeTestOutputToFile(output, arguments);

        assertEquals(initialModifiedTime, Files.getLastModifiedTime(report));
    }

    @Test
    void shouldRewriteChangedTestOutputWhenSizeChanges() throws Exception {
        var arguments = new TestRunner.Arguments(tempDir, TestRunner.ReportType.JUNIT, Level.INFO);
        var reportPath = relativePath("reports", "TEST-capy.lang.MathTest.xml");

        TestRunner.writeTestOutputToFile(new TestOutput(reportPath, "<xml/>", false), arguments);

        var report = tempDir.resolve(java.nio.file.Path.of("reports", "TEST-capy.lang.MathTest.xml"));
        var initialModifiedTime = Files.getLastModifiedTime(report);

        Thread.sleep(1100);

        TestRunner.writeTestOutputToFile(new TestOutput(reportPath, "<xml>changed</xml>", false), arguments);

        assertEquals("<xml>changed</xml>", Files.readString(report));
        assertTrue(Files.getLastModifiedTime(report).compareTo(initialModifiedTime) > 0);
    }

    @Test
    void shouldNotRewriteIdenticalOutputManifest() throws Exception {
        var expectedFiles = Set.of(java.nio.file.Path.of("reports", "TEST-keep.xml"));
        Files.createDirectories(tempDir.resolve("reports"));

        TestRunner.deleteStaleOutputs(tempDir, expectedFiles);

        var manifestFile = tempDir.resolve(TestRunner.OUTPUT_MANIFEST_FILE);
        var initialModifiedTime = Files.getLastModifiedTime(manifestFile);

        Thread.sleep(1100);

        TestRunner.deleteStaleOutputs(tempDir, expectedFiles);

        assertEquals(initialModifiedTime, Files.getLastModifiedTime(manifestFile));
    }

    @Test
    void shouldDeleteStaleOutputsFromManifestWithoutWalkingCurrentTree() throws Exception {
        var keptFile = Files.createDirectories(tempDir.resolve("reports")).resolve("TEST-keep.xml");
        Files.writeString(keptFile, "<keep/>");
        var staleFile = tempDir.resolve("stale").resolve("TEST-old.xml");
        Files.createDirectories(staleFile.getParent());
        Files.writeString(staleFile, "<old/>");
        Files.writeString(
                tempDir.resolve(TestRunner.OUTPUT_MANIFEST_FILE),
                String.join(System.lineSeparator(), "reports/TEST-keep.xml", "stale/TEST-old.xml") + System.lineSeparator()
        );

        Files.delete(staleFile);

        TestRunner.deleteStaleOutputs(tempDir, Set.of(java.nio.file.Path.of("reports", "TEST-keep.xml")));

        assertTrue(Files.exists(keptFile));
        assertFalse(Files.exists(tempDir.resolve("stale")));
        assertEquals("reports/TEST-keep.xml" + System.lineSeparator(),
                Files.readString(tempDir.resolve(TestRunner.OUTPUT_MANIFEST_FILE)));
    }

    @Test
    void shouldPruneSharedStaleDirectoryTreesInSingleCleanupPass() throws Exception {
        var keptFile = Files.createDirectories(tempDir.resolve("reports")).resolve("TEST-keep.xml");
        Files.writeString(keptFile, "<keep/>");
        var staleDir = Files.createDirectories(tempDir.resolve("stale").resolve("nested"));
        var staleFileOne = staleDir.resolve("TEST-old-one.xml");
        var staleFileTwo = staleDir.resolve("TEST-old-two.xml");
        Files.writeString(staleFileOne, "<old-one/>");
        Files.writeString(staleFileTwo, "<old-two/>");

        TestRunner.deleteStaleOutputs(tempDir, Set.of(java.nio.file.Path.of("reports", "TEST-keep.xml")));

        assertTrue(Files.exists(keptFile));
        assertFalse(Files.exists(staleFileOne));
        assertFalse(Files.exists(staleFileTwo));
        assertFalse(Files.exists(staleDir));
        assertFalse(Files.exists(tempDir.resolve("stale")));
    }

    @Test
    void shouldPrintFailureMessagesToStandardOutput() {
        var stdout = new ByteArrayOutputStream();
        var failingOutput = new TestOutput(
                relativePath("reports", "TEST-capy.lang.MathTest.xml"),
                """
                        <testsuite name="/capy/lang/MathTest" tests="1" failures="1" errors="0" skipped="0" assertions="1" time="0" timestamp="1970-01-01T00:00:00Z">
                          <testcase name="should_add_numbers" classname="/capy/lang/MathTest" assertions="1" time="0" file="/capy/lang/MathTest" line="0">
                            <failure message="Expected int" type="IntAssert.is_equal_to"><![CDATA[Expected int:
                        1
                        to be equal to:
                        2]]></failure>
                          </testcase>
                        </testsuite>
                        """,
                true
        );

        TestRunner.printFailureSummary(List.of(failingOutput), new PrintStream(stdout));

        assertEquals(
                String.join(
                        System.lineSeparator(),
                        "",
                        "Failures:",
                        "",
                        "  /capy/lang/MathTest > should_add_numbers()",
                        "Expected int:",
                        "1",
                        "to be equal to:",
                        "2",
                        ""
                ) + System.lineSeparator(),
                stdout.toString()
        );
    }

    @Test
    void shouldRenderEscapedLineSeparatorsInFailureMessages() {
        var stdout = new ByteArrayOutputStream();
        var failingOutput = new TestOutput(
                relativePath("reports", "TEST-capy.lang.Json.cfun.xml"),
                """
                        <testsuite name="/capy/lang/Json.cfun" tests="1" failures="1" errors="0" skipped="0" assertions="1" time="0" timestamp="1970-01-01T00:00:00Z">
                          <testcase name="should_deserialize_array_with_values" classname="/capy/lang/Json.cfun" assertions="1" time="0" file="/capy/lang/Json.cfun" line="0">
                            <failure message="Expected result" type="ResultAssert[T].succeeds"><![CDATA[Expected result:\\nError { "message": boom }\\nto succeed with value:\\nJsonObject { "value": {} }]]></failure>
                          </testcase>
                        </testsuite>
                        """,
                true
        );

        TestRunner.printFailureSummary(List.of(failingOutput), new PrintStream(stdout));

        assertEquals(
                String.join(
                        System.lineSeparator(),
                        "",
                        "Failures:",
                        "",
                        "  /capy/lang/Json.cfun > should_deserialize_array_with_values()",
                        "Expected result:",
                        "Error { \"message\": boom }",
                        "to succeed with value:",
                        "JsonObject { \"value\": {} }",
                        ""
                ) + System.lineSeparator(),
                stdout.toString()
        );
    }

    @Test
    void shouldNormalizeActualAndEscapedNewlinesForWindowsOutput() {
        assertEquals(
                "Expected int:\r\n1\r\nto be equal to:\r\n2",
                TestRunner.normalizeFailureMessage("Expected int:\n1\\nto be equal to:\r\n2", "\r\n")
        );
    }

    @Test
    void shouldNotPrintAnythingWhenThereAreNoFailures() {
        var stdout = new ByteArrayOutputStream();
        var passingOutput = new TestOutput(relativePath("reports", "TEST-capy.lang.MathTest.xml"), "<testsuite/>", false);

        TestRunner.printFailureSummary(List.of(passingOutput), new PrintStream(stdout));

        assertEquals("", stdout.toString());
    }

    private static Path relativePath(String... segments) {
        return new Path(PathRoot.RELATIVE, Optional.empty(), List.of(segments));
    }
}
