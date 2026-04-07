package dev.capylang.test;

import capy.io.Path;
import capy.io.PathRoot;
import capy.test.CapyTest.TestOutput;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

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

    private static Path relativePath(String... segments) {
        return new Path(PathRoot.RELATIVE, Optional.empty(), List.of(segments));
    }
}
