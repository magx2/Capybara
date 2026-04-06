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

    private static Path relativePath(String... segments) {
        return new Path(PathRoot.RELATIVE, Optional.empty(), List.of(segments));
    }
}
