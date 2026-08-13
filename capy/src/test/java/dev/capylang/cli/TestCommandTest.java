package dev.capylang.cli;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.api.parallel.ResourceLock;
import org.junit.jupiter.api.parallel.Resources;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.assertj.core.api.Assertions.assertThat;

@ResourceLock(Resources.SYSTEM_ERR)
class TestCommandTest {
    @TempDir
    Path tempDir;

    @Test
    void packagesJavaScriptAndPythonRunners() {
        assertThat(TestCommand.class.getResource("/test-runner/js/run-capybara-tests.js")).isNotNull();
        assertThat(TestCommand.class.getResource("/test-runner/python/run-capybara-tests.py")).isNotNull();
    }

    @Test
    void rejectsUnsupportedReportTypeBeforeStartingRuntime() {
        var result = executeWithCapturedError(
                "js",
                "--generated-dir", tempDir.toString(),
                "--output-dir", tempDir.resolve("reports").toString(),
                "--report-type", "ADOC"
        );

        assertThat(result.exitCode()).isEqualTo(2);
        assertThat(result.error()).contains("Use JUNIT, CTRF, or JEST");
    }

    @Test
    void rejectsMissingGeneratedDirectory() {
        var missing = tempDir.resolve("missing");

        var result = executeWithCapturedError(
                "python",
                "--generated-dir", missing.toString(),
                "--available-tests"
        );

        assertThat(result.exitCode()).isEqualTo(2);
        assertThat(result.error()).contains("does not exist or is not a directory");
    }

    @Test
    void runsGeneratedJavaSourcesAndWritesReportsOutsideGeneratedDirectory() throws Exception {
        var generated = tempDir.resolve("generated Java tests with spaces");
        var source = generated.resolve("capy/test/CapyTestRuntime.java");
        Files.createDirectories(source.getParent());
        try (var input = TestCommandTest.class.getResourceAsStream("/test-runner/java/capy/test/CapyTestRuntime.java")) {
            assertThat(input).isNotNull();
            Files.copy(input, source);
        }
        var output = tempDir.resolve("Java reports with spaces");

        var exitCode = TestCommand.execute(new String[]{
                "java",
                "--generated-dir", generated.toString(),
                "--output-dir", output.toString(),
                "--report-type", "JUNIT",
                "--log", "TC"
        });

        assertThat(exitCode).isZero();
        assertThat(output.resolve("TEST-sample.JavaRunnerTest.cfun.xml")).isRegularFile();
        assertThat(Files.readString(source)).contains("passes from packaged Java runner");
        try (var generatedFiles = Files.walk(generated)) {
            assertThat(generatedFiles.filter(Files::isRegularFile))
                    .allMatch(path -> path.equals(source));
        }
    }

    private static Execution executeWithCapturedError(String... args) {
        var originalError = System.err;
        var error = new ByteArrayOutputStream();
        try {
            System.setErr(new PrintStream(error));
            return new Execution(TestCommand.execute(args), error.toString());
        } finally {
            System.setErr(originalError);
        }
    }

    private record Execution(int exitCode, String error) {
    }
}
