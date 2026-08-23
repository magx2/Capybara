package dev.capylang.cli;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class CapyJarParsingErrorIntegrationTest {
    @TempDir
    Path tempDir;

    @Test
    void shouldPrintParsingErrorWithoutJavaStackTraceFromPackagedJar() throws Exception {
        assertConciseParsingError(List.of(
                "-jar",
                System.getProperty("capy.jar.path")
        ));
    }

    @Test
    void shouldPrintParsingErrorWithoutJavaStackTraceFromApplicationLauncher() throws Exception {
        assertConciseParsingError(List.of(
                "-cp",
                System.getProperty("capy.jar.path"),
                CapyLauncher.class.getName()
        ));
    }

    private void assertConciseParsingError(List<String> launcherArguments) throws Exception {
        var input = Files.createDirectories(tempDir.resolve("input"));
        var output = tempDir.resolve("output");
        Files.writeString(input.resolve("main.cfun"), "fun broken(): List[int] = [1, 2");

        var arguments = new ArrayList<String>();
        arguments.add(ProcessHandle.current().info().command().orElseThrow());
        arguments.addAll(launcherArguments);
        arguments.addAll(List.of(
                "compile",
                "--input", input.toString(),
                "--output", output.toString()
        ));
        var process = new ProcessBuilder(arguments).redirectErrorStream(true).start();
        var diagnostic = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8)
                .replace("\r\n", "\n");

        assertThat(process.waitFor()).isEqualTo(1);
        assertThat(diagnostic)
                .containsPattern("main\\.cfun:\\d+:\\d+: ParserError: .+\n")
                .doesNotContain("Exception in thread")
                .doesNotContain("\tat ");
    }
}
