package dev.capylang.cli;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;

import static org.assertj.core.api.Assertions.assertThat;

class CapyMainTest {
    @TempDir
    Path tempDir;

    @Test
    void doesNotReuseSharedInputWhenTestOutputIsAFile() throws Exception {
        var input = Files.createDirectories(tempDir.resolve("input"));
        var output = tempDir.resolve("output");
        var testOutput = tempDir.resolve("test-output");
        Files.writeString(testOutput, "keep me");

        var shared = CapyMain.sharedInputOutputs(new String[]{
                "compile-generate", "python",
                "--input", input.toString(),
                "--output", output.toString(),
                "--test-input", input.toString(),
                "--test-output", testOutput.toString()
        });

        assertThat(shared).isEmpty();
        assertThat(testOutput).content().isEqualTo("keep me");
    }
}
