package dev.capylang.cli;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class CapyJarDuplicateGeneratedPathIntegrationTest {
    @TempDir
    Path tempDir;

    @Test
    void preservesGenerationOrderWhenPythonModulesShareAnOutputPath() throws Exception {
        var input = Files.createDirectories(tempDir.resolve("input/dev/capylang/test"));
        Files.writeString(input.resolve("Clock.coo"), """
                interface Clock {
                    def now_millis(): long
                }

                class NativeProviderDomain(clock: Clock) {
                    field clock: Clock = clock

                    def read(): long = this.clock.now_millis()
                }
                """);
        Files.writeString(input.resolve("ClockProvider.cfun"), """
                from /capy/lang/Effect import { Effect }
                from /capy/meta_prog/NativeProvider import { NativeProvider }
                from Clock import { Clock }

                @NativeProvider(qualifier: "system")
                fun system_clock(): Effect[Clock] = <native>
                """);

        for (var attempt = 0; attempt < 3; attempt++) {
            var output = tempDir.resolve("output-" + attempt);
            compileGeneratePython(tempDir.resolve("input"), output);

            assertThat(output.resolve("dev/capylang/test/Clock.py"))
                    .content()
                    .contains("class NativeProviderDomain:");
        }
    }

    private static void compileGeneratePython(Path input, Path output) throws Exception {
        var process = new ProcessBuilder(List.of(
                ProcessHandle.current().info().command().orElseThrow(),
                "-jar", System.getProperty("capy.jar.path"),
                "compile-generate", "python",
                "--input", input.toString(),
                "--output", output.toString()
        )).redirectErrorStream(true).start();
        var diagnostic = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8);

        assertThat(process.waitFor()).describedAs(diagnostic).isZero();
    }
}
