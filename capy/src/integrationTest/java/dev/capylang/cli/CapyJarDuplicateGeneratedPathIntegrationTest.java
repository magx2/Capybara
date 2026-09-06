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
        var nativePython = Files.createDirectories(tempDir.resolve("input/native/py"));
        Files.writeString(nativePython.resolve("SystemClock.py"), """
                from dev.capylang.capybara import NativeImplementation
                from dev.capylang.test.Clock import Clock

                @NativeImplementation(qualifier="system")
                class SystemClock(Clock):
                    def now_millis(self):
                        return 0
                """);

        for (var attempt = 0; attempt < 3; attempt++) {
            var output = tempDir.resolve("output-" + attempt);
            compileGeneratePython(tempDir.resolve("input"), output);

            assertThat(output.resolve("dev/capylang/test/Clock.py"))
                    .content()
                    .contains("class NativeProviderDomain:");
        }
    }

    @Test
    void rejectsNativeProviderWithoutSelectedBackendBindingBeforeGeneration() throws Exception {
        var input = Files.createDirectories(tempDir.resolve("unwired-input/paper_soccer/ui"));
        Files.writeString(input.resolve("UI.coo"), """
                interface UI {
                    def draw(): String
                }
                """);
        Files.writeString(input.resolve("UIProvider.cfun"), """
                from /capy/lang/Effect import { Effect }
                from /capy/meta_prog/NativeProvider import { NativeProvider }
                from UI import { UI }

                @NativeProvider
                fun ui(): Effect[UI] = <native>
                """);
        var output = tempDir.resolve("unwired-output");
        var process = new ProcessBuilder(List.of(
                ProcessHandle.current().info().command().orElseThrow(),
                "-jar", System.getProperty("capy.jar.path"),
                "compile-generate", "java",
                "--input", tempDir.resolve("unwired-input").toString(),
                "--output", output.toString()
        )).redirectErrorStream(true).start();

        var diagnostic = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8);

        assertThat(process.waitFor()).describedAs(diagnostic).isNotZero();
        assertThat(diagnostic).contains(
                "NotWired: No native provider registered for interface `paper_soccer.ui.UI` "
                        + "with qualifier `` for backend `java` (provider `ui`)."
        );
        assertThat(output.resolve("paper_soccer/ui/UIProvider.java")).doesNotExist();
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
