package dev.capylang.compiler;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.assertj.core.api.Assertions.assertThat;

class NativeImplementationScannerTest {
    @TempDir
    Path tempDir;

    @Test
    void shouldScanBackendNativeImplementationAnnotations() throws IOException {
        var capybaraDir = Files.createDirectories(tempDir.resolve("capybara"));
        var javaDir = Files.createDirectories(tempDir.resolve("java").resolve("dev").resolve("example").resolve("nativeinterop"));
        var jsDir = Files.createDirectories(tempDir.resolve("js").resolve("dev").resolve("example").resolve("nativeinterop"));
        var pyDir = Files.createDirectories(tempDir.resolve("py").resolve("dev").resolve("example").resolve("nativeinterop"));

        Files.writeString(javaDir.resolve("SystemClock.java"), """
                package dev.example.nativeinterop;

                import dev.capylang.NativeImplementation;
                import dev.example.Clock;

                @NativeImplementation(qualifier = "system")
                public final class SystemClock implements Clock {
                }
                """);
        Files.writeString(jsDir.resolve("SystemClock.js"), """
                'use strict';

                const { Clock } = require('../Clock.js');

                @NativeImplementation("system")
                class SystemClock extends Clock {
                }

                module.exports = { SystemClock };
                """);
        Files.writeString(pyDir.resolve("SystemClock.py"), """
                from dev.capylang.capybara import NativeImplementation
                from dev.example.Clock import Clock

                @NativeImplementation(qualifier="system")
                class SystemClock(Clock):
                    pass
                """);

        var manifest = NativeImplementationScanner.scan(capybaraDir);

        assertThat(manifest.providers())
                .singleElement()
                .satisfies(binding -> {
                    assertThat(binding.interfaceId()).isEqualTo("/dev/example/Clock");
                    assertThat(binding.qualifier()).isEqualTo("system");
                    assertThat(binding.javaBinding().className()).isEqualTo("dev.example.nativeinterop.SystemClock");
                    assertThat(binding.javaBinding().factory()).isEqualTo("constructor");
                    assertThat(binding.javascriptBinding().moduleName()).isEqualTo("../example/nativeinterop/SystemClock.js");
                    assertThat(binding.javascriptBinding().exportName()).isEqualTo("SystemClock");
                    assertThat(binding.javascriptBinding().factory()).isEqualTo("new");
                    assertThat(binding.pythonBinding().moduleName()).isEqualTo("dev.example.nativeinterop.SystemClock");
                    assertThat(binding.pythonBinding().className()).isEqualTo("SystemClock");
                    assertThat(binding.pythonBinding().factory()).isEqualTo("call");
                });
    }
}
