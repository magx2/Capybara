package dev.capylang.cli;

import org.junit.jupiter.api.Test;

import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

class CapyJarAdocReportIntegrationTest {
    @Test
    void shouldPackageAdocTestReportSupport() throws Exception {
        var jarPath = Path.of(System.getProperty("capy.jar.path"));
        assertThat(jarPath).isRegularFile();

        try (var classLoader = new URLClassLoader(
                new java.net.URL[]{jarPath.toUri().toURL()},
                ClassLoader.getPlatformClassLoader()
        )) {
            var runnerClass = Class.forName("dev.capylang.test.TestRunner", true, classLoader);
            var arguments = runnerClass.getMethod("parseArguments", String[].class).invoke(
                    null,
                    (Object) new String[]{
                            "--output-dir", Files.createTempDirectory("capy-adoc-report").toString(),
                            "--report-type", "ADOC"
                    }
            );
            assertThat(arguments.getClass().getMethod("reportType").invoke(arguments).toString())
                    .isEqualTo("ADOC");

            var adocReport = runnerClass.getDeclaredMethod("adocReport", Object.class);
            adocReport.setAccessible(true);
            var report = adocReport.invoke(null, Map.of(
                    "__type", "TestFile",
                    "file_name", "/example/ExampleTest.cfun",
                    "test_cases", List.of(
                            Map.of(
                                    "__type", "TestCase",
                                    "name", "renders | AsciiDoc",
                                    "result", Map.of("__type", "Passed"),
                                    "assertions_count", 2,
                                    "execution_time", 0.01
                            ),
                            Map.of(
                                    "__type", "TestCase",
                                    "name", "reports failures",
                                    "result", Map.of(
                                            "__type", "Failed",
                                            "message", "expected true\\nbut was false",
                                            "type", "assertion"
                                    ),
                                    "assertions_count", 1,
                                    "execution_time", 0.02
                            )
                    ),
                    "timestamp_millis", 0L
            ));
            assertThat(report.toString())
                    .contains("= Test results: /example/ExampleTest.cfun")
                    .contains("|renders \\| AsciiDoc")
                    .contains("|PASS")
                    .contains("|FAIL")
                    .contains("*Tests:* 2")
                    .contains("*Failed:* 1")
                    .contains("== Failures")
                    .contains("expected true\nbut was false");
        }
    }
}
