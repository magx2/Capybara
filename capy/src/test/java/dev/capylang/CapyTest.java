package dev.capylang;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.zip.ZipFile;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CapyTest {
    @TempDir
    Path tempDir;

    @Test
    void shouldPrintVersion() {
        var stdout = new ByteArrayOutputStream();
        var stderr = new ByteArrayOutputStream();

        var exitCode = Capy.execute(new String[]{"--version"}, new PrintStream(stdout), new PrintStream(stderr));

        assertEquals(0, exitCode);
        assertEquals("", stderr.toString().trim());
        assertEquals("Capybara compiler version: " + Capy.readCompilerVersion(), stdout.toString().trim());
    }

    @Test
    void shouldPrintHelpWithVersionFirst() {
        var stdout = new ByteArrayOutputStream();
        var stderr = new ByteArrayOutputStream();

        var exitCode = Capy.execute(new String[]{"--help"}, new PrintStream(stdout), new PrintStream(stderr));

        assertEquals(0, exitCode);
        assertEquals("", stderr.toString().trim());
        var text = stdout.toString();
        assertTrue(text.startsWith("Capybara compiler version: " + Capy.readCompilerVersion()));
        assertTrue(text.contains("capy compile"));
        assertTrue(text.contains("capy generate"));
        assertTrue(text.contains("capy package"));
    }

    @Test
    void shouldCompileUsingLibraries() throws IOException {
        var libSourceDir = Files.createDirectories(tempDir.resolve("lib-src"));
        Files.createDirectories(libSourceDir.resolve("lib"));
        Files.writeString(libSourceDir.resolve("lib").resolve("Lib.cfun"), "fun value(): int = 41\n");
        var libOutputDir = Files.createDirectories(tempDir.resolve("lib-linked"));

        var compileLibs = Capy.execute(
                new String[]{"compile", "-i", libSourceDir.toString(), "-o", libOutputDir.toString(), "--log", "debug"},
                new PrintStream(new ByteArrayOutputStream()),
                new PrintStream(new ByteArrayOutputStream())
        );
        assertEquals(0, compileLibs);

        var appSourceDir = Files.createDirectories(tempDir.resolve("app-src"));
        Files.createDirectories(appSourceDir.resolve("app"));
        Files.writeString(
                appSourceDir.resolve("app").resolve("Main.cfun"),
                "from /lib/Lib import { value }\nfun main(): int = value()\n"
        );
        var appOutputDir = Files.createDirectories(tempDir.resolve("app-linked"));

        var compileApp = Capy.execute(
                new String[]{
                        "compile",
                        "-i", appSourceDir.toString(),
                        "-l", libOutputDir.toString(),
                        "-o", appOutputDir.toString()
                },
                new PrintStream(new ByteArrayOutputStream()),
                new PrintStream(new ByteArrayOutputStream())
        );

        assertEquals(0, compileApp);
        assertTrue(Files.exists(appOutputDir.resolve("app").resolve("Main.json")));
        assertTrue(Files.exists(appOutputDir.resolve("build-info.json")));
        assertFalse(Files.exists(appOutputDir.resolve("lib").resolve("Lib.json")));
    }

    @Test
    void shouldReturnNonZeroAndLogMessageForInvalidCompileOutputDirectory() throws IOException {
        var sourceDir = Files.createDirectories(tempDir.resolve("source"));
        Files.writeString(sourceDir.resolve("Main.cfun"), "fun main(): int = 1\n");
        var outputDir = Files.createDirectories(tempDir.resolve("linked"));
        Files.writeString(outputDir.resolve("existing.txt"), "occupied");
        var stdout = new ByteArrayOutputStream();
        var stderr = new ByteArrayOutputStream();

        var exitCode = Capy.execute(
                new String[]{"compile", "-i", sourceDir.toString(), "-o", outputDir.toString()},
                new PrintStream(stdout),
                new PrintStream(stderr)
        );

        assertEquals(1, exitCode);
        assertEquals("", stdout.toString().trim());
        assertFalse(stderr.toString().trim().isEmpty());
    }

    @Test
    void shouldAcceptJsAliasForGenerate() throws IOException {
        var sourceDir = Files.createDirectories(tempDir.resolve("source"));
        Files.writeString(sourceDir.resolve("Main.cfun"), "fun main(): int = 1\n");
        var linkedDir = Files.createDirectories(tempDir.resolve("linked"));
        var generatedDir = tempDir.resolve("generated");

        var compileExit = Capy.execute(
                new String[]{"compile", "-i", sourceDir.toString(), "-o", linkedDir.toString()},
                new PrintStream(new ByteArrayOutputStream()),
                new PrintStream(new ByteArrayOutputStream())
        );
        assertEquals(0, compileExit);

        var generateExit = Capy.execute(
                new String[]{"generate", "Js", "-i", linkedDir.toString(), "-o", generatedDir.toString()},
                new PrintStream(new ByteArrayOutputStream()),
                new PrintStream(new ByteArrayOutputStream())
        );

        assertEquals(0, generateExit);
    }

    @Test
    void shouldPackageCompiledInputAndOverrideMetadata() throws IOException {
        var sourceDir = Files.createDirectories(tempDir.resolve("source"));
        Files.writeString(sourceDir.resolve("Main.cfun"), "fun main(): int = 1\n");
        var linkedDir = Files.createDirectories(tempDir.resolve("linked"));
        assertEquals(0, Capy.execute(
                new String[]{"compile", "-i", sourceDir.toString(), "-o", linkedDir.toString()},
                new PrintStream(new ByteArrayOutputStream()),
                new PrintStream(new ByteArrayOutputStream())
        ));

        var moduleDir = Files.createDirectories(tempDir.resolve("module"));
        var moduleFile = moduleDir.resolve("capy.yml");
        Files.writeString(moduleFile, """
                version: 1.2.3
                authors:
                  - name: Jane Doe
                    email: jane@example.com
                scm: https://example.com/repo.git
                license: Apache-2.0
                """);

        var exitCode = Capy.execute(
                new String[]{
                        "package",
                        "-ci", linkedDir.toString(),
                        "-m", moduleFile.toString(),
                        "--capy.license", "GPLv2"
                },
                new PrintStream(new ByteArrayOutputStream()),
                new PrintStream(new ByteArrayOutputStream())
        );

        assertEquals(0, exitCode);
        var archive = moduleDir.resolve("capy.cbin");
        assertTrue(Files.exists(archive));
        try (var zip = new ZipFile(archive.toFile())) {
            assertTrue(zip.getEntry("Main.json") != null);
            assertTrue(zip.getEntry("build-info.json") != null);
            var moduleYaml = new String(zip.getInputStream(zip.getEntry("capy.yml")).readAllBytes());
            assertTrue(moduleYaml.contains("version: 1.2.3"));
            assertTrue(moduleYaml.contains("license: GPLv2"));
            assertTrue(moduleYaml.contains("capybara_compiler_version: '" + Capy.readCompilerVersion() + "'")
                    || moduleYaml.contains("capybara_compiler_version: " + Capy.readCompilerVersion()));
            assertTrue(moduleYaml.contains("build_date_time:"));
            assertTrue(moduleYaml.contains("os:"));
        }
    }

    @Test
    void shouldCompileInputBeforePackaging() throws IOException {
        var sourceDir = Files.createDirectories(tempDir.resolve("package-src"));
        Files.createDirectories(sourceDir.resolve("foo"));
        Files.writeString(sourceDir.resolve("foo").resolve("Main.cfun"), "fun main(): int = 1\n");

        var moduleDir = Files.createDirectories(tempDir.resolve("module-inline"));
        var moduleFile = moduleDir.resolve("capy.yml");
        Files.writeString(moduleFile, "version: 1.0.0\nlicense: MIT\n");

        var exitCode = Capy.execute(
                new String[]{
                        "package",
                        "-i", sourceDir.toString(),
                        "-m", moduleFile.toString(),
                        "--log", "info"
                },
                new PrintStream(new ByteArrayOutputStream()),
                new PrintStream(new ByteArrayOutputStream())
        );

        assertEquals(0, exitCode);
        var archive = moduleDir.resolve("capy.cbin");
        assertTrue(Files.exists(archive));
        try (var zip = new ZipFile(archive.toFile())) {
            assertTrue(zip.getEntry("foo/Main.json") != null);
            assertTrue(zip.getEntry("capy.yml") != null);
        }
    }
}
