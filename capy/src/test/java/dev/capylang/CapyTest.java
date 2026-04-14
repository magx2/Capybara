package dev.capylang;

import dev.capylang.compiler.OutputType;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.TreeSet;
import java.util.zip.ZipFile;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CapyTest {
    @TempDir
    Path tempDir;

    @Test
    void shouldPrintVersion() {
        var stdout = new ByteArrayOutputStream();
        var stderr = new ByteArrayOutputStream();

        var exitCode = Capy.execute(new String[]{"--version"}, new PrintStream(stdout), new PrintStream(stderr));

        assertEquals(0, exitCode, stderr.toString());
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
        assertTrue(text.contains("capy compile-generate"));
        assertTrue(text.contains("capy generate"));
        assertTrue(text.contains("--compile-tests"));
        assertTrue(text.contains("--test-input <dir> --test-output <dir>"));
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
        assertTrue(Files.exists(appOutputDir.resolve("program.json")));
        assertFalse(Files.exists(appOutputDir.resolve("lib").resolve("Lib.json")));
    }

    @Test
    void shouldIgnoreNonCapybaraFilesDuringCompilation() throws IOException {
        var sourceDir = Files.createDirectories(tempDir.resolve("mixed-source-input"));
        Files.createDirectories(sourceDir.resolve("foo"));
        Files.writeString(sourceDir.resolve("foo").resolve("Main.cfun"), "fun main(): int = 1\n");
        Files.writeString(sourceDir.resolve("foo").resolve("README.md"), "# ignored\n");
        Files.writeString(sourceDir.resolve("foo").resolve("notes.txt"), "ignored\n");
        var outputDir = Files.createDirectories(tempDir.resolve("mixed-source-output"));

        assertEquals(0, Capy.execute(
                new String[]{"compile", "-i", sourceDir.toString(), "-o", outputDir.toString()},
                new PrintStream(new ByteArrayOutputStream()),
                new PrintStream(new ByteArrayOutputStream())
        ));

        assertTrue(Files.exists(outputDir.resolve("foo").resolve("Main.json")));
        assertFalse(Files.exists(outputDir.resolve("foo").resolve("README.json")));
        assertFalse(Files.exists(outputDir.resolve("foo").resolve("notes.json")));
    }

    @Test
    void shouldCompileGenerateObjectOrientedFilesToJava() throws IOException {
        var sourceDir = Files.createDirectories(tempDir.resolve("oo-source-input"));
        Files.createDirectories(sourceDir.resolve("foo"));
        Files.writeString(sourceDir.resolve("foo").resolve("Main.coo"), """
                class Main(name: string) {
                    field name: string = name

                    def greet(): string = "hello " + this.name
                }
                """);
        var generatedDir = tempDir.resolve("oo-source-generated");
        var linkedDir = tempDir.resolve("oo-source-linked");
        var stderr = new ByteArrayOutputStream();

        var exitCode = Capy.compileGenerate(
                OutputType.JAVA,
                sourceDir,
                generatedDir,
                linkedDir,
                null,
                null,
                new TreeSet<>(),
                false,
                false,
                "test-version",
                new PrintStream(stderr)
        );

        assertEquals(0, exitCode);
        assertEquals("", stderr.toString());
        assertTrue(Files.exists(generatedDir.resolve("foo").resolve("Main.java")));
        assertTrue(Files.readString(generatedDir.resolve("foo").resolve("Main.java")).contains("hello "));
        assertTrue(Files.exists(linkedDir.resolve("program.json")));
    }

    @Test
    void shouldCompileGenerateMixedFunctionalAndObjectOrientedFiles() throws IOException {
        var sourceDir = Files.createDirectories(tempDir.resolve("mixed-oo-functional-input"));
        Files.createDirectories(sourceDir.resolve("foo"));
        Files.writeString(sourceDir.resolve("foo").resolve("Main.cfun"), "fun main(): int = 1\n");
        Files.writeString(sourceDir.resolve("foo").resolve("User.coo"), """
                class User(name: string) {
                    field name: string = name
                    def greet(): string = "hi " + this.name
                }
                """);
        var generatedDir = tempDir.resolve("mixed-oo-functional-generated");
        var linkedDir = tempDir.resolve("mixed-oo-functional-linked");
        var stderr = new ByteArrayOutputStream();

        var exitCode = Capy.compileGenerate(
                OutputType.JAVA,
                sourceDir,
                generatedDir,
                linkedDir,
                null,
                null,
                new TreeSet<>(),
                false,
                false,
                "test-version",
                new PrintStream(stderr)
        );

        assertEquals(0, exitCode);
        assertEquals("", stderr.toString());
        assertTrue(Files.exists(generatedDir.resolve("foo").resolve("Main.java")));
        assertTrue(Files.exists(generatedDir.resolve("foo").resolve("User.java")));
    }

    @Test
    void shouldCompileGenerateWithProvidedCompilerVersion() throws IOException {
        var sourceDir = Files.createDirectories(tempDir.resolve("compile-generate-direct-source"));
        Files.createDirectories(sourceDir.resolve("foo"));
        Files.writeString(sourceDir.resolve("foo").resolve("Main.cfun"), "fun main(): int = 1\n");
        var generatedDir = tempDir.resolve("compile-generate-direct-output");
        var linkedDir = tempDir.resolve("compile-generate-direct-linked");

        var exitCode = Capy.compileGenerate(
                OutputType.JAVA,
                sourceDir,
                generatedDir,
                linkedDir,
                null,
                null,
                new TreeSet<>(),
                false,
                false,
                "test-version",
                new PrintStream(new ByteArrayOutputStream())
        );

        assertEquals(0, exitCode);
        assertTrue(Files.exists(generatedDir.resolve("foo").resolve("Main.java")));
        assertTrue(Files.readString(linkedDir.resolve("build-info.json")).contains("test-version"));
    }

    @Test
    void shouldReturnNonZeroAndLogMessageForCompileOutputPathThatIsNotDirectory() throws IOException {
        var sourceDir = Files.createDirectories(tempDir.resolve("source"));
        Files.writeString(sourceDir.resolve("Main.cfun"), "fun main(): int = 1\n");
        var outputDir = tempDir.resolve("linked.txt");
        Files.writeString(outputDir, "occupied");
        var stdout = new ByteArrayOutputStream();
        var stderr = new ByteArrayOutputStream();

        var exitCode = Capy.execute(
                new String[]{"compile", "-i", sourceDir.toString(), "-o", outputDir.toString()},
                new PrintStream(stdout),
                new PrintStream(stderr)
        );

        assertNotEquals(0, exitCode);
        assertEquals("", stdout.toString().trim());
        assertFalse(stderr.toString().trim().isEmpty());
    }

    @Test
    void shouldPruneStaleFilesFromReusedLinkedOutputDirectory() throws IOException {
        var sourceDir = Files.createDirectories(tempDir.resolve("reused-linked-source"));
        Files.createDirectories(sourceDir.resolve("foo"));
        Files.writeString(sourceDir.resolve("foo").resolve("Main.cfun"), "fun main(): int = 1\n");
        var linkedDir = Files.createDirectories(tempDir.resolve("reused-linked-output"));
        var staleFile = Files.createDirectories(linkedDir.resolve("stale")).resolve("Old.json");
        Files.writeString(staleFile, "{}");

        assertEquals(0, Capy.execute(
                new String[]{"compile", "-i", sourceDir.toString(), "-o", linkedDir.toString()},
                new PrintStream(new ByteArrayOutputStream()),
                new PrintStream(new ByteArrayOutputStream())
        ));

        assertTrue(Files.exists(linkedDir.resolve("foo").resolve("Main.json")));
        assertTrue(Files.exists(linkedDir.resolve("program.json")));
        assertTrue(Files.exists(linkedDir.resolve(".capy-output-manifest")));
        assertFalse(Files.exists(staleFile));
    }

    @Test
    void shouldPruneRemovedModulesFromManifestTrackedLinkedOutputDirectory() throws Exception {
        var sourceDir = Files.createDirectories(tempDir.resolve("manifest-linked-source"));
        Files.createDirectories(sourceDir.resolve("foo"));
        Files.writeString(sourceDir.resolve("foo").resolve("Main.cfun"), "fun main(): int = 1\n");
        Files.writeString(sourceDir.resolve("foo").resolve("Extra.cfun"), "fun extra(): int = 2\n");
        var linkedDir = Files.createDirectories(tempDir.resolve("manifest-linked-output"));

        assertEquals(0, Capy.execute(
                new String[]{"compile", "-i", sourceDir.toString(), "-o", linkedDir.toString()},
                new PrintStream(new ByteArrayOutputStream()),
                new PrintStream(new ByteArrayOutputStream())
        ));

        var removedModule = linkedDir.resolve("foo").resolve("Extra.json");
        assertTrue(Files.exists(removedModule));

        Files.delete(sourceDir.resolve("foo").resolve("Extra.cfun"));
        Thread.sleep(1100);

        assertEquals(0, Capy.execute(
                new String[]{"compile", "-i", sourceDir.toString(), "-o", linkedDir.toString()},
                new PrintStream(new ByteArrayOutputStream()),
                new PrintStream(new ByteArrayOutputStream())
        ));

        assertFalse(Files.exists(removedModule));
        assertTrue(Files.readString(linkedDir.resolve(".capy-output-manifest")).contains("foo/Main.json"));
        assertFalse(Files.readString(linkedDir.resolve(".capy-output-manifest")).contains("foo/Extra.json"));
    }

    @Test
    void shouldReadLibrariesFromAggregatedProgramFile() throws IOException {
        var libSourceDir = Files.createDirectories(tempDir.resolve("aggregated-lib-src"));
        Files.createDirectories(libSourceDir.resolve("lib"));
        Files.writeString(libSourceDir.resolve("lib").resolve("Lib.cfun"), "fun value(): int = 41\n");
        var libOutputDir = Files.createDirectories(tempDir.resolve("aggregated-lib-linked"));

        assertEquals(0, Capy.execute(
                new String[]{"compile", "-i", libSourceDir.toString(), "-o", libOutputDir.toString()},
                new PrintStream(new ByteArrayOutputStream()),
                new PrintStream(new ByteArrayOutputStream())
        ));

        assertTrue(Files.exists(libOutputDir.resolve("program.json")));
        Files.delete(libOutputDir.resolve("lib").resolve("Lib.json"));

        var appSourceDir = Files.createDirectories(tempDir.resolve("aggregated-app-src"));
        Files.createDirectories(appSourceDir.resolve("app"));
        Files.writeString(
                appSourceDir.resolve("app").resolve("Main.cfun"),
                "from /lib/Lib import { value }\nfun main(): int = value()\n"
        );
        var appOutputDir = Files.createDirectories(tempDir.resolve("aggregated-app-linked"));

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
    }

    @Test
    void shouldNotRewriteIdenticalLinkedOutputs() throws Exception {
        var sourceDir = Files.createDirectories(tempDir.resolve("stable-linked-source"));
        Files.createDirectories(sourceDir.resolve("foo"));
        Files.writeString(sourceDir.resolve("foo").resolve("Main.cfun"), "fun main(): int = 1\n");
        var linkedDir = Files.createDirectories(tempDir.resolve("stable-linked-output"));

        assertEquals(0, Capy.execute(
                new String[]{"compile", "-i", sourceDir.toString(), "-o", linkedDir.toString()},
                new PrintStream(new ByteArrayOutputStream()),
                new PrintStream(new ByteArrayOutputStream())
        ));

        var programFile = linkedDir.resolve("program.json");
        var moduleFile = linkedDir.resolve("foo").resolve("Main.json");
        var buildInfoFile = linkedDir.resolve("build-info.json");
        var manifestFile = linkedDir.resolve(".capy-output-manifest");
        var initialProgramTime = Files.getLastModifiedTime(programFile);
        var initialModuleTime = Files.getLastModifiedTime(moduleFile);
        var initialBuildInfoTime = Files.getLastModifiedTime(buildInfoFile);
        var initialManifestTime = Files.getLastModifiedTime(manifestFile);

        Thread.sleep(1100);

        assertEquals(0, Capy.execute(
                new String[]{"compile", "-i", sourceDir.toString(), "-o", linkedDir.toString()},
                new PrintStream(new ByteArrayOutputStream()),
                new PrintStream(new ByteArrayOutputStream())
        ));

        assertEquals(initialProgramTime, Files.getLastModifiedTime(programFile));
        assertEquals(initialModuleTime, Files.getLastModifiedTime(moduleFile));
        assertEquals(initialBuildInfoTime, Files.getLastModifiedTime(buildInfoFile));
        assertEquals(initialManifestTime, Files.getLastModifiedTime(manifestFile));
    }

    @Test
    void shouldRewriteChangedLinkedOutputsWhenFileSizeChanges() throws Exception {
        var sourceDir = Files.createDirectories(tempDir.resolve("changed-linked-source"));
        Files.createDirectories(sourceDir.resolve("foo"));
        var sourceFile = sourceDir.resolve("foo").resolve("Main.cfun");
        Files.writeString(sourceFile, "fun main(): int = 1\n");
        var linkedDir = Files.createDirectories(tempDir.resolve("changed-linked-output"));

        assertEquals(0, Capy.execute(
                new String[]{"compile", "-i", sourceDir.toString(), "-o", linkedDir.toString()},
                new PrintStream(new ByteArrayOutputStream()),
                new PrintStream(new ByteArrayOutputStream())
        ));

        var moduleFile = linkedDir.resolve("foo").resolve("Main.json");
        var initialModifiedTime = Files.getLastModifiedTime(moduleFile);

        Thread.sleep(1100);
        Files.writeString(sourceFile, "fun main(): int = 1000\n");

        assertEquals(0, Capy.execute(
                new String[]{"compile", "-i", sourceDir.toString(), "-o", linkedDir.toString()},
                new PrintStream(new ByteArrayOutputStream()),
                new PrintStream(new ByteArrayOutputStream())
        ));

        assertTrue(Files.readString(moduleFile).contains("\"1000\""));
        assertTrue(Files.getLastModifiedTime(moduleFile).compareTo(initialModifiedTime) > 0);
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
    void shouldCopyBundledJavaLibSourcesByDefault() throws IOException {
        var sourceDir = Files.createDirectories(tempDir.resolve("default-generate-source"));
        Files.writeString(sourceDir.resolve("Main.cfun"), "fun main(): int = 1\n");
        var linkedDir = Files.createDirectories(tempDir.resolve("default-generate-linked"));
        var generatedDir = tempDir.resolve("default-generate-output");

        assertEquals(0, Capy.execute(
                new String[]{"compile", "-i", sourceDir.toString(), "-o", linkedDir.toString()},
                new PrintStream(new ByteArrayOutputStream()),
                new PrintStream(new ByteArrayOutputStream())
        ));

        assertEquals(0, Capy.execute(
                new String[]{"generate", "java", "-i", linkedDir.toString(), "-o", generatedDir.toString()},
                new PrintStream(new ByteArrayOutputStream()),
                new PrintStream(new ByteArrayOutputStream())
        ));

        assertTrue(Files.exists(generatedDir.resolve("dev").resolve("capylang").resolve("CapybaraUtil.java")));
    }

    @Test
    void shouldSkipBundledJavaLibSourcesWhenRequested() throws IOException {
        var sourceDir = Files.createDirectories(tempDir.resolve("skip-java-lib-source"));
        Files.writeString(sourceDir.resolve("Main.cfun"), "fun main(): int = 1\n");
        var linkedDir = Files.createDirectories(tempDir.resolve("skip-java-lib-linked"));
        var generatedDir = tempDir.resolve("skip-java-lib-output");

        assertEquals(0, Capy.execute(
                new String[]{"compile", "-i", sourceDir.toString(), "-o", linkedDir.toString()},
                new PrintStream(new ByteArrayOutputStream()),
                new PrintStream(new ByteArrayOutputStream())
        ));

        assertEquals(0, Capy.execute(
                new String[]{"generate", "java", "--skip-java-lib", "-i", linkedDir.toString(), "-o", generatedDir.toString()},
                new PrintStream(new ByteArrayOutputStream()),
                new PrintStream(new ByteArrayOutputStream())
        ));

        assertFalse(Files.exists(generatedDir.resolve("dev").resolve("capylang").resolve("CapybaraUtil.java")));
    }

    @Test
    void shouldPruneStaleFilesFromReusedGeneratedOutputDirectory() throws IOException {
        var sourceDir = Files.createDirectories(tempDir.resolve("reused-generate-source"));
        Files.writeString(sourceDir.resolve("Main.cfun"), "fun main(): int = 1\n");
        var linkedDir = Files.createDirectories(tempDir.resolve("reused-generate-linked"));
        var generatedDir = Files.createDirectories(tempDir.resolve("reused-generate-output"));
        var staleFile = Files.createDirectories(generatedDir.resolve("stale")).resolve("Old.java");
        Files.writeString(staleFile, "class Old {}");

        assertEquals(0, Capy.execute(
                new String[]{"compile", "-i", sourceDir.toString(), "-o", linkedDir.toString()},
                new PrintStream(new ByteArrayOutputStream()),
                new PrintStream(new ByteArrayOutputStream())
        ));

        assertEquals(0, Capy.execute(
                new String[]{"generate", "java", "--skip-java-lib", "-i", linkedDir.toString(), "-o", generatedDir.toString()},
                new PrintStream(new ByteArrayOutputStream()),
                new PrintStream(new ByteArrayOutputStream())
        ));

        assertTrue(Files.exists(generatedDir.resolve("Main.java")));
        assertTrue(Files.exists(generatedDir.resolve(".capy-output-manifest")));
        assertFalse(Files.exists(staleFile));
        assertFalse(Files.exists(generatedDir.resolve("stale")));
    }

    @Test
    void shouldNotRewriteIdenticalGeneratedOutputs() throws Exception {
        var sourceDir = Files.createDirectories(tempDir.resolve("stable-generate-source"));
        Files.writeString(sourceDir.resolve("Main.cfun"), "fun main(): int = 1\n");
        var linkedDir = Files.createDirectories(tempDir.resolve("stable-generate-linked"));
        var generatedDir = Files.createDirectories(tempDir.resolve("stable-generate-output"));

        assertEquals(0, Capy.execute(
                new String[]{"compile", "-i", sourceDir.toString(), "-o", linkedDir.toString()},
                new PrintStream(new ByteArrayOutputStream()),
                new PrintStream(new ByteArrayOutputStream())
        ));

        assertEquals(0, Capy.execute(
                new String[]{"generate", "java", "-i", linkedDir.toString(), "-o", generatedDir.toString()},
                new PrintStream(new ByteArrayOutputStream()),
                new PrintStream(new ByteArrayOutputStream())
        ));

        var mainFile = generatedDir.resolve("Main.java");
        var runtimeFile = generatedDir.resolve("dev").resolve("capylang").resolve("CapybaraUtil.java");
        var manifestFile = generatedDir.resolve(".capy-output-manifest");
        var initialMainTime = Files.getLastModifiedTime(mainFile);
        var initialRuntimeTime = Files.getLastModifiedTime(runtimeFile);
        var initialManifestTime = Files.getLastModifiedTime(manifestFile);

        Thread.sleep(1100);

        assertEquals(0, Capy.execute(
                new String[]{"generate", "java", "-i", linkedDir.toString(), "-o", generatedDir.toString()},
                new PrintStream(new ByteArrayOutputStream()),
                new PrintStream(new ByteArrayOutputStream())
        ));

        assertEquals(initialMainTime, Files.getLastModifiedTime(mainFile));
        assertEquals(initialRuntimeTime, Files.getLastModifiedTime(runtimeFile));
        assertEquals(initialManifestTime, Files.getLastModifiedTime(manifestFile));
    }

    @Test
    void shouldCompileGenerateTestsUsingLibrariesWithoutLinkedIntermediates() throws IOException {
        var libSourceDir = Files.createDirectories(tempDir.resolve("compile-generate-lib-source"));
        Files.createDirectories(libSourceDir.resolve("foo"));
        Files.writeString(libSourceDir.resolve("foo").resolve("Lib.cfun"), """
                fun forty_two(): int = 42
                """);
        var libLinkedDir = Files.createDirectories(tempDir.resolve("compile-generate-lib-linked"));

        assertEquals(0, Capy.execute(
                new String[]{"compile", "-i", libSourceDir.toString(), "-o", libLinkedDir.toString()},
                new PrintStream(new ByteArrayOutputStream()),
                new PrintStream(new ByteArrayOutputStream())
        ));

        var testSourceDir = Files.createDirectories(tempDir.resolve("compile-generate-test-source"));
        Files.createDirectories(testSourceDir.resolve("bar"));
        Files.writeString(testSourceDir.resolve("bar").resolve("TestModule.cfun"), """
                from /capy/test/Assert import { * }
                from /capy/test/CapyTest import { * }
                from /foo/Lib import { forty_two }

                fun works(): Assert =
                    assert_that(forty_two()).is_equal_to(42)

                fun tests(): TestFile =
                    test_file("/bar/TestModule.cfun", [
                        test("works", works())
                    ])
                """);
        var generatedDir = tempDir.resolve("compile-generate-test-output");

        assertEquals(0, Capy.execute(
                new String[]{
                        "compile-generate",
                        "java",
                        "--skip-java-lib",
                        "--compile-tests",
                        "-i", testSourceDir.toString(),
                        "-l", libLinkedDir.toString(),
                        "-o", generatedDir.toString()
                },
                new PrintStream(new ByteArrayOutputStream()),
                new PrintStream(new ByteArrayOutputStream())
        ));

        assertTrue(Files.exists(generatedDir.resolve("bar").resolve("TestModule.java")));
        assertTrue(Files.exists(generatedDir.resolve("capy").resolve("test").resolve("CapyTestRuntime.java")));
        assertFalse(Files.exists(generatedDir.resolve("foo").resolve("Lib.json")));
        assertFalse(Files.exists(generatedDir.resolve("dev").resolve("capylang").resolve("CapybaraUtil.java")));
    }

    @Test
    void shouldCompileGenerateAndWriteLinkedOutputWhenRequested() throws IOException {
        var sourceDir = Files.createDirectories(tempDir.resolve("compile-generate-source"));
        Files.createDirectories(sourceDir.resolve("foo"));
        Files.writeString(sourceDir.resolve("foo").resolve("Main.cfun"), """
                fun main(): int = 42
                """);
        var generatedDir = tempDir.resolve("compile-generate-output");
        var linkedDir = Files.createDirectories(tempDir.resolve("compile-generate-linked"));

        assertEquals(0, Capy.execute(
                new String[]{
                        "compile-generate",
                        "java",
                        "--skip-java-lib",
                        "-i", sourceDir.toString(),
                        "-o", generatedDir.toString(),
                        "--linked-output", linkedDir.toString()
                },
                new PrintStream(new ByteArrayOutputStream()),
                new PrintStream(new ByteArrayOutputStream())
        ));

        assertTrue(Files.exists(generatedDir.resolve("foo").resolve("Main.java")));
        assertTrue(Files.exists(linkedDir.resolve("foo").resolve("Main.json")));
        assertTrue(Files.exists(linkedDir.resolve("build-info.json")));
        assertTrue(Files.exists(linkedDir.resolve("program.json")));
        assertFalse(Files.exists(generatedDir.resolve("dev").resolve("capylang").resolve("CapybaraUtil.java")));
    }

    @Test
    void shouldCompileGenerateMainAndTestsInOneCommand() throws IOException {
        var mainSourceDir = Files.createDirectories(tempDir.resolve("compile-generate-main-source"));
        Files.createDirectories(mainSourceDir.resolve("foo"));
        Files.writeString(mainSourceDir.resolve("foo").resolve("Lib.cfun"), """
                fun forty_two(): int = 42
                """);

        var testSourceDir = Files.createDirectories(tempDir.resolve("compile-generate-main-test-source"));
        Files.createDirectories(testSourceDir.resolve("bar"));
        Files.writeString(testSourceDir.resolve("bar").resolve("TestModule.cfun"), """
                from /capy/test/Assert import { * }
                from /capy/test/CapyTest import { * }
                from /foo/Lib import { forty_two }

                fun works(): Assert =
                    assert_that(forty_two()).is_equal_to(42)

                fun tests(): TestFile =
                    test_file("/bar/TestModule.cfun", [
                        test("works", works())
                    ])
                """);

        var generatedMainDir = tempDir.resolve("compile-generate-main-output");
        var linkedMainDir = Files.createDirectories(tempDir.resolve("compile-generate-main-linked"));
        var generatedTestDir = tempDir.resolve("compile-generate-main-test-output");

        assertEquals(0, Capy.execute(
                new String[]{
                        "compile-generate",
                        "java",
                        "--skip-java-lib",
                        "-i", mainSourceDir.toString(),
                        "-o", generatedMainDir.toString(),
                        "--linked-output", linkedMainDir.toString(),
                        "--test-input", testSourceDir.toString(),
                        "--test-output", generatedTestDir.toString()
                },
                new PrintStream(new ByteArrayOutputStream()),
                new PrintStream(new ByteArrayOutputStream())
        ));

        assertTrue(Files.exists(generatedMainDir.resolve("foo").resolve("Lib.java")));
        assertTrue(Files.exists(linkedMainDir.resolve("foo").resolve("Lib.json")));
        assertTrue(Files.exists(generatedTestDir.resolve("bar").resolve("TestModule.java")));
        assertTrue(Files.exists(generatedTestDir.resolve("capy").resolve("test").resolve("CapyTestRuntime.java")));
        assertFalse(Files.exists(generatedMainDir.resolve("dev").resolve("capylang").resolve("CapybaraUtil.java")));
        assertFalse(Files.exists(generatedTestDir.resolve("dev").resolve("capylang").resolve("CapybaraUtil.java")));
    }

    @Test
    void shouldCompileGenerateMainAndTestsIntoSharedOutputDirectory() throws IOException {
        var mainSourceDir = Files.createDirectories(tempDir.resolve("compile-generate-shared-main-source"));
        Files.createDirectories(mainSourceDir.resolve("foo"));
        Files.writeString(mainSourceDir.resolve("foo").resolve("Lib.cfun"), """
                fun forty_two(): int = 42
                """);

        var testSourceDir = Files.createDirectories(tempDir.resolve("compile-generate-shared-test-source"));
        Files.createDirectories(testSourceDir.resolve("bar"));
        Files.writeString(testSourceDir.resolve("bar").resolve("TestModule.cfun"), """
                from /capy/test/Assert import { * }
                from /capy/test/CapyTest import { * }
                from /foo/Lib import { forty_two }

                fun works(): Assert =
                    assert_that(forty_two()).is_equal_to(42)

                fun tests(): TestFile =
                    test_file("/bar/TestModule.cfun", [
                        test("works", works())
                    ])
                """);

        var generatedDir = tempDir.resolve("compile-generate-shared-output");

        assertEquals(0, Capy.execute(
                new String[]{
                        "compile-generate",
                        "java",
                        "--skip-java-lib",
                        "-i", mainSourceDir.toString(),
                        "-o", generatedDir.toString(),
                        "--test-input", testSourceDir.toString(),
                        "--test-output", generatedDir.toString()
                },
                new PrintStream(new ByteArrayOutputStream()),
                new PrintStream(new ByteArrayOutputStream())
        ));

        assertTrue(Files.exists(generatedDir.resolve("foo").resolve("Lib.java")));
        assertTrue(Files.exists(generatedDir.resolve("bar").resolve("TestModule.java")));
        assertTrue(Files.exists(generatedDir.resolve("capy").resolve("test").resolve("CapyTestRuntime.java")));
        assertFalse(Files.exists(generatedDir.resolve("dev").resolve("capylang").resolve("CapybaraUtil.java")));
    }

    @Test
    void shouldCompileTestsAndWriteCapyTestRuntimeModule() throws IOException {
        var sourceDir = Files.createDirectories(tempDir.resolve("test-source"));
        Files.createDirectories(sourceDir.resolve("foo"));
        Files.writeString(sourceDir.resolve("foo").resolve("TestModule.cfun"), """
                from /capy/test/Assert import { * }
                from /capy/test/CapyTest import { * }

                fun works(): Assert =
                    assert_that(1).is_equal_to(1)

                fun tests(): TestFile =
                    test_file("/foo/TestModule.cfun", [
                        test("works", works())
                    ])
                """);
        var linkedDir = Files.createDirectories(tempDir.resolve("test-linked"));

        assertEquals(0, Capy.execute(
                new String[]{"compile", "--compile-tests", "-i", sourceDir.toString(), "-o", linkedDir.toString()},
                new PrintStream(new ByteArrayOutputStream()),
                new PrintStream(new ByteArrayOutputStream())
        ));

        assertTrue(Files.exists(linkedDir.resolve("capy").resolve("test").resolve("CapyTestRuntime.json")));
        assertTrue(Files.exists(linkedDir.resolve("foo").resolve("TestModule.json")));
        assertTrue(Files.exists(linkedDir.resolve("program.json")));
    }

    @Test
    void shouldFailCompileTestsWhenNoTestsAreFound() throws IOException {
        var sourceDir = Files.createDirectories(tempDir.resolve("no-tests-source"));
        Files.writeString(sourceDir.resolve("Main.cfun"), "fun main(): int = 1\n");
        var linkedDir = Files.createDirectories(tempDir.resolve("no-tests-linked"));
        var stderr = new ByteArrayOutputStream();
        var exitCode = Capy.execute(
                new String[]{"compile", "--compile-tests", "-i", sourceDir.toString(), "-o", linkedDir.toString()},
                new PrintStream(new ByteArrayOutputStream()),
                new PrintStream(stderr)
        );

        assertNotEquals(0, exitCode);
        assertFalse(stderr.toString().isBlank());
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
            assertTrue(zip.getEntry("program.json") != null);
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
