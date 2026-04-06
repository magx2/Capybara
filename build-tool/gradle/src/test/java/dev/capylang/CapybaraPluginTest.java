package dev.capylang;

import org.gradle.api.Project;
import org.gradle.api.file.ConfigurableFileCollection;
import org.gradle.api.logging.LogLevel;
import org.gradle.api.plugins.JavaPlugin;
import org.gradle.api.tasks.SourceSetContainer;
import org.gradle.api.tasks.testing.Test;
import org.gradle.testfixtures.ProjectBuilder;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.params.provider.Arguments.arguments;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

class CapybaraPluginTest {
    @TempDir
    Path tempDir;

    @Test
    void shouldCompileAndGenerateMainSourcesInSingleTask() throws IOException {
        var project = newProject();
        var sourceDir = Files.createDirectories(tempDir.resolve("src/main/capybara/foo"));
        Files.writeString(sourceDir.resolve("Main.cfun"), "fun main(): int = 42\n");

        var task = project.getTasks().named("compileCapybara", CompileCapybaraTask.class).get();
        task.compile();

        assertTrue(project.file("build/classes/capybara/foo/Main.json").isFile());
        assertTrue(project.file("build/classes/capybara/build-info.json").isFile());
        assertTrue(project.file("build/classes/capybara/program.json").isFile());
        assertTrue(project.file("build/generated/sources/capybara/java/foo/Main.java").isFile());
        assertTrue(project.file("build/generated/sources/capybara/java/dev/capylang/CapybaraUtil.java").isFile());
    }

    @Test
    void shouldCompileMainAndTestSourcesInSingleTaskForCheckBuilds() throws IOException {
        var project = newProject(List.of("check"));

        var mainSourceDir = Files.createDirectories(tempDir.resolve("src/main/capybara/foo"));
        Files.writeString(mainSourceDir.resolve("Lib.cfun"), "fun forty_two(): int = 42\n");
        var testSourceDir = Files.createDirectories(tempDir.resolve("src/test/capybara/bar"));
        Files.writeString(testSourceDir.resolve("TestModule.cfun"), """
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

        project.getTasks().named("compileCapybara", CompileCapybaraTask.class).get().compile();

        assertTrue(project.file("build/generated/sources/capybara/java/check/foo/Lib.java").isFile());
        assertTrue(project.file("build/generated/sources/capybara/java/check/bar/TestModule.java").isFile());
        assertFalse(project.file("build/classes/capybara/foo/Lib.json").exists());
        assertFalse(project.file("build/classes/capybara/program.json").exists());
        assertFalse(project.file("build/classes/capybara/build-info.json").exists());
        assertFalse(project.file("build/generated/sources/capybara/java/check/dev/capylang/CapybaraUtil.java").exists());
        assertFalse(project.getTasks().named("compileTestCapybara", CompileCapybaraTask.class).get().getOnlyIf().isSatisfiedBy(
                project.getTasks().named("compileTestCapybara", CompileCapybaraTask.class).get()
        ));
    }

    @Test
    void shouldOnlyDeclareFusedTaskInputsAndOutputsForCapybaraOnlyCheckBuildsWithoutCapybaraTests() {
        var standardProject = newProject();
        var standardTask = standardProject.getTasks().named("compileCapybara", CompileCapybaraTask.class).get();
        var fusedProject = newProject(List.of("check"));
        var fusedTask = fusedProject.getTasks().named("compileCapybara", CompileCapybaraTask.class).get();

        assertTrue(standardTask.getOutputDir().isPresent());
        assertFalse(standardTask.getTestInputDir().isPresent());
        assertFalse(standardTask.getGeneratedTestOutputDir().isPresent());

        assertFalse(fusedTask.getOutputDir().isPresent());
        assertFalse(fusedTask.getTestInputDir().isPresent());
        assertFalse(fusedTask.getGeneratedTestOutputDir().isPresent());
    }

    @Test
    void shouldDeclareFusedTaskInputsAndOutputsForCheckBuildsWithCapybaraTests() throws IOException {
        var testSourceDir = Files.createDirectories(tempDir.resolve("src/test/capybara/bar"));
        Files.writeString(testSourceDir.resolve("TestModule.cfun"), """
                from /capy/test/Assert import { * }
                from /capy/test/CapyTest import { * }

                fun works(): Assert =
                    assert_that(42).is_equal_to(42)

                fun tests(): TestFile =
                    test_file("/bar/TestModule.cfun", [
                        test("works", works())
                    ])
                """);

        var project = newProject(List.of("check"));
        var fusedTask = project.getTasks().named("compileCapybara", CompileCapybaraTask.class).get();

        assertFalse(fusedTask.getOutputDir().isPresent());
        assertTrue(fusedTask.getTestInputDir().isPresent());
        assertTrue(fusedTask.getGeneratedTestOutputDir().isPresent());
        assertEquals(
                fusedTask.getGeneratedOutputDir().get().getAsFile(),
                fusedTask.getGeneratedTestOutputDir().get().getAsFile()
        );
    }

    @Test
    void shouldCompileTestSourcesWithoutCopyingBundledJavaLibAgain() throws IOException {
        var project = newProject();

        var mainSourceDir = Files.createDirectories(tempDir.resolve("src/main/capybara/foo"));
        Files.writeString(mainSourceDir.resolve("Lib.cfun"), "fun forty_two(): int = 42\n");
        project.getTasks().named("compileCapybara", CompileCapybaraTask.class).get().compile();

        var testSourceDir = Files.createDirectories(tempDir.resolve("src/test/capybara/bar"));
        Files.writeString(testSourceDir.resolve("TestModule.cfun"), """
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

        var task = project.getTasks().named("compileTestCapybara", CompileCapybaraTask.class).get();
        task.compile();

        assertTrue(project.file("build/generated/sources/test-capybara/java/bar/TestModule.java").isFile());
        assertTrue(project.file("build/generated/sources/test-capybara/java/capy/test/CapyTestRuntime.java").isFile());
        assertFalse(project.file("build/generated/sources/test-capybara/java/dev/capylang/CapybaraUtil.java").exists());
        assertFalse(project.file("build/classes/test-capybara/program.json").exists());
        assertFalse(project.file("build/classes/test-capybara/build-info.json").exists());
    }

    @Test
    void shouldKeepStandaloneTestGenerationPathWhenCompileTestCapybaraIsRequestedDirectly() throws IOException {
        var testSourceDir = Files.createDirectories(tempDir.resolve("src/test/capybara/bar"));
        Files.writeString(testSourceDir.resolve("TestModule.cfun"), """
                from /capy/test/Assert import { * }
                from /capy/test/CapyTest import { * }

                fun works(): Assert =
                    assert_that(42).is_equal_to(42)

                fun tests(): TestFile =
                    test_file("/bar/TestModule.cfun", [
                        test("works", works())
                    ])
                """);

        var project = newProject(List.of("compileTestCapybara"));
        var compileCapybara = project.getTasks().named("compileCapybara", CompileCapybaraTask.class).get();
        var compileTestCapybara = project.getTasks().named("compileTestCapybara", CompileCapybaraTask.class).get();

        assertTrue(compileCapybara.getOutputDir().isPresent());
        assertFalse(compileCapybara.getTestInputDir().isPresent());
        assertFalse(compileCapybara.getGeneratedTestOutputDir().isPresent());
        assertTrue(compileTestCapybara.getOnlyIf().isSatisfiedBy(compileTestCapybara));
        assertEquals(project.file("build/generated/sources/test-capybara/java"), compileTestCapybara.getGeneratedOutputDir().get().getAsFile());
    }

    @Test
    void shouldKeepStandaloneTestGenerationPathWhenGenerateTestCapybaraJavaIsRequestedDirectly() throws IOException {
        var testSourceDir = Files.createDirectories(tempDir.resolve("src/test/capybara/bar"));
        Files.writeString(testSourceDir.resolve("TestModule.cfun"), """
                from /capy/test/Assert import { * }
                from /capy/test/CapyTest import { * }

                fun works(): Assert =
                    assert_that(42).is_equal_to(42)

                fun tests(): TestFile =
                    test_file("/bar/TestModule.cfun", [
                        test("works", works())
                    ])
                """);

        var project = newProject(List.of("generateTestCapybaraJava"));
        var compileCapybara = project.getTasks().named("compileCapybara", CompileCapybaraTask.class).get();
        var compileTestCapybara = project.getTasks().named("compileTestCapybara", CompileCapybaraTask.class).get();

        assertTrue(compileCapybara.getOutputDir().isPresent());
        assertFalse(compileCapybara.getTestInputDir().isPresent());
        assertFalse(compileCapybara.getGeneratedTestOutputDir().isPresent());
        assertTrue(compileTestCapybara.getOnlyIf().isSatisfiedBy(compileTestCapybara));
        assertEquals(project.file("build/generated/sources/test-capybara/java"), compileTestCapybara.getGeneratedOutputDir().get().getAsFile());
    }

    @Test
    void shouldReadPluginLibraryInputsFromAggregatedProgramFile() throws IOException {
        var project = newProject();

        var mainSourceDir = Files.createDirectories(tempDir.resolve("src/main/capybara/foo"));
        Files.writeString(mainSourceDir.resolve("Lib.cfun"), "fun forty_two(): int = 42\n");
        project.getTasks().named("compileCapybara", CompileCapybaraTask.class).get().compile();

        Files.delete(project.file("build/classes/capybara/foo/Lib.json").toPath());

        var testSourceDir = Files.createDirectories(tempDir.resolve("src/test/capybara/bar"));
        Files.writeString(testSourceDir.resolve("TestModule.cfun"), """
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

        project.getTasks().named("compileTestCapybara", CompileCapybaraTask.class).get().compile();

        assertTrue(project.file("build/classes/capybara/program.json").isFile());
        assertTrue(project.file("build/generated/sources/test-capybara/java/bar/TestModule.java").isFile());
    }

    @Test
    void shouldDeclareOnlyAggregatedProgramFileAsPluginLibraryInput() {
        var project = newProject();
        var task = project.getTasks().named("compileTestCapybara", CompileCapybaraTask.class).get();
        var libraryProgramFiles = task.getLibraryProgramFiles().getFiles();

        assertEquals(1, libraryProgramFiles.size());
        assertTrue(libraryProgramFiles.contains(project.file("build/classes/capybara/program.json")));
    }

    @Test
    void shouldPruneStaleFilesWhenReusingTaskOutputs() throws IOException {
        var project = newProject();
        var sourceDir = Files.createDirectories(tempDir.resolve("src/main/capybara/foo"));
        Files.writeString(sourceDir.resolve("Main.cfun"), "fun main(): int = 42\n");

        var staleLinkedFile = Files.createDirectories(tempDir.resolve("build/classes/capybara/stale")).resolve("Old.json");
        Files.writeString(staleLinkedFile, "{}");
        var staleGeneratedFile = Files.createDirectories(tempDir.resolve("build/generated/sources/capybara/java/stale")).resolve("Old.java");
        Files.writeString(staleGeneratedFile, "class Old {}");

        project.getTasks().named("compileCapybara", CompileCapybaraTask.class).get().compile();

        assertFalse(Files.exists(staleLinkedFile));
        assertFalse(Files.exists(staleGeneratedFile));
        assertTrue(project.file("build/classes/capybara/foo/Main.json").isFile());
        assertTrue(project.file("build/generated/sources/capybara/java/foo/Main.java").isFile());
    }

    @Test
    void shouldNotAddMainGeneratedJavaToTestSourceSet() {
        var project = newProject();
        var sourceSets = project.getExtensions().getByType(SourceSetContainer.class);
        var testSrcDirs = sourceSets.getByName("test").getJava().getSrcDirs();

        assertTrue(testSrcDirs.contains(project.file("build/generated/sources/test-capybara/java")));
        assertFalse(testSrcDirs.contains(project.file("build/generated/sources/capybara/java")));
    }

    @Test
    void shouldAddMainGeneratedJavaToTestSourceSetForCheckBuildsWithoutJvmMainSources() {
        var project = newProject(List.of("check"));
        var sourceSets = project.getExtensions().getByType(SourceSetContainer.class);
        var testSrcDirs = sourceSets.getByName("test").getJava().getSrcDirs();

        assertTrue(testSrcDirs.contains(project.file("build/generated/sources/capybara/java/check")));
        assertFalse(testSrcDirs.contains(project.file("build/generated/sources/test-capybara/java")));
        assertFalse(testSrcDirs.contains(project.file("src/main/java")));
    }

    @Test
    void shouldCompileGeneratedCapybaraTestsThroughCompileTestJava() throws IOException {
        var testSourceDir = tempDir.resolve("src/test/capybara/bar");
        Files.createDirectories(testSourceDir);
        Files.writeString(testSourceDir.resolve("TestModule.cfun"), """
                from /capy/test/Assert import { * }
                from /capy/test/CapyTest import { * }

                fun works(): Assert =
                    assert_that(42).is_equal_to(42)

                fun tests(): TestFile =
                    test_file("/bar/TestModule.cfun", [
                        test("works", works())
                    ])
                """);
        var project = newProject();
        var testCapybara = project.getTasks().named("testCapybara").get();
        var resolvedDependencies = testCapybara.getTaskDependencies().getDependencies(testCapybara);
        var compileTestJava = project.getTasks().named("compileTestJava").get();
        var compileTestJavaDependencies = compileTestJava.getTaskDependencies().getDependencies(compileTestJava);

        assertFalse(project.getTasks().getNames().contains("compileCapybaraTestJava"));
        assertTrue(resolvedDependencies.contains(compileTestJava));
        assertTrue(compileTestJavaDependencies.contains(project.getTasks().named("compileTestCapybara").get()));
        assertFalse(compileTestJavaDependencies.contains(project.getTasks().named("generateTestCapybaraJava").get()));
    }

    @Test
    void shouldWireStandardJavaCompilationDirectlyToCompileCapybara() {
        var project = newProject();
        var compileJava = project.getTasks().named("compileJava").get();
        var compileJavaDependencies = compileJava.getTaskDependencies().getDependencies(compileJava);

        assertTrue(compileJavaDependencies.contains(project.getTasks().named("compileCapybara").get()));
        assertFalse(compileJavaDependencies.contains(project.getTasks().named("generateCapybaraJava").get()));
    }

    @Test
    void shouldDisableJvmTestTaskWhenProjectHasNoJvmTestSources() {
        var project = newProject();
        var testTask = project.getTasks().named("test").get();

        assertFalse(testTask.getEnabled());
        assertTrue(testTask.getDependsOn().isEmpty());
    }

    @Test
    void shouldRemoveCheckDependencyOnJvmTestTaskWhenProjectHasNoJvmTestSources() {
        var project = newProject();
        var checkTask = project.getTasks().named("check").get();
        var checkDependencies = checkTask.getTaskDependencies().getDependencies(checkTask);

        assertFalse(checkDependencies.contains(project.getTasks().named("test").get()));
        assertFalse(checkDependencies.contains(project.getTasks().named("testCapybara").get()));
    }

    @Test
    void shouldDisableCapybaraTestTaskWhenProjectHasNoCapybaraTestSources() {
        var project = newProject();
        var testCapybara = project.getTasks().named("testCapybara").get();

        assertFalse(testCapybara.getEnabled());
        assertTrue(testCapybara.getDependsOn().isEmpty());
    }

    @Test
    void shouldKeepJvmTestTaskEnabledWhenProjectHasJvmTestSources() throws IOException {
        var jvmTestSourceDir = Files.createDirectories(tempDir.resolve("src/test/java/dev/capylang"));
        Files.writeString(jvmTestSourceDir.resolve("PluginJvmTest.java"), "class PluginJvmTest {}");

        var project = newProject();
        var testTask = project.getTasks().named("test").get();

        assertTrue(testTask.getEnabled());
    }

    @Test
    void shouldKeepCheckDependencyOnJvmTestTaskWhenProjectHasJvmTestSources() throws IOException {
        var jvmTestSourceDir = Files.createDirectories(tempDir.resolve("src/test/java/dev/capylang"));
        Files.writeString(jvmTestSourceDir.resolve("PluginJvmTest.java"), "class PluginJvmTest {}");
        var capybaraTestSourceDir = Files.createDirectories(tempDir.resolve("src/test/capybara/bar"));
        Files.writeString(capybaraTestSourceDir.resolve("TestModule.cfun"), """
                from /capy/test/Assert import { * }
                from /capy/test/CapyTest import { * }

                fun works(): Assert =
                    assert_that(42).is_equal_to(42)

                fun tests(): TestFile =
                    test_file("/bar/TestModule.cfun", [
                        test("works", works())
                    ])
                """);

        var project = newProject();
        var checkTask = project.getTasks().named("check").get();
        var checkDependencies = checkTask.getTaskDependencies().getDependencies(checkTask);

        assertTrue(checkDependencies.contains(project.getTasks().named("test").get()));
        assertFalse(checkDependencies.contains(project.getTasks().named("testCapybara").get()));
    }

    @Test
    void shouldKeepDirectCheckDependencyOnCapybaraTestsWhenNoJvmTestSourcesExist() throws IOException {
        var capybaraTestSourceDir = Files.createDirectories(tempDir.resolve("src/test/capybara/bar"));
        Files.writeString(capybaraTestSourceDir.resolve("TestModule.cfun"), """
                from /capy/test/Assert import { * }
                from /capy/test/CapyTest import { * }

                fun works(): Assert =
                    assert_that(42).is_equal_to(42)

                fun tests(): TestFile =
                    test_file("/bar/TestModule.cfun", [
                        test("works", works())
                    ])
                """);

        var project = newProject();
        var checkTask = project.getTasks().named("check").get();
        var checkDependencies = checkTask.getTaskDependencies().getDependencies(checkTask);

        assertFalse(checkDependencies.contains(project.getTasks().named("test").get()));
        assertTrue(checkDependencies.contains(project.getTasks().named("testCapybara").get()));
    }

    @Test
    void shouldNotWireCompileTestJavaToCapybaraTestCompilationWhenNoCapybaraTestSourcesExist() {
        var project = newProject();
        var compileTestJava = project.getTasks().named("compileTestJava").get();
        var compileTestJavaDependencies = compileTestJava.getTaskDependencies().getDependencies(compileTestJava);

        assertFalse(compileTestJavaDependencies.contains(project.getTasks().named("compileTestCapybara").get()));
    }

    @Test
    void shouldDisableProcessResourcesTaskWhenProjectHasNoMainResources() {
        var project = newProject();
        var processResources = project.getTasks().named("processResources").get();

        assertFalse(processResources.getEnabled());
    }

    @Test
    void shouldKeepProcessResourcesTaskEnabledWhenProjectHasMainResources() throws IOException {
        var resourcesDir = Files.createDirectories(tempDir.resolve("src/main/resources"));
        Files.writeString(resourcesDir.resolve("plugin.properties"), "name=test\n");

        var project = newProject();
        var processResources = project.getTasks().named("processResources").get();

        assertTrue(processResources.getEnabled());
    }

    @Test
    void shouldDisableProcessTestResourcesTaskWhenOnlyJUnitPlatformConfigExistsWithoutJvmTests() throws IOException {
        var resourcesDir = Files.createDirectories(tempDir.resolve("src/test/resources"));
        Files.writeString(resourcesDir.resolve("junit-platform.properties"), "junit.jupiter.execution.parallel.enabled=true\n");

        var project = newProject();
        var processTestResources = project.getTasks().named("processTestResources").get();

        assertFalse(processTestResources.getEnabled());
    }

    @Test
    void shouldKeepProcessTestResourcesTaskEnabledWhenNonJvmTestResourcesExist() throws IOException {
        var resourcesDir = Files.createDirectories(tempDir.resolve("src/test/resources/fixtures"));
        Files.writeString(resourcesDir.resolve("sample.txt"), "fixture\n");

        var project = newProject();
        var processTestResources = project.getTasks().named("processTestResources").get();

        assertTrue(processTestResources.getEnabled());
    }

    @Test
    void shouldKeepProcessTestResourcesTaskEnabledWhenJvmTestSourcesExist() throws IOException {
        var jvmTestSourceDir = Files.createDirectories(tempDir.resolve("src/test/java/dev/capylang"));
        Files.writeString(jvmTestSourceDir.resolve("PluginJvmTest.java"), "class PluginJvmTest {}");
        var resourcesDir = Files.createDirectories(tempDir.resolve("src/test/resources"));
        Files.writeString(resourcesDir.resolve("junit-platform.properties"), "junit.jupiter.execution.parallel.enabled=true\n");

        var project = newProject();
        var processTestResources = project.getTasks().named("processTestResources").get();

        assertTrue(processTestResources.getEnabled());
    }

    @Test
    void shouldDisableProcessTestResourcesTaskWhenJvmTestSourcesExistWithoutTestResources() throws IOException {
        var jvmTestSourceDir = Files.createDirectories(tempDir.resolve("src/test/java/dev/capylang"));
        Files.writeString(jvmTestSourceDir.resolve("PluginJvmTest.java"), "class PluginJvmTest {}");

        var project = newProject();
        var processTestResources = project.getTasks().named("processTestResources").get();

        assertFalse(processTestResources.getEnabled());
    }

    @Test
    void shouldUseInProcessTaskForCapybaraTests() {
        var project = newProject();

        assertInstanceOf(CapybaraTestTask.class, project.getTasks().named("testCapybara").get());
    }

    @Test
    void shouldDisableJvmTestClassScanningAndUseConventionalIncludes() {
        var project = newProject();
        var testTask = project.getTasks().named("test", Test.class).get();

        assertFalse(testTask.isScanForTestClasses());
        assertTrue(testTask.getIncludes().contains("**/*Test.class"));
        assertTrue(testTask.getIncludes().contains("**/*Tests.class"));
        assertTrue(testTask.getIncludes().contains("**/*IT.class"));
        assertTrue(testTask.getIncludes().contains("**/*IntegrationTest.class"));
        assertTrue(testTask.getExcludes().contains("capy/**"));
    }

    @Test
    void shouldDisableHtmlJvmTestReportsAndKeepJUnitXml() {
        var project = newProject();
        var testTask = project.getTasks().named("test", Test.class).get();

        assertFalse(testTask.getReports().getHtml().getRequired().get());
        assertTrue(testTask.getReports().getJunitXml().getRequired().get());
    }

    @Test
    void shouldSetParallelJvmTestExecutionProperties() {
        var project = newProject();
        var testTask = project.getTasks().named("test", Test.class).get();

        assertEquals("true", testTask.getSystemProperties().get("junit.jupiter.execution.parallel.enabled"));
        assertEquals("concurrent", testTask.getSystemProperties().get("junit.jupiter.execution.parallel.mode.default"));
        assertEquals("concurrent", testTask.getSystemProperties().get("junit.jupiter.execution.parallel.mode.classes.default"));
        assertEquals("dynamic", testTask.getSystemProperties().get("junit.jupiter.execution.parallel.config.strategy"));
    }

    @Test
    void shouldUseMainRuntimeClasspathAndTestClassesForCapybaraTests() {
        var project = newProject();
        var sourceSets = project.getExtensions().getByType(SourceSetContainer.class);
        var testCapybara = project.getTasks().named("testCapybara", CapybaraTestTask.class).get();
        var runtimeClasspathSources = ((ConfigurableFileCollection) testCapybara.getRuntimeClasspath()).getFrom();

        assertTrue(runtimeClasspathSources.contains(sourceSets.getByName("main").getRuntimeClasspath()));
        assertTrue(runtimeClasspathSources.contains(sourceSets.getByName("test").getOutput().getClassesDirs()));
        assertFalse(runtimeClasspathSources.contains(sourceSets.getByName("test").getOutput()));
        assertFalse(runtimeClasspathSources.contains(sourceSets.getByName("test").getRuntimeClasspath()));
    }

    @ParameterizedTest
    @MethodSource("gradleLogLevels")
    void shouldMapGradleLogLevelToCapybaraTestLogLevel(LogLevel gradleLogLevel, String expectedLogLevel) {
        assertEquals(expectedLogLevel, CapybaraPlugin.capybaraTestLogLevel(gradleLogLevel));
    }

    @ParameterizedTest
    @MethodSource("fusedCapybaraLifecycleTasks")
    void shouldWireLifecycleBuildJavaCompilationDirectlyToFusedCompileTask(String requestedTask) {
        var project = newProject(List.of(requestedTask));
        var compileJava = project.getTasks().named("compileJava").get();
        var compileJavaDependencies = compileJava.getTaskDependencies().getDependencies(compileJava);
        var compileTestJava = project.getTasks().named("compileTestJava").get();
        var compileTestJavaDependencies = compileTestJava.getTaskDependencies().getDependencies(compileTestJava);
        var compileCapybara = project.getTasks().named("compileCapybara").get();

        assertTrue(compileJavaDependencies.contains(compileCapybara));
        assertFalse(compileJavaDependencies.contains(project.getTasks().named("generateCapybaraJava").get()));
        assertTrue(compileTestJavaDependencies.contains(compileCapybara));
        assertFalse(compileTestJavaDependencies.contains(project.getTasks().named("generateTestCapybaraJava").get()));
        assertFalse(compileTestJavaDependencies.contains(compileJava));
    }

    @ParameterizedTest
    @MethodSource("fusedCapybaraLifecycleTasks")
    void shouldRemoveMainClassesLifecycleDependenciesFromCompileTestJavaForCapybaraOnlyBuilds(String requestedTask) {
        var project = newProject(List.of(requestedTask));
        var compileTestJava = project.getTasks().named("compileTestJava").get();
        var compileTestJavaDependencies = compileTestJava.getTaskDependencies().getDependencies(compileTestJava);

        assertFalse(compileTestJavaDependencies.contains(project.getTasks().named("classes").get()));
        assertFalse(compileTestJavaDependencies.contains(project.getTasks().named("compileJava").get()));
        assertFalse(compileTestJavaDependencies.contains(project.getTasks().named("processResources").get()));
        assertTrue(compileTestJavaDependencies.contains(project.getTasks().named("compileCapybara").get()));
    }

    @ParameterizedTest
    @MethodSource("fusedCapybaraLifecycleTasks")
    void shouldSkipCompileJavaForLifecycleBuildsWithoutJvmMainSources(String requestedTask) {
        var project = newProject(List.of(requestedTask));
        var compileJava = project.getTasks().named("compileJava").get();

        assertFalse(compileJava.getEnabled());
        assertTrue(compileJava.getDependsOn().isEmpty());
    }

    @Test
    void shouldUseFusedCheckPathForQualifiedTaskRequests() {
        var project = newProject(List.of(":lib:capybara-lib:check"));
        var compileJava = project.getTasks().named("compileJava").get();
        var compileTestJava = project.getTasks().named("compileTestJava").get();
        var compileTestJavaDependencies = compileTestJava.getTaskDependencies().getDependencies(compileTestJava);
        var compileCapybara = project.getTasks().named("compileCapybara").get();

        assertFalse(compileJava.getEnabled());
        assertTrue(compileJava.getDependsOn().isEmpty());
        assertTrue(compileTestJavaDependencies.contains(compileCapybara));
        assertFalse(compileTestJavaDependencies.contains(project.getTasks().named("generateTestCapybaraJava").get()));
    }

    @Test
    void shouldUseFusedBuildPathForQualifiedTaskRequests() {
        var project = newProject(List.of(":lib:capybara-lib:build"));
        var compileJava = project.getTasks().named("compileJava").get();
        var compileTestJava = project.getTasks().named("compileTestJava").get();
        var compileTestJavaDependencies = compileTestJava.getTaskDependencies().getDependencies(compileTestJava);
        var compileCapybara = project.getTasks().named("compileCapybara").get();

        assertFalse(compileJava.getEnabled());
        assertTrue(compileJava.getDependsOn().isEmpty());
        assertTrue(compileTestJavaDependencies.contains(compileCapybara));
        assertFalse(compileTestJavaDependencies.contains(project.getTasks().named("generateTestCapybaraJava").get()));
    }

    @Test
    void shouldFoldJvmMainSourcesIntoCompileTestJavaForCheckBuildsWithoutMainResources() throws IOException {
        var jvmMainSourceDir = Files.createDirectories(tempDir.resolve("src/main/java/dev/capylang"));
        Files.writeString(jvmMainSourceDir.resolve("PluginMain.java"), "class PluginMain {}");

        var project = newProject(List.of("check"));
        var compileJava = project.getTasks().named("compileJava").get();
        var compileTestJava = project.getTasks().named("compileTestJava").get();
        var compileTestJavaDependencies = compileTestJava.getTaskDependencies().getDependencies(compileTestJava);
        var classes = project.getTasks().named("classes").get();
        var sourceSets = project.getExtensions().getByType(SourceSetContainer.class);
        var testSrcDirs = sourceSets.getByName("test").getJava().getSrcDirs();

        assertFalse(compileJava.getEnabled());
        assertTrue(compileJava.getDependsOn().isEmpty());
        assertFalse(classes.getEnabled());
        assertTrue(classes.getDependsOn().isEmpty());
        assertTrue(compileTestJavaDependencies.contains(project.getTasks().named("compileCapybara").get()));
        assertFalse(compileTestJavaDependencies.contains(project.getTasks().named("generateTestCapybaraJava").get()));
        assertFalse(compileTestJavaDependencies.contains(compileJava));
        assertTrue(testSrcDirs.contains(project.file("src/main/java")));
        assertTrue(testSrcDirs.contains(project.file("build/generated/sources/capybara/java/check")));
        assertFalse(testSrcDirs.contains(project.file("build/generated/sources/test-capybara/java")));
    }

    @Test
    void shouldKeepSeparateMainOutputsForCheckBuildsWithMainResources() throws IOException {
        var resourcesDir = Files.createDirectories(tempDir.resolve("src/main/resources"));
        Files.writeString(resourcesDir.resolve("capybara.txt"), "resource");
        var capybaraTestSourceDir = Files.createDirectories(tempDir.resolve("src/test/capybara/bar"));
        Files.writeString(capybaraTestSourceDir.resolve("TestModule.cfun"), """
                from /capy/test/Assert import { * }
                from /capy/test/CapyTest import { * }

                fun works(): Assert =
                    assert_that(42).is_equal_to(42)

                fun tests(): TestFile =
                    test_file("/bar/TestModule.cfun", [
                        test("works", works())
                    ])
                """);

        var project = newProject(List.of("check"));
        var compileCapybara = project.getTasks().named("compileCapybara", CompileCapybaraTask.class).get();
        var compileJava = project.getTasks().named("compileJava").get();
        var compileTestJava = project.getTasks().named("compileTestJava").get();
        var compileTestJavaDependencies = compileTestJava.getTaskDependencies().getDependencies(compileTestJava);
        var sourceSets = project.getExtensions().getByType(SourceSetContainer.class);
        var testSrcDirs = sourceSets.getByName("test").getJava().getSrcDirs();
        var testCapybara = project.getTasks().named("testCapybara", CapybaraTestTask.class).get();
        var runtimeClasspathSources = ((ConfigurableFileCollection) testCapybara.getRuntimeClasspath()).getFrom();

        assertTrue(compileCapybara.getOutputDir().isPresent());
        assertFalse(compileCapybara.getTestInputDir().isPresent());
        assertFalse(compileCapybara.getGeneratedTestOutputDir().isPresent());
        assertTrue(compileJava.getEnabled());
        assertFalse(testSrcDirs.contains(project.file("build/generated/sources/capybara/java")));
        assertTrue(compileTestJavaDependencies.contains(project.getTasks().named("compileTestCapybara").get()));
        assertFalse(compileTestJavaDependencies.contains(project.getTasks().named("generateTestCapybaraJava").get()));
        assertFalse(compileTestJavaDependencies.contains(project.getTasks().named("compileCapybara").get()));
        assertTrue(runtimeClasspathSources.contains(sourceSets.getByName("main").getRuntimeClasspath()));
    }

    @Test
    void shouldUseDependencyClasspathAndTestClassesForCheckBuildsWithJvmMainSources() throws IOException {
        var jvmMainSourceDir = Files.createDirectories(tempDir.resolve("src/main/java/dev/capylang"));
        Files.writeString(jvmMainSourceDir.resolve("PluginMain.java"), "class PluginMain {}");

        var project = newProject(List.of("check"));
        var sourceSets = project.getExtensions().getByType(SourceSetContainer.class);
        var testClasses = project.getTasks().named("testClasses").get();
        var testClassesDependencies = testClasses.getTaskDependencies().getDependencies(testClasses);
        var testCapybara = project.getTasks().named("testCapybara", CapybaraTestTask.class).get();
        var runtimeClasspathSources = ((ConfigurableFileCollection) testCapybara.getRuntimeClasspath()).getFrom();

        assertFalse(testClassesDependencies.contains(project.getTasks().named("classes").get()));
        assertFalse(runtimeClasspathSources.contains(sourceSets.getByName("main").getRuntimeClasspath()));
        assertTrue(runtimeClasspathSources.contains(sourceSets.getByName("test").getOutput().getClassesDirs()));
    }

    @ParameterizedTest
    @MethodSource("fusedCapybaraLifecycleTasks")
    void shouldDisableClassesLifecycleTaskForCapybaraOnlyBuilds(String requestedTask) {
        var project = newProject(List.of(requestedTask));
        var classes = project.getTasks().named("classes").get();

        assertFalse(classes.getEnabled());
        assertTrue(classes.getDependsOn().isEmpty());
    }

    @Test
    void shouldKeepClassesLifecycleTaskEnabledWhenMainOutputsExist() throws IOException {
        var resourcesDir = Files.createDirectories(tempDir.resolve("src/main/resources"));
        Files.writeString(resourcesDir.resolve("capybara.txt"), "resource");

        var project = newProject(List.of("check"));
        var classes = project.getTasks().named("classes").get();

        assertTrue(classes.getEnabled());
    }

    @ParameterizedTest
    @MethodSource("fusedCapybaraLifecycleTasks")
    void shouldDisableTestClassesLifecycleTaskWhenOnlyCapybaraTestsExist(String requestedTask) {
        var project = newProject(List.of(requestedTask));
        var testClasses = project.getTasks().named("testClasses").get();

        assertFalse(testClasses.getEnabled());
        assertTrue(testClasses.getDependsOn().isEmpty());
    }

    @Test
    void shouldKeepTestClassesLifecycleTaskEnabledWhenTestResourcesExist() throws IOException {
        var resourcesDir = Files.createDirectories(tempDir.resolve("src/test/resources"));
        Files.writeString(resourcesDir.resolve("capybara-data.txt"), "resource");

        var project = newProject(List.of("check"));
        var testClasses = project.getTasks().named("testClasses").get();

        assertTrue(testClasses.getEnabled());
    }

    @Test
    void shouldUseDependencyClasspathAndTestClassesForCapybaraOnlyCheckBuilds() {
        var project = newProject(List.of("check"));
        var sourceSets = project.getExtensions().getByType(SourceSetContainer.class);
        var testCapybara = project.getTasks().named("testCapybara", CapybaraTestTask.class).get();
        var runtimeClasspathSources = ((ConfigurableFileCollection) testCapybara.getRuntimeClasspath()).getFrom();

        assertFalse(runtimeClasspathSources.contains(sourceSets.getByName("main").getRuntimeClasspath()));
        assertTrue(runtimeClasspathSources.contains(sourceSets.getByName("test").getOutput().getClassesDirs()));
    }

    private Project newProject() {
        return newProject(List.of());
    }

    private static Stream<org.junit.jupiter.params.provider.Arguments> gradleLogLevels() {
        return Stream.of(
                arguments(LogLevel.DEBUG, "DEBUG"),
                arguments(LogLevel.INFO, "INFO"),
                arguments(LogLevel.LIFECYCLE, "WARN"),
                arguments(LogLevel.WARN, "WARN"),
                arguments(LogLevel.QUIET, "WARN"),
                arguments(LogLevel.ERROR, "WARN")
        );
    }

    private static Stream<org.junit.jupiter.params.provider.Arguments> fusedCapybaraLifecycleTasks() {
        return Stream.of(
                arguments("check"),
                arguments("build"),
                arguments("buildNeeded"),
                arguments("buildDependents")
        );
    }

    private Project newProject(List<String> requestedTasks) {
        var project = ProjectBuilder.builder()
                .withProjectDir(tempDir.toFile())
                .build();
        project.getGradle().getStartParameter().setTaskNames(requestedTasks);
        project.setVersion("0.0.0-test");
        project.getPluginManager().apply(JavaPlugin.class);
        project.getPluginManager().apply(CapybaraPlugin.class);
        return project;
    }
}
