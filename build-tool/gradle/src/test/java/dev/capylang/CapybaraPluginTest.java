package dev.capylang;

import org.gradle.api.Project;
import org.gradle.api.file.ConfigurableFileCollection;
import org.gradle.api.logging.LogLevel;
import org.gradle.api.plugins.JavaPlugin;
import org.gradle.api.tasks.SourceSetContainer;
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

        assertTrue(project.file("build/generated/sources/capybara/java/foo/Lib.java").isFile());
        assertTrue(project.file("build/generated/sources/test-capybara/java/bar/TestModule.java").isFile());
        assertFalse(project.file("build/classes/capybara/foo/Lib.json").exists());
        assertFalse(project.file("build/classes/capybara/program.json").exists());
        assertFalse(project.file("build/classes/capybara/build-info.json").exists());
        assertFalse(project.file("build/generated/sources/test-capybara/java/dev/capylang/CapybaraUtil.java").exists());
        assertFalse(project.getTasks().named("compileTestCapybara", CompileCapybaraTask.class).get().getOnlyIf().isSatisfiedBy(
                project.getTasks().named("compileTestCapybara", CompileCapybaraTask.class).get()
        ));
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
    void shouldCompileGeneratedCapybaraTestsThroughCompileTestJava() {
        var project = newProject();
        var testCapybara = project.getTasks().named("testCapybara").get();
        var resolvedDependencies = testCapybara.getTaskDependencies().getDependencies(testCapybara);
        var compileTestJava = project.getTasks().named("compileTestJava").get();
        var compileTestJavaDependencies = compileTestJava.getTaskDependencies().getDependencies(compileTestJava);

        assertFalse(project.getTasks().getNames().contains("compileCapybaraTestJava"));
        assertTrue(resolvedDependencies.contains(compileTestJava));
        assertTrue(compileTestJavaDependencies.contains(project.getTasks().named("generateTestCapybaraJava").get()));
    }

    @Test
    void shouldDisableJvmTestTaskWhenProjectHasNoJvmTestSources() {
        var project = newProject();
        var testTask = project.getTasks().named("test").get();

        assertFalse(testTask.getEnabled());
        assertTrue(testTask.getDependsOn().isEmpty());
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
    void shouldUseInProcessTaskForCapybaraTests() {
        var project = newProject();

        assertInstanceOf(CapybaraTestTask.class, project.getTasks().named("testCapybara").get());
    }

    @Test
    void shouldUseMainRuntimeClasspathAndTestOutputsForCapybaraTests() {
        var project = newProject();
        var sourceSets = project.getExtensions().getByType(SourceSetContainer.class);
        var testCapybara = project.getTasks().named("testCapybara", CapybaraTestTask.class).get();
        var runtimeClasspathSources = ((ConfigurableFileCollection) testCapybara.getRuntimeClasspath()).getFrom();

        assertTrue(runtimeClasspathSources.contains(sourceSets.getByName("main").getRuntimeClasspath()));
        assertTrue(runtimeClasspathSources.contains(sourceSets.getByName("test").getOutput()));
        assertFalse(runtimeClasspathSources.contains(sourceSets.getByName("test").getRuntimeClasspath()));
    }

    @ParameterizedTest
    @MethodSource("gradleLogLevels")
    void shouldMapGradleLogLevelToCapybaraTestLogLevel(LogLevel gradleLogLevel, String expectedLogLevel) {
        assertEquals(expectedLogLevel, CapybaraPlugin.capybaraTestLogLevel(gradleLogLevel));
    }

    @Test
    void shouldWireCheckBuildJavaCompilationDirectlyToFusedCompileTask() {
        var project = newProject(List.of("check"));
        var compileJava = project.getTasks().named("compileJava").get();
        var compileJavaDependencies = compileJava.getTaskDependencies().getDependencies(compileJava);
        var compileTestJava = project.getTasks().named("compileTestJava").get();
        var compileTestJavaDependencies = compileTestJava.getTaskDependencies().getDependencies(compileTestJava);
        var compileCapybara = project.getTasks().named("compileCapybara").get();

        assertTrue(compileJavaDependencies.contains(compileCapybara));
        assertFalse(compileJavaDependencies.contains(project.getTasks().named("generateCapybaraJava").get()));
        assertTrue(compileTestJavaDependencies.contains(compileCapybara));
        assertFalse(compileTestJavaDependencies.contains(project.getTasks().named("generateTestCapybaraJava").get()));
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
