package dev.capylang;

import org.gradle.api.Project;
import org.gradle.api.plugins.JavaPlugin;
import org.gradle.api.tasks.SourceSetContainer;
import org.gradle.testfixtures.ProjectBuilder;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

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
        assertTrue(project.file("build/generated/sources/capybara/java/foo/Main.java").isFile());
        assertTrue(project.file("build/generated/sources/capybara/java/dev/capylang/CapybaraUtil.java").isFile());
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

    private Project newProject() {
        var project = ProjectBuilder.builder()
                .withProjectDir(tempDir.toFile())
                .build();
        project.setVersion("0.0.0-test");
        project.getPluginManager().apply(JavaPlugin.class);
        project.getPluginManager().apply(CapybaraPlugin.class);
        return project;
    }
}
