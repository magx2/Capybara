package dev.capylang;

import org.gradle.api.Plugin;
import org.gradle.api.Project;
import org.gradle.api.tasks.JavaExec;
import org.gradle.api.tasks.SourceSetContainer;
import org.gradle.api.tasks.TaskProvider;
import org.gradle.api.tasks.testing.Test;
import dev.capylang.compiler.OutputType;

import java.util.EnumSet;

public class CapybaraPlugin implements Plugin<Project> {
    @Override
    public void apply(Project project) {
        var layout = project.getLayout();
        var compilerVersion = project.getVersion().toString();

        TaskProvider<CompileCapybaraTask> compileCapybara = project.getTasks().register(
                "compileCapybara",
                CompileCapybaraTask.class,
                task -> {
                    task.setGroup("build");
                    task.setDescription("Compiles Capybara files from src/main/capybara.");
                    task.getInputDir().set(project.file("src/main/capybara"));
                    task.getOutputDir().set(layout.getBuildDirectory().dir("classes/capybara"));
                    task.getCompilerVersion().set(compilerVersion);
                }
        );

        TaskProvider<GenerateCapybaraTask> generateCapybaraJava = project.getTasks().register(
                "generateCapybaraJava",
                GenerateCapybaraTask.class,
                task -> {
                    task.setGroup("build");
                    task.setDescription("Generates Java classes");
                    task.dependsOn(compileCapybara);
                    task.getInputDir().set(layout.getBuildDirectory().dir("classes/capybara"));
                    task.getOutputRootDir().set(layout.getBuildDirectory().dir("generated/sources/capybara"));
                    task.getOutputTypes().set(EnumSet.of(OutputType.JAVA));
                }
        );

        project.getTasks().named("compileJava", task -> task.dependsOn(generateCapybaraJava));

        TaskProvider<CompileCapybaraTask> compileTestCapybara = project.getTasks().register(
                "compileTestCapybara",
                CompileCapybaraTask.class,
                task -> {
                    task.setGroup("verification");
                    task.setDescription("Compiles Capybara files from src/test/capybara.");
                    task.getInputDir().set(project.file("src/test/capybara"));
                    task.getOutputDir().set(layout.getBuildDirectory().dir("classes/test-capybara"));
                    task.getCompilerVersion().set(compilerVersion);
                }
        );

        TaskProvider<GenerateCapybaraTask> generateTestCapybaraJava = project.getTasks().register(
                "generateTestCapybaraJava",
                GenerateCapybaraTask.class,
                task -> {
                    task.setGroup("verification");
                    task.setDescription("Generates Java classes for tests");
                    task.dependsOn(compileTestCapybara);
                    task.getInputDir().set(layout.getBuildDirectory().dir("classes/test-capybara"));
                    task.getOutputRootDir().set(layout.getBuildDirectory().dir("generated/sources/test-capybara"));
                    task.getOutputTypes().set(EnumSet.of(OutputType.JAVA));
                }
        );

        project.getTasks().named("compileTestJava", task -> task.dependsOn(generateTestCapybaraJava));

        var sourceSets = project.getExtensions().findByType(SourceSetContainer.class);
        if (sourceSets != null) {
            sourceSets.named("main", sourceSet ->
                    sourceSet.getJava().srcDir(layout.getBuildDirectory().dir("generated/sources/capybara/java")));
            sourceSets.named("test", sourceSet ->
                    sourceSet.getJava().srcDir(layout.getBuildDirectory().dir("generated/sources/test-capybara/java")));
        }

        if (sourceSets != null) {
            TaskProvider<JavaExec> runJsonTest = project.getTasks().register("runJsonTest", JavaExec.class, task -> {
                task.setGroup("verification");
                task.setDescription("Runs `JsonTest` main function.");
                task.dependsOn(project.getTasks().named("compileTestJava"));
                task.setClasspath(sourceSets.getByName("test").getRuntimeClasspath());
                task.getMainClass().set("lang.serialization.JsonTest");
            });

            TaskProvider<JavaExec> runResultTest = project.getTasks().register("runResultTest", JavaExec.class, task -> {
                task.setGroup("verification");
                task.setDescription("Runs `ResultTest` main function.");
                task.dependsOn(project.getTasks().named("compileTestJava"));
                task.setClasspath(sourceSets.getByName("test").getRuntimeClasspath());
                task.getMainClass().set("capy.lang.ResultTest");
            });

            TaskProvider<JavaExec> runSeqTest = project.getTasks().register("runSeqTest", JavaExec.class, task -> {
                task.setGroup("verification");
                task.setDescription("Runs `SeqTest` main function.");
                task.dependsOn(project.getTasks().named("compileTestJava"));
                task.setClasspath(sourceSets.getByName("test").getRuntimeClasspath());
                task.getMainClass().set("capy.lang.SeqTest");
            });

            TaskProvider<JavaExec> runCollectionAssertTest = project.getTasks().register("runCollectionAssertTest", JavaExec.class, task -> {
                task.setGroup("verification");
                task.setDescription("Runs `CollectionAssertTest` main function.");
                task.dependsOn(project.getTasks().named("compileTestJava"));
                task.setClasspath(sourceSets.getByName("test").getRuntimeClasspath());
                task.getMainClass().set("capy.assert_.CollectionAssertTest");
            });

            project.getTasks().register("testCapybara", task -> {
                task.setGroup("verification");
                task.setDescription("Runs all generated Capybara main-based tests.");
                task.dependsOn(runJsonTest, runResultTest, runSeqTest, runCollectionAssertTest);
            });

            project.getTasks().named("test", Test.class, task -> task.dependsOn("testCapybara"));
        }
    }
}
