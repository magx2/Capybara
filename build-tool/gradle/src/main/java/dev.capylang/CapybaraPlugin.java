package dev.capylang;

import org.gradle.api.Plugin;
import org.gradle.api.Project;
import org.gradle.api.tasks.SourceSetContainer;
import org.gradle.api.tasks.testing.Test;
import org.gradle.api.tasks.JavaExec;
import dev.capylang.compiler.OutputType;

import java.util.EnumSet;

public class CapybaraPlugin implements Plugin<Project> {
    @Override
    public void apply(Project project) {
        var layout = project.getLayout();
        var compilerVersion = project.getVersion().toString();
        var capybaraTestResultsDir = layout.getBuildDirectory().dir("test-results/capybara");

        var compileCapybara = project.getTasks().register(
                "compileCapybara",
                CompileCapybaraTask.class,
                task -> {
                    task.setGroup("build");
                    task.setDescription("Compiles Capybara files from src/main/capybara.");
                    task.getInputDir().set(project.file("src/main/capybara"));
                    task.getOutputDir().set(layout.getBuildDirectory().dir("classes/capybara"));
                    task.getAdditionalInputDirs().from();
                    task.getCompilerVersion().set(compilerVersion);
                    task.getCompileTests().set(false);
                }
        );

        var generateCapybaraJava = project.getTasks().register(
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

        var compileTestCapybara = project.getTasks().register(
                "compileTestCapybara",
                CompileCapybaraTask.class,
                task -> {
                    task.setGroup("verification");
                    task.setDescription("Compiles Capybara files from src/test/capybara.");
                    task.dependsOn(compileCapybara);
                    task.getInputDir().set(project.file("src/test/capybara"));
                    task.getOutputDir().set(layout.getBuildDirectory().dir("classes/test-capybara"));
                    task.getAdditionalInputDirs().from(layout.getBuildDirectory().dir("classes/capybara"));
                    task.getCompilerVersion().set(compilerVersion);
                    task.getCompileTests().set(true);
                }
        );

        var generateTestCapybaraJava = project.getTasks().register(
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
            sourceSets.named("test", sourceSet -> {
                sourceSet.getJava().srcDir(layout.getBuildDirectory().dir("generated/sources/capybara/java"));
                sourceSet.getJava().srcDir(layout.getBuildDirectory().dir("generated/sources/test-capybara/java"));
            });

            var testCapybara = project.getTasks().register(
                    "testCapybara",
                    JavaExec.class,
                    task -> {
                        task.setGroup("verification");
                        task.setDescription("Runs Capybara tests using generated Java classes.");
                        task.dependsOn(generateCapybaraJava);
                        task.dependsOn(project.getTasks().named("compileTestJava"));
                        task.classpath(sourceSets.getByName("test").getRuntimeClasspath());
                        task.getMainClass().set("dev.capylang.test.TestRunner");
                        task.doFirst(ignored -> {
                            var outputDir = capybaraTestResultsDir.get().getAsFile();
                            outputDir.mkdirs();
                            task.setArgs(java.util.List.of(
                                    "-o", outputDir.getAbsolutePath(),
                                    "-rt", "JUNIT"
                            ));
                        });
                    }
            );

            project.getTasks().matching(task -> task.getName().equals("test")).configureEach(task -> task.dependsOn(testCapybara));
            project.getTasks().matching(task -> task.getName().equals("check")).configureEach(task -> task.dependsOn(testCapybara));
        }
    }
}

