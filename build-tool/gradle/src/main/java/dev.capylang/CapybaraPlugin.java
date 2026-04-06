package dev.capylang;

import org.gradle.api.Plugin;
import org.gradle.api.Project;
import org.gradle.api.tasks.SourceSetContainer;
import org.gradle.api.tasks.JavaExec;
import org.gradle.api.tasks.compile.JavaCompile;

public class CapybaraPlugin implements Plugin<Project> {
    @Override
    public void apply(Project project) {
        var layout = project.getLayout();
        var compilerVersion = project.getVersion().toString();
        var capybaraTestResultsDir = layout.getBuildDirectory().dir("test-results/capybara");
        var capybaraTestClassesDir = layout.getBuildDirectory().dir("classes/java/capybaraTest");
        var capybaraTestBuildRequested = project.provider(() ->
                project.getGradle().getStartParameter().getTaskNames().stream().anyMatch(taskName ->
                        taskName.equals("check") || taskName.endsWith(":check") ||
                                taskName.equals("test") || taskName.endsWith(":test") ||
                                taskName.equals("testClasses") || taskName.endsWith(":testClasses") ||
                                taskName.equals("compileTestJava") || taskName.endsWith(":compileTestJava") ||
                                taskName.equals("compileTestCapybara") || taskName.endsWith(":compileTestCapybara") ||
                                taskName.equals("generateTestCapybaraJava") || taskName.endsWith(":generateTestCapybaraJava") ||
                                taskName.equals("compileCapybaraTestJava") || taskName.endsWith(":compileCapybaraTestJava") ||
                                taskName.equals("testCapybara") || taskName.endsWith(":testCapybara")
                ));

        var compileCapybara = project.getTasks().register(
                "compileCapybara",
                CompileCapybaraTask.class,
                task -> {
                    task.setGroup("build");
                    task.setDescription("Compiles Capybara files from src/main/capybara.");
                    task.getInputDir().set(project.file("src/main/capybara"));
                    task.getOutputDir().set(layout.getBuildDirectory().dir("classes/capybara"));
                    task.getGeneratedOutputDir().set(layout.getBuildDirectory().dir("generated/sources/capybara/java"));
                    task.getTestInputDir().set(project.file("src/test/capybara"));
                    task.getGeneratedTestOutputDir().set(layout.getBuildDirectory().dir("generated/sources/test-capybara/java"));
                    task.getAdditionalInputDirs().from();
                    task.getCompilerVersion().set(compilerVersion);
                    task.getCompileTests().set(false);
                    task.getIncludeJavaLibResources().set(true);
                    task.getCompileTestSourcesWithMainCompilation().set(capybaraTestBuildRequested);
                    task.getIncludeJavaLibResourcesInTestOutput().set(false);
                }
        );

        var generateCapybaraJava = project.getTasks().register(
                "generateCapybaraJava",
                task -> {
                    task.setGroup("build");
                    task.setDescription("Compatibility task for generated Java classes.");
                    task.dependsOn(compileCapybara);
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
                    task.getGeneratedOutputDir().set(layout.getBuildDirectory().dir("generated/sources/test-capybara/java"));
                    task.getAdditionalInputDirs().from(layout.getBuildDirectory().dir("classes/capybara"));
                    task.getCompilerVersion().set(compilerVersion);
                    task.getCompileTests().set(true);
                    task.getIncludeJavaLibResources().set(false);
                    task.getCompileTestSourcesWithMainCompilation().set(false);
                    task.getIncludeJavaLibResourcesInTestOutput().set(false);
                    task.onlyIf(ignored -> !capybaraTestBuildRequested.get());
                }
        );

        var generateTestCapybaraJava = project.getTasks().register(
                "generateTestCapybaraJava",
                task -> {
                    task.setGroup("verification");
                    task.setDescription("Compatibility task for generated test Java classes.");
                    task.dependsOn(compileCapybara);
                    task.dependsOn(compileTestCapybara);
                }
        );

        var sourceSets = project.getExtensions().findByType(SourceSetContainer.class);
        if (sourceSets != null) {
            sourceSets.named("main", sourceSet ->
                    sourceSet.getJava().srcDir(layout.getBuildDirectory().dir("generated/sources/capybara/java")));

            var compileCapybaraTestJava = project.getTasks().register(
                    "compileCapybaraTestJava",
                    JavaCompile.class,
                    task -> {
                        task.setGroup("verification");
                        task.setDescription("Compiles generated Java from test Capybara sources.");
                        task.dependsOn(project.getTasks().named("compileTestJava"));
                        task.dependsOn(generateTestCapybaraJava);
                        task.source(project.fileTree(layout.getBuildDirectory().dir("generated/sources/test-capybara/java")));
                        task.setClasspath(project.files(
                                sourceSets.getByName("test").getOutput(),
                                sourceSets.getByName("test").getCompileClasspath()
                        ));
                        task.getDestinationDirectory().set(capybaraTestClassesDir);
                    }
            );

            var testCapybara = project.getTasks().register(
                    "testCapybara",
                    JavaExec.class,
                    task -> {
                        task.setGroup("verification");
                        task.setDescription("Runs Capybara tests using generated Java classes.");
                        task.dependsOn(compileCapybara);
                        task.dependsOn(project.getTasks().named("compileTestJava"));
                        task.dependsOn(compileCapybaraTestJava);
                        task.classpath(project.files(
                                capybaraTestClassesDir,
                                sourceSets.getByName("test").getRuntimeClasspath()
                        ));
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
