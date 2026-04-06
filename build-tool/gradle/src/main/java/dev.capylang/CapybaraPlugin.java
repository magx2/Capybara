package dev.capylang;

import org.gradle.api.Plugin;
import org.gradle.api.Project;
import org.gradle.api.tasks.SourceSetContainer;
import org.gradle.api.tasks.JavaExec;

public class CapybaraPlugin implements Plugin<Project> {
    @Override
    public void apply(Project project) {
        var layout = project.getLayout();
        var compilerVersion = project.getVersion().toString();
        var capybaraTestResultsDir = layout.getBuildDirectory().dir("test-results/capybara");
        var capybaraTestBuildRequested = project.provider(() ->
                project.getGradle().getStartParameter().getTaskNames().stream().anyMatch(taskName ->
                        taskName.equals("check") || taskName.endsWith(":check") ||
                                taskName.equals("test") || taskName.endsWith(":test") ||
                                taskName.equals("testClasses") || taskName.endsWith(":testClasses") ||
                                taskName.equals("compileTestJava") || taskName.endsWith(":compileTestJava") ||
                                taskName.equals("compileTestCapybara") || taskName.endsWith(":compileTestCapybara") ||
                                taskName.equals("generateTestCapybaraJava") || taskName.endsWith(":generateTestCapybaraJava") ||
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
                    task.getWriteLinkedOutput().set(capybaraTestBuildRequested.map(requested -> !requested));
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

        project.getTasks().named("compileJava", task ->
                task.dependsOn(capybaraTestBuildRequested.get() ? compileCapybara : generateCapybaraJava));

        var compileTestCapybara = project.getTasks().register(
                "compileTestCapybara",
                CompileCapybaraTask.class,
                task -> {
                    task.setGroup("verification");
                    task.setDescription("Compiles Capybara files from src/test/capybara.");
                    task.dependsOn(compileCapybara);
                    task.getInputDir().set(project.file("src/test/capybara"));
                    task.getGeneratedOutputDir().set(layout.getBuildDirectory().dir("generated/sources/test-capybara/java"));
                    task.getAdditionalInputDirs().from(layout.getBuildDirectory().dir("classes/capybara"));
                    task.getCompilerVersion().set(compilerVersion);
                    task.getCompileTests().set(true);
                    task.getIncludeJavaLibResources().set(false);
                    task.getCompileTestSourcesWithMainCompilation().set(false);
                    task.getWriteLinkedOutput().set(false);
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
            sourceSets.named("test", sourceSet ->
                    sourceSet.getJava().srcDir(layout.getBuildDirectory().dir("generated/sources/test-capybara/java")));
            project.getTasks().named("compileTestJava", task ->
                    task.dependsOn(capybaraTestBuildRequested.get() ? compileCapybara : generateTestCapybaraJava));

            var testCapybara = project.getTasks().register(
                    "testCapybara",
                    JavaExec.class,
                    task -> {
                        task.setGroup("verification");
                        task.setDescription("Runs Capybara tests using generated Java classes.");
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
