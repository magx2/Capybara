package dev.capylang;

import org.gradle.api.Plugin;
import org.gradle.api.Project;
import org.gradle.api.logging.LogLevel;
import org.gradle.api.tasks.SourceSetContainer;

public class CapybaraPlugin implements Plugin<Project> {
    @Override
    public void apply(Project project) {
        var layout = project.getLayout();
        var compilerVersion = project.getVersion().toString();
        var capybaraTestResultsDir = layout.getBuildDirectory().dir("test-results/capybara");
        var hasJvmMainSources = project.provider(() ->
                project.fileTree(project.file("src/main"), spec -> {
                    spec.include("**/*.java");
                    spec.include("**/*.kt");
                    spec.include("**/*.kts");
                    spec.include("**/*.groovy");
                    spec.exclude("capybara/**");
                }).getFiles().stream().findAny().isPresent()
        );
        var hasMainResources = project.provider(() ->
                project.fileTree(project.file("src/main/resources")).getFiles().stream().findAny().isPresent()
        );
        var hasJvmTestSources = project.provider(() ->
                project.fileTree(project.file("src/test"), spec -> {
                    spec.include("**/*.java");
                    spec.include("**/*.kt");
                    spec.include("**/*.kts");
                    spec.include("**/*.groovy");
                    spec.exclude("capybara/**");
                }).getFiles().stream().findAny().isPresent()
        );
        var hasNonJvmTestResources = project.provider(() ->
                project.fileTree(project.file("src/test/resources"), spec ->
                        spec.exclude("junit-platform.properties")
                ).getFiles().stream().findAny().isPresent()
        );
        var capybaraTestBuildRequested = project.provider(() ->
                project.getGradle().getStartParameter().getTaskNames().stream().anyMatch(taskName ->
                        taskName.equals("build") || taskName.endsWith(":build") ||
                                taskName.equals("buildNeeded") || taskName.endsWith(":buildNeeded") ||
                                taskName.equals("buildDependents") || taskName.endsWith(":buildDependents") ||
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
            var mainSourceSet = sourceSets.getByName("main");
            var testSourceSet = sourceSets.getByName("test");

            sourceSets.named("main", sourceSet ->
                    sourceSet.getJava().srcDir(layout.getBuildDirectory().dir("generated/sources/capybara/java")));
            sourceSets.named("test", sourceSet -> {
                sourceSet.getJava().srcDir(layout.getBuildDirectory().dir("generated/sources/test-capybara/java"));
                if (capybaraTestBuildRequested.get() && !hasJvmMainSources.get()) {
                    sourceSet.getJava().srcDir(layout.getBuildDirectory().dir("generated/sources/capybara/java"));
                    sourceSet.setCompileClasspath(sourceSet.getCompileClasspath().minus(mainSourceSet.getOutput()));
                    sourceSet.setRuntimeClasspath(sourceSet.getRuntimeClasspath().minus(mainSourceSet.getOutput()));
                }
            });
            project.getTasks().named("compileJava", task -> {
                var enabled = !capybaraTestBuildRequested.get() || hasJvmMainSources.get();
                task.setEnabled(enabled);
                if (!enabled) {
                    task.setDependsOn(java.util.List.of());
                }
            });
            project.getTasks().named("processResources", task -> task.setEnabled(hasMainResources.get()));
            project.getTasks().named("processTestResources", task ->
                    task.setEnabled(hasJvmTestSources.get() || hasNonJvmTestResources.get()));
            project.getTasks().named("classes", task -> {
                var enabled = hasJvmMainSources.get() || hasMainResources.get();
                task.setEnabled(enabled);
                if (!enabled) {
                    task.setDependsOn(java.util.List.of());
                }
            });
            project.getTasks().named("testClasses", task -> {
                var enabled = hasJvmTestSources.get() || hasNonJvmTestResources.get();
                task.setEnabled(enabled);
                if (!enabled) {
                    task.setDependsOn(java.util.List.of());
                }
            });
            project.getTasks().named("compileTestJava", task -> {
                if (capybaraTestBuildRequested.get() && !hasJvmMainSources.get()) {
                    task.setDependsOn(task.getDependsOn().stream()
                            .filter(dependency -> {
                                if (dependency instanceof org.gradle.api.tasks.TaskProvider<?> provider) {
                                    return !provider.getName().equals("classes")
                                            && !provider.getName().equals("compileJava")
                                            && !provider.getName().equals("processResources");
                                }
                                if (dependency instanceof org.gradle.api.Task dependencyTask) {
                                    return !dependencyTask.getName().equals("classes")
                                            && !dependencyTask.getName().equals("compileJava")
                                            && !dependencyTask.getName().equals("processResources");
                                }
                                return !"classes".equals(dependency)
                                        && !"compileJava".equals(dependency)
                                        && !"processResources".equals(dependency);
                            })
                            .toList());
                }
                task.dependsOn(capybaraTestBuildRequested.get() ? compileCapybara : generateTestCapybaraJava);
            });

            var testCapybara = project.getTasks().register(
                    "testCapybara",
                    CapybaraTestTask.class,
                    task -> {
                        task.setGroup("verification");
                        task.setDescription("Runs Capybara tests using generated Java classes without launching a separate JVM.");
                        task.dependsOn(project.getTasks().named("compileTestJava"));
                        var mainRuntimeClasspath = capybaraTestBuildRequested.get() && !hasJvmMainSources.get()
                                ? mainSourceSet.getRuntimeClasspath().minus(mainSourceSet.getOutput())
                                : mainSourceSet.getRuntimeClasspath();
                        task.getRuntimeClasspath().from(mainRuntimeClasspath, testSourceSet.getOutput().getClassesDirs());
                        task.getOutputDir().set(capybaraTestResultsDir);
                        task.getReportType().set("JUNIT");
                        task.getLogLevel().set(capybaraTestLogLevel(project.getGradle().getStartParameter().getLogLevel()));
                    }
            );

            project.getTasks().matching(task -> task.getName().equals("test")).configureEach(task -> {
                task.dependsOn(testCapybara);
                if (task.hasProperty("failOnNoDiscoveredTests")) {
                    task.setProperty("failOnNoDiscoveredTests", false);
                }
                task.setEnabled(hasJvmTestSources.get());
                if (!hasJvmTestSources.get()) {
                    task.setDependsOn(java.util.List.of());
                }
            });
            project.getTasks().matching(task -> task.getName().equals("check")).configureEach(task -> {
                if (!hasJvmTestSources.get()) {
                    task.setDependsOn(task.getDependsOn().stream()
                            .filter(dependency -> {
                                if (dependency instanceof org.gradle.api.tasks.TaskProvider<?> provider) {
                                    return !provider.getName().equals("test");
                                }
                                if (dependency instanceof org.gradle.api.Task dependencyTask) {
                                    return !dependencyTask.getName().equals("test");
                                }
                                return !"test".equals(dependency);
                            })
                            .toList());
                }
                task.dependsOn(testCapybara);
            });
        }
    }

    static String capybaraTestLogLevel(LogLevel gradleLogLevel) {
        return switch (gradleLogLevel) {
            case DEBUG -> "DEBUG";
            case INFO -> "INFO";
            default -> "WARN";
        };
    }
}
