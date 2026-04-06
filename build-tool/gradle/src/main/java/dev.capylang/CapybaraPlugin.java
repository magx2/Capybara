package dev.capylang;

import org.gradle.api.Plugin;
import org.gradle.api.Project;
import org.gradle.api.logging.LogLevel;
import org.gradle.api.tasks.SourceSetContainer;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.function.Predicate;

public class CapybaraPlugin implements Plugin<Project> {
    @Override
    public void apply(Project project) {
        var layout = project.getLayout();
        var compilerVersion = project.getVersion().toString();
        var capybaraTestResultsDir = layout.getBuildDirectory().dir("test-results/capybara");
        var hasJvmMainSources = containsMatchingFile(project.file("src/main").toPath(), relativePath ->
                !relativePath.startsWith("capybara/") && hasJvmSourceExtension(relativePath)
        );
        var hasMainResources = containsMatchingFile(project.file("src/main/resources").toPath(), relativePath -> true);
        var hasJvmTestSources = containsMatchingFile(project.file("src/test").toPath(), relativePath ->
                !relativePath.startsWith("capybara/") && hasJvmSourceExtension(relativePath)
        );
        var hasNonJvmTestResources = containsMatchingFile(project.file("src/test/resources").toPath(), relativePath ->
                !relativePath.equals("junit-platform.properties")
        );
        var requestedTaskNames = project.getGradle().getStartParameter().getTaskNames();
        var capybaraTestBuildRequested = requestedTaskNames.stream().anyMatch(taskName ->
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
        );
        var singleJavaVerificationBuild = capybaraTestBuildRequested && !hasMainResources;

        var compileCapybara = project.getTasks().register(
                "compileCapybara",
                CompileCapybaraTask.class,
                task -> {
                    task.setGroup("build");
                    task.setDescription("Compiles Capybara files from src/main/capybara.");
                    task.getInputDir().set(project.file("src/main/capybara"));
                    task.getGeneratedOutputDir().set(layout.getBuildDirectory().dir("generated/sources/capybara/java"));
                    task.getAdditionalInputDirs().from();
                    task.getCompilerVersion().set(compilerVersion);
                    task.getCompileTests().set(false);
                    task.getIncludeJavaLibResources().set(true);
                    task.getCompileTestSourcesWithMainCompilation().set(singleJavaVerificationBuild);
                    task.getWriteLinkedOutput().set(!singleJavaVerificationBuild);
                    task.getIncludeJavaLibResourcesInTestOutput().set(false);
                    if (!singleJavaVerificationBuild) {
                        task.getOutputDir().set(layout.getBuildDirectory().dir("classes/capybara"));
                    }
                    if (singleJavaVerificationBuild) {
                        task.getTestInputDir().set(project.file("src/test/capybara"));
                        task.getGeneratedTestOutputDir().set(layout.getBuildDirectory().dir("generated/sources/test-capybara/java"));
                    }
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
                task.dependsOn(capybaraTestBuildRequested ? compileCapybara : generateCapybaraJava));

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
                    task.onlyIf(ignored -> !capybaraTestBuildRequested);
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
                if (singleJavaVerificationBuild) {
                    sourceSet.getJava().srcDir(layout.getBuildDirectory().dir("generated/sources/capybara/java"));
                    sourceSet.getJava().srcDir(project.file("src/main/java"));
                    sourceSet.setCompileClasspath(sourceSet.getCompileClasspath().minus(mainSourceSet.getOutput()));
                    sourceSet.setRuntimeClasspath(sourceSet.getRuntimeClasspath().minus(mainSourceSet.getOutput()));
                }
            });
            project.getTasks().named("compileJava", task -> {
                var enabled = !singleJavaVerificationBuild;
                task.setEnabled(enabled);
                if (!enabled) {
                    task.setDependsOn(java.util.List.of());
                }
            });
            project.getTasks().named("processResources", task -> task.setEnabled(hasMainResources));
            project.getTasks().named("processTestResources", task ->
                    task.setEnabled(hasJvmTestSources || hasNonJvmTestResources));
            project.getTasks().named("classes", task -> {
                var enabled = hasJvmMainSources || hasMainResources;
                task.setEnabled(enabled);
                if (!enabled) {
                    task.setDependsOn(java.util.List.of());
                }
            });
            project.getTasks().named("testClasses", task -> {
                var enabled = hasJvmTestSources || hasNonJvmTestResources;
                task.setEnabled(enabled);
                if (!enabled) {
                    task.setDependsOn(java.util.List.of());
                }
            });
            project.getTasks().named("compileTestJava", task -> {
                if (singleJavaVerificationBuild) {
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
                task.dependsOn(singleJavaVerificationBuild ? compileCapybara : generateTestCapybaraJava);
            });

            var testCapybara = project.getTasks().register(
                    "testCapybara",
                    CapybaraTestTask.class,
                    task -> {
                        task.setGroup("verification");
                        task.setDescription("Runs Capybara tests using generated Java classes without launching a separate JVM.");
                        task.dependsOn(project.getTasks().named("compileTestJava"));
                        var mainRuntimeClasspath = singleJavaVerificationBuild
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
                task.setEnabled(hasJvmTestSources);
                if (!hasJvmTestSources) {
                    task.setDependsOn(java.util.List.of());
                }
            });
            project.getTasks().matching(task -> task.getName().equals("check")).configureEach(task -> {
                if (!hasJvmTestSources) {
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

    private static boolean hasJvmSourceExtension(String relativePath) {
        return relativePath.endsWith(".java")
                || relativePath.endsWith(".kt")
                || relativePath.endsWith(".kts")
                || relativePath.endsWith(".groovy");
    }

    private static boolean containsMatchingFile(Path root, Predicate<String> relativePathMatcher) {
        if (Files.notExists(root) || !Files.isDirectory(root)) {
            return false;
        }

        try (var matchingFiles = Files.find(root, Integer.MAX_VALUE, (path, attributes) ->
                attributes.isRegularFile() && relativePathMatcher.test(normalizeRelativePath(root, path))
        )) {
            return matchingFiles.findAny().isPresent();
        } catch (IOException e) {
            throw new UncheckedIOException("Unable to inspect project sources under " + root, e);
        }
    }

    private static String normalizeRelativePath(Path root, Path path) {
        return root.relativize(path).toString().replace('\\', '/');
    }
}
