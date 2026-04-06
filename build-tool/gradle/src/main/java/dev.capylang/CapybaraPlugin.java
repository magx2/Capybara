package dev.capylang;

import org.gradle.api.Plugin;
import org.gradle.api.Project;
import org.gradle.api.logging.LogLevel;
import org.gradle.api.tasks.SourceSetContainer;
import org.gradle.api.tasks.testing.Test;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Stream;

public class CapybaraPlugin implements Plugin<Project> {
    private static final List<String> JVM_SOURCE_DIRECTORIES = List.of("java", "kotlin", "kts", "groovy");
    private static final Set<String> CAPYBARA_TEST_BUILD_TASKS = Set.of(
            "build",
            "buildNeeded",
            "buildDependents",
            "check",
            "test",
            "testClasses",
            "compileTestJava",
            "compileTestCapybara",
            "generateTestCapybaraJava",
            "testCapybara"
    );
    private static final Set<String> SINGLE_JAVA_VERIFICATION_TASKS = Set.of(
            "build",
            "buildNeeded",
            "buildDependents",
            "check",
            "test",
            "testClasses",
            "compileTestJava",
            "compileTestCapybara",
            "generateTestCapybaraJava",
            "testCapybara"
    );

    @Override
    public void apply(Project project) {
        var layout = project.getLayout();
        var compilerVersion = project.getVersion().toString();
        var capybaraTestResultsDir = layout.getBuildDirectory().dir("test-results/capybara");
        var generatedMainJavaDir = layout.getBuildDirectory().dir("generated/sources/capybara/java");
        var generatedTestJavaDir = layout.getBuildDirectory().dir("generated/sources/test-capybara/java");
        var generatedCheckJavaDir = layout.getBuildDirectory().dir("generated/sources/capybara/java/check");
        var hasCapybaraTestSources = hasMatchingFile(project.file("src/test/capybara").toPath(), relativePath -> true);
        var hasJvmMainSources = hasJvmSources(project.file("src/main").toPath());
        var hasMainResources = hasMatchingFile(project.file("src/main/resources").toPath(), relativePath -> true);
        var hasJvmTestSources = hasJvmSources(project.file("src/test").toPath());
        var testResources = inspectResourceDirectory(project.file("src/test/resources").toPath());
        var hasAnyTestResources = testResources.hasAnyFiles();
        var hasNonJvmTestResources = testResources.hasNonJvmFiles();
        var requestedTaskBasenames = project.getGradle().getStartParameter().getTaskNames().stream()
                .map(CapybaraPlugin::taskBasename)
                .collect(java.util.stream.Collectors.toSet());
        var capybaraTestBuildRequested = requestedTaskBasenames.stream().anyMatch(CAPYBARA_TEST_BUILD_TASKS::contains);
        var singleJavaVerificationBuild = !hasMainResources
                && requestedTaskBasenames.stream().anyMatch(SINGLE_JAVA_VERIFICATION_TASKS::contains);

        var compileCapybara = project.getTasks().register(
                "compileCapybara",
                CompileCapybaraTask.class,
                task -> {
                    task.setGroup("build");
                    task.setDescription("Compiles Capybara files from src/main/capybara.");
                    task.getInputDir().set(project.file("src/main/capybara"));
                    task.getGeneratedOutputDir().set(singleJavaVerificationBuild ? generatedCheckJavaDir : generatedMainJavaDir);
                    task.getLibraryProgramFiles().from();
                    task.getCompilerVersion().set(compilerVersion);
                    task.getCompileTests().set(false);
                    task.getIncludeJavaLibResources().set(true);
                    task.getCompileTestSourcesWithMainCompilation().set(singleJavaVerificationBuild);
                    task.getWriteLinkedOutput().set(!singleJavaVerificationBuild);
                    task.getIncludeJavaLibResourcesInTestOutput().set(false);
                    if (!singleJavaVerificationBuild) {
                        task.getOutputDir().set(layout.getBuildDirectory().dir("classes/capybara"));
                    }
                    if (singleJavaVerificationBuild && hasCapybaraTestSources) {
                        task.getTestInputDir().set(project.file("src/test/capybara"));
                        task.getGeneratedTestOutputDir().set(generatedCheckJavaDir);
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
                task.dependsOn(compileCapybara));

        var compileTestCapybara = project.getTasks().register(
                "compileTestCapybara",
                CompileCapybaraTask.class,
                task -> {
                    task.setGroup("verification");
                    task.setDescription("Compiles Capybara files from src/test/capybara.");
                    task.dependsOn(compileCapybara);
                    task.getInputDir().set(project.file("src/test/capybara"));
                    task.getGeneratedOutputDir().set(generatedTestJavaDir);
                    task.getLibraryProgramFiles().from(layout.getBuildDirectory().file("classes/capybara/program.json"));
                    task.getCompilerVersion().set(compilerVersion);
                    task.getCompileTests().set(true);
                    task.getIncludeJavaLibResources().set(false);
                    task.getCompileTestSourcesWithMainCompilation().set(false);
                    task.getWriteLinkedOutput().set(false);
                    task.getIncludeJavaLibResourcesInTestOutput().set(false);
                    task.onlyIf(ignored -> hasCapybaraTestSources && !capybaraTestBuildRequested);
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
                    sourceSet.getJava().srcDir(generatedMainJavaDir));
            sourceSets.named("test", sourceSet -> {
                if (singleJavaVerificationBuild) {
                    sourceSet.getJava().srcDir(generatedCheckJavaDir);
                    if (hasJvmMainSources) {
                        sourceSet.getJava().srcDir(project.file("src/main/java"));
                    }
                    sourceSet.setCompileClasspath(sourceSet.getCompileClasspath().minus(mainSourceSet.getOutput()));
                    sourceSet.setRuntimeClasspath(sourceSet.getRuntimeClasspath().minus(mainSourceSet.getOutput()));
                } else {
                    sourceSet.getJava().srcDir(generatedTestJavaDir);
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
                    task.setEnabled(hasNonJvmTestResources || (hasJvmTestSources && hasAnyTestResources)));
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
                if (singleJavaVerificationBuild) {
                    task.dependsOn(compileCapybara);
                } else if (hasCapybaraTestSources) {
                    task.dependsOn(compileTestCapybara);
                }
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
                        task.setEnabled(hasCapybaraTestSources);
                        if (!hasCapybaraTestSources) {
                            task.setDependsOn(java.util.List.of());
                        }
                    }
            );

            project.getTasks().named("test", Test.class, task -> {
                if (hasCapybaraTestSources) {
                    task.dependsOn(testCapybara);
                }
                task.setScanForTestClasses(false);
                task.include("**/*Test.class", "**/*Tests.class", "**/*IT.class", "**/*IntegrationTest.class");
                task.exclude("capy/**");
                task.getReports().getHtml().getRequired().set(false);
                task.getReports().getJunitXml().getRequired().set(true);
                task.systemProperty("junit.jupiter.execution.parallel.enabled", "true");
                task.systemProperty("junit.jupiter.execution.parallel.mode.default", "concurrent");
                task.systemProperty("junit.jupiter.execution.parallel.mode.classes.default", "concurrent");
                task.systemProperty("junit.jupiter.execution.parallel.config.strategy", "dynamic");
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
                if (hasCapybaraTestSources) {
                    task.dependsOn(testCapybara);
                }
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

    private static boolean hasJvmSources(Path sourceRoot) {
        return JVM_SOURCE_DIRECTORIES.stream()
                .anyMatch(directory -> hasMatchingFile(sourceRoot.resolve(directory), relativePath -> true));
    }

    private static boolean hasMatchingFile(Path root, Predicate<String> relativePathMatcher) {
        if (Files.notExists(root) || !Files.isDirectory(root)) {
            return false;
        }

        try (Stream<Path> paths = Files.walk(root)) {
            return paths
                    .filter(Files::isRegularFile)
                    .map(path -> normalizeRelativePath(root, path))
                    .anyMatch(relativePathMatcher);
        } catch (IOException e) {
            throw new UncheckedIOException("Unable to inspect project sources under " + root, e);
        }
    }

    private static ResourceDirectoryState inspectResourceDirectory(Path root) {
        if (Files.notExists(root) || !Files.isDirectory(root)) {
            return new ResourceDirectoryState(false, false);
        }

        var hasAnyFiles = new boolean[1];
        var hasNonJvmFiles = new boolean[1];
        try (Stream<Path> paths = Files.walk(root)) {
            paths
                    .filter(Files::isRegularFile)
                    .map(path -> normalizeRelativePath(root, path))
                    .anyMatch(relativePath -> {
                        hasAnyFiles[0] = true;
                        if (!relativePath.equals("junit-platform.properties")) {
                            hasNonJvmFiles[0] = true;
                            return true;
                        }
                        return false;
                    });
            return new ResourceDirectoryState(hasAnyFiles[0], hasNonJvmFiles[0]);
        } catch (IOException e) {
            throw new UncheckedIOException("Unable to inspect project resources under " + root, e);
        }
    }

    private static String normalizeRelativePath(Path root, Path path) {
        return root.relativize(path).toString().replace('\\', '/');
    }

    private static String taskBasename(String taskName) {
        var separator = taskName.lastIndexOf(':');
        return separator >= 0 ? taskName.substring(separator + 1) : taskName;
    }

    private record ResourceDirectoryState(boolean hasAnyFiles, boolean hasNonJvmFiles) {
    }
}
