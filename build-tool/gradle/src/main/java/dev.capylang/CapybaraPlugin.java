package dev.capylang;

import org.gradle.api.Plugin;
import org.gradle.api.Project;
import org.gradle.api.logging.LogLevel;
import org.gradle.api.tasks.SourceSetContainer;
import org.gradle.api.tasks.testing.Test;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.function.Consumer;

public class CapybaraPlugin implements Plugin<Project> {
    @Override
    public void apply(Project project) {
        var layout = project.getLayout();
        var compilerVersion = project.getVersion().toString();
        var capybaraTestResultsDir = layout.getBuildDirectory().dir("test-results/capybara");
        var generatedMainJavaDir = layout.getBuildDirectory().dir("generated/sources/capybara/java");
        var generatedTestJavaDir = layout.getBuildDirectory().dir("generated/sources/test-capybara/java");
        var generatedCheckJavaDir = layout.getBuildDirectory().dir("generated/sources/capybara/java/check");
        var mainSourceLayout = inspectSourceLayout(project.file("src/main").toPath(), false);
        var testSourceLayout = inspectSourceLayout(project.file("src/test").toPath(), true);
        var hasJvmMainSources = mainSourceLayout.hasJvmSources();
        var hasMainResources = mainSourceLayout.hasAnyResources();
        var hasJvmTestSources = testSourceLayout.hasJvmSources();
        var hasAnyTestResources = testSourceLayout.hasAnyResources();
        var hasNonJvmTestResources = testSourceLayout.hasNonJvmResources();
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
                    task.getGeneratedOutputDir().set(singleJavaVerificationBuild ? generatedCheckJavaDir : generatedMainJavaDir);
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
                task.dependsOn(capybaraTestBuildRequested ? compileCapybara : generateCapybaraJava));

        var compileTestCapybara = project.getTasks().register(
                "compileTestCapybara",
                CompileCapybaraTask.class,
                task -> {
                    task.setGroup("verification");
                    task.setDescription("Compiles Capybara files from src/test/capybara.");
                    task.dependsOn(compileCapybara);
                    task.getInputDir().set(project.file("src/test/capybara"));
                    task.getGeneratedOutputDir().set(generatedTestJavaDir);
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

            project.getTasks().named("test", Test.class, task -> {
                task.dependsOn(testCapybara);
                task.setScanForTestClasses(false);
                task.include("**/*Test.class", "**/*Tests.class", "**/*IT.class", "**/*IntegrationTest.class");
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

    private static SourceLayout inspectSourceLayout(Path root, boolean includeResources) {
        var hasJvmSources = new boolean[1];
        var hasAnyResources = new boolean[1];
        var hasNonJvmResources = new boolean[1];
        scanSourceTree(root, relativePath -> {
            if (!hasJvmSources[0] && !relativePath.startsWith("capybara/") && hasJvmSourceExtension(relativePath)) {
                hasJvmSources[0] = true;
            }
            if (includeResources && relativePath.startsWith("resources/")) {
                hasAnyResources[0] = true;
                if (!relativePath.equals("resources/junit-platform.properties")) {
                    hasNonJvmResources[0] = true;
                }
            }
        });
        return new SourceLayout(hasJvmSources[0], hasAnyResources[0], hasNonJvmResources[0]);
    }

    private static void scanSourceTree(Path root, Consumer<String> relativePathVisitor) {
        if (Files.notExists(root) || !Files.isDirectory(root)) {
            return;
        }

        try {
            Files.walkFileTree(root, new SimpleFileVisitor<>() {
                @Override
                public FileVisitResult visitFile(Path path, BasicFileAttributes attributes) {
                    if (attributes.isRegularFile()) {
                        relativePathVisitor.accept(normalizeRelativePath(root, path));
                    }
                    return FileVisitResult.CONTINUE;
                }
            });
        } catch (IOException e) {
            throw new UncheckedIOException("Unable to inspect project sources under " + root, e);
        }
    }

    private static String normalizeRelativePath(Path root, Path path) {
        return root.relativize(path).toString().replace('\\', '/');
    }

    private record SourceLayout(boolean hasJvmSources, boolean hasAnyResources, boolean hasNonJvmResources) {
    }
}
