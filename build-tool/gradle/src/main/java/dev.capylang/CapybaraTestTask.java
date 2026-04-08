package dev.capylang;

import org.gradle.api.DefaultTask;
import org.gradle.api.GradleException;
import org.gradle.api.file.ConfigurableFileCollection;
import org.gradle.api.file.DirectoryProperty;
import org.gradle.api.provider.Property;
import org.gradle.api.tasks.Classpath;
import org.gradle.api.tasks.Input;
import org.gradle.api.tasks.OutputDirectory;
import org.gradle.api.tasks.TaskAction;

import java.lang.reflect.Method;
import java.net.URL;
import java.net.URLClassLoader;

public abstract class CapybaraTestTask extends DefaultTask {
    @Classpath
    public abstract ConfigurableFileCollection getRuntimeClasspath();

    @OutputDirectory
    public abstract DirectoryProperty getOutputDir();

    @Input
    public abstract Property<String> getReportType();

    @Input
    public abstract Property<String> getLogLevel();

    @TaskAction
    public void runCapybaraTests() throws Exception {
        var outputDir = getOutputDir().get().getAsFile();
        outputDir.mkdirs();

        URL[] runtimeUrls = getRuntimeClasspath().getFiles().stream()
                .map(file -> {
                    try {
                        return file.toURI().toURL();
                    } catch (java.net.MalformedURLException e) {
                        throw new IllegalStateException("Unable to resolve runtime classpath URL for " + file, e);
                    }
                })
                .toArray(URL[]::new);

        var originalContextClassLoader = Thread.currentThread().getContextClassLoader();
        try (var classLoader = new URLClassLoader(runtimeUrls, ClassLoader.getPlatformClassLoader())) {
            Thread.currentThread().setContextClassLoader(classLoader);
            var testRunnerClass = Class.forName("dev.capylang.test.TestRunner", true, classLoader);
            var argumentsClass = Class.forName("dev.capylang.test.TestRunner$Arguments", true, classLoader);
            Method parseArguments = testRunnerClass.getMethod("parseArguments", String[].class);
            Method runTests = testRunnerClass.getMethod("runTests", argumentsClass);

            var logLevel = getLogLevel().getOrElse("INFO");
            var args = logLevel.isBlank()
                    ? new String[]{
                    "-o", outputDir.getAbsolutePath(),
                    "-rt", getReportType().get()
            }
                    : new String[]{
                    "-o", outputDir.getAbsolutePath(),
                    "-rt", getReportType().get(),
                    "-ll", logLevel
            };

            var parsedArguments = parseArguments.invoke(null, (Object) args);
            var exitCode = (Integer) runTests.invoke(null, parsedArguments);
            if (exitCode != 0) {
                throw new GradleException("Capybara tests failed with exit code " + exitCode);
            }
        } finally {
            Thread.currentThread().setContextClassLoader(originalContextClassLoader);
        }
    }
}
