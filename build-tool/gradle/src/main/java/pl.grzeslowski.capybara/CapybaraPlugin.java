package pl.grzeslowski.capybara;

import org.gradle.api.DefaultTask;
import org.gradle.api.Plugin;
import org.gradle.api.Project;
import org.gradle.api.file.DirectoryProperty;
import org.gradle.api.tasks.InputDirectory;
import org.gradle.api.tasks.OutputDirectory;
import org.gradle.api.tasks.TaskAction;
import org.gradle.api.tasks.TaskProvider;

public class CapybaraPlugin implements Plugin<Project> {
    @Override
    public void apply(Project project) {
        var layout = project.getLayout();

        // main
        TaskProvider<CompileCapybaraTask> compileCapybara = project.getTasks().register(
                "compileCapybara",
                CompileCapybaraTask.class,
                task -> {
                    task.setGroup("build");
                    task.setDescription("Compiles Capybara files from src/main/capybara.");
                    task.getInputDir().set(project.file("src/main/capybara"));
                    task.getOutputDir().set(layout.getBuildDirectory().dir("classes/capybara"));
                }
        );

        TaskProvider<GenerateCapybaraJavaTask> generateCapybaraJava = project.getTasks().register(
                "generateCapybaraJava",
                GenerateCapybaraJavaTask.class,
                task -> {
                    task.setGroup("build");
                    task.setDescription("Generates Java classes");
                    task.dependsOn(compileCapybara);
                    task.getInputDir().set(layout.getBuildDirectory().dir("classes/capybara"));
                    task.getOutputDir().set(layout.getBuildDirectory().dir("generated/sources/capybara"));
                }
        );

        project.getTasks().named("compileJava", task -> task.dependsOn(generateCapybaraJava));

        // test
        TaskProvider<CompileCapybaraTask> compileTestCapybara = project.getTasks().register(
                "compileTestCapybara",
                CompileCapybaraTask.class,
                task -> {
                    task.setGroup("verification");
                    task.setDescription("Compiles Capybara files from src/test/capybara.");
                    task.getInputDir().set(project.file("src/test/capybara"));
                    task.getOutputDir().set(layout.getBuildDirectory().dir("classes/test-capybara"));
                }
        );

        TaskProvider<GenerateCapybaraJavaTask> generateTestCapybaraJava = project.getTasks().register(
                "generateTestCapybaraJava",
                GenerateCapybaraJavaTask.class,
                task -> {
                    task.setGroup("verification");
                    task.setDescription("Generates Java classes for tests");
                    task.dependsOn(compileTestCapybara);
                    task.getInputDir().set(layout.getBuildDirectory().dir("classes/test-capybara"));
                    task.getOutputDir().set(layout.getBuildDirectory().dir("generated/sources/test-capybara"));
                }
        );

        project.getTasks().named("compileTestJava", task -> task.dependsOn(generateTestCapybaraJava));
    }

    public abstract static class CompileCapybaraTask extends DefaultTask {
        @InputDirectory
        public abstract DirectoryProperty getInputDir();

        @OutputDirectory
        public abstract DirectoryProperty getOutputDir();

        @TaskAction
        public void compile() {
            var input = getInputDir().get().getAsFile();
            var output = getOutputDir().get().getAsFile();

            getLogger().lifecycle("Compiling Capybara from {} to {}", input, output);

            output.mkdirs();
            // TODO invoke Capybara compiler and write compiled artifacts to output
        }
    }

    public abstract static class GenerateCapybaraJavaTask extends DefaultTask {
        @InputDirectory
        public abstract DirectoryProperty getInputDir();

        @OutputDirectory
        public abstract DirectoryProperty getOutputDir();

        @TaskAction
        public void generate() {
            var input = getInputDir().get().getAsFile();
            var output = getOutputDir().get().getAsFile();

            getLogger().lifecycle("Generating Java from {} to {}", input, output);

            output.mkdirs();
            // TODO transform compiled Capybara artifacts into generated Java sources
        }
    }
}
