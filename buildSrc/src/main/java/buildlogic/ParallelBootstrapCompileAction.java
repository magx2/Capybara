package buildlogic;

import org.gradle.api.Action;
import org.gradle.api.Task;
import org.gradle.api.file.DirectoryProperty;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;

/** Compiles Capybara sources and generates bootstrap Java output in parallel. */
public final class ParallelBootstrapCompileAction implements Action<Task> {
    @Override
    public void execute(Task task) {
        try {
            var inputProperty = (DirectoryProperty) task.getClass()
                    .getMethod("getInputDir")
                    .invoke(task);
            var outputProperty = (DirectoryProperty) task.getClass()
                    .getMethod("getGeneratedOutputDir")
                    .invoke(task);
            var input = inputProperty.get().getAsFile().toPath();
            var output = outputProperty.get().getAsFile().toPath();
            clearDirectory(output);
            ParallelBootstrapJavaGenerator.compileAndGenerate(
                    task.getClass().getClassLoader(),
                    input,
                    output
            );
        } catch (ReflectiveOperationException | IOException exception) {
            throw new IllegalStateException("Unable to run parallel bootstrap Java generation.", exception);
        }
    }

    private static void clearDirectory(Path directory) throws IOException {
        if (!Files.exists(directory)) {
            return;
        }
        try (var paths = Files.walk(directory)) {
            for (var path : paths.sorted(Comparator.reverseOrder()).toList()) {
                Files.delete(path);
            }
        }
    }
}
