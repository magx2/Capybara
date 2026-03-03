package pl.grzeslowski.capybara.generator;

import java.nio.file.Path;

public record CompiledModule(Path relativePath, String code) {
    public CompiledModule {
        var fileName = relativePath.getFileName().toString();
        if (!fileName.endsWith(".java")
            && !fileName.endsWith(".js")
            && !fileName.endsWith(".py")) {
            throw new IllegalArgumentException("Module name must end with .java, .js or .py");
        }
    }
}
