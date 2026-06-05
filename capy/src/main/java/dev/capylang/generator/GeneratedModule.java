package dev.capylang.generator;

import java.nio.file.Path;

public record GeneratedModule(Path relativePath, String code) {
    public GeneratedModule {
        var fileName = relativePath.getFileName().toString();
        if (!fileName.endsWith(".java")
            && !fileName.endsWith(".js")
            && !fileName.endsWith(".py")) {
            throw new IllegalArgumentException("Module name must end with .java, .js or .py");
        }
    }
}
