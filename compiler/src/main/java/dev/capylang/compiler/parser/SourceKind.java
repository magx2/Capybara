package dev.capylang.compiler.parser;

import java.nio.file.Path;
import java.util.Arrays;
import java.util.Optional;

public enum SourceKind {
    FUNCTIONAL(".cfun"),
    OBJECT_ORIENTED(".coo");

    private final String extension;

    SourceKind(String extension) {
        this.extension = extension;
    }

    public String extension() {
        return extension;
    }

    public String fileName(String moduleName) {
        return moduleName + extension;
    }

    public String moduleFile(String path, String moduleName) {
        var normalizedPath = path.replace('\\', '/');
        if (!normalizedPath.startsWith("/")) {
            normalizedPath = "/" + normalizedPath;
        }
        return normalizedPath + "/" + fileName(moduleName);
    }

    public static Optional<SourceKind> fromPath(Path path) {
        return fromFileName(path.getFileName().toString());
    }

    public static Optional<SourceKind> fromFileName(String fileName) {
        return Arrays.stream(values())
                .filter(kind -> fileName.endsWith(kind.extension))
                .findFirst();
    }

    public static String stripKnownExtension(String fileName) {
        return fromFileName(fileName)
                .map(kind -> fileName.substring(0, fileName.length() - kind.extension.length()))
                .orElse(fileName);
    }
}
