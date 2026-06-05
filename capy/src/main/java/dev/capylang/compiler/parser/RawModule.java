package dev.capylang.compiler.parser;

public record RawModule(String name, String path, String input, SourceKind sourceKind) {
    public RawModule(String name, String path, String input) {
        this(name, path, input, SourceKind.FUNCTIONAL);
    }

    public String file() {
        return sourceKind.moduleFile(path, name);
    }
}
