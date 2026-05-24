package dev.capylang.compiler.parser;

import java.util.List;
import java.util.Optional;

public record EnumDeclaration(
        String name,
        List<String> values,
        List<String> comments,
        Optional<SourcePosition> position,
        List<AnnotationUsage> annotations
) implements Definition {
    public EnumDeclaration(String name, List<String> values, List<String> comments, Optional<SourcePosition> position) {
        this(name, values, comments, position, List.of());
    }
}
