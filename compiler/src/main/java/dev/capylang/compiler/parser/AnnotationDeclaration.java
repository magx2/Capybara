package dev.capylang.compiler.parser;

import dev.capylang.compiler.Visibility;

import java.util.List;
import java.util.Optional;

public record AnnotationDeclaration(
        String name,
        List<AnnotationTarget> targets,
        List<AnnotationFieldDeclaration> fields,
        List<String> comments,
        Visibility visibility,
        Optional<SourcePosition> position,
        List<AnnotationUsage> annotations
) implements Definition {
    public AnnotationDeclaration(
            String name,
            List<AnnotationTarget> targets,
            List<AnnotationFieldDeclaration> fields,
            List<String> comments,
            Visibility visibility,
            Optional<SourcePosition> position
    ) {
        this(name, targets, fields, comments, visibility, position, List.of());
    }
}
