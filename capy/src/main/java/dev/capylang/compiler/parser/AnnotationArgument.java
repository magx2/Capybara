package dev.capylang.compiler.parser;

import java.util.Optional;

public record AnnotationArgument(
        String name,
        AnnotationValue value,
        Optional<SourcePosition> position
) {
}
