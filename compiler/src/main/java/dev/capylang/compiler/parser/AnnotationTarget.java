package dev.capylang.compiler.parser;

import java.util.Optional;

public record AnnotationTarget(
        String name,
        Optional<SourcePosition> position
) {
}
