package dev.capylang.compiler.parser;

import java.util.List;
import java.util.Optional;

public record AnnotationUsage(
        String name,
        List<AnnotationArgument> arguments,
        Optional<SourcePosition> position
) {
}
