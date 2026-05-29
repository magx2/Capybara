package dev.capylang.compiler.parser;

import dev.capylang.compiler.parser.ParserAst.AnnotationValue;

import java.util.Optional;

public record AnnotationArgument(
        String name,
        AnnotationValue value,
        Optional<SourcePosition> position
) {
}
