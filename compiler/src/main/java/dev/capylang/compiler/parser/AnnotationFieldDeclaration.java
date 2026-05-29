package dev.capylang.compiler.parser;

import dev.capylang.compiler.parser.ParserAst.AnnotationValue;

import java.util.Optional;

public record AnnotationFieldDeclaration(
        String name,
        String type,
        Optional<AnnotationValue> defaultValue,
        Optional<SourcePosition> position
) {
}
