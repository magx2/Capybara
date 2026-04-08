package dev.capylang.compiler.parser;

import java.util.Optional;

public record IndexExpression(
        Expression source,
        Expression index,
        Optional<SourcePosition> position
) implements Expression {
}

