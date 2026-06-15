package dev.capylang.compiler.parser;

import java.util.List;
import java.util.Optional;

public record TupleExpression(
        List<Expression> values,
        Optional<SourcePosition> position
) implements Expression {
}

