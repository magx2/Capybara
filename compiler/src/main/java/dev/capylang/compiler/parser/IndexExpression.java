package dev.capylang.compiler.parser;

import java.util.List;
import java.util.Optional;

public record IndexExpression(
        Expression source,
        List<Expression> arguments,
        Optional<SourcePosition> position
) implements Expression {
    public IndexExpression(Expression source, Expression index, Optional<SourcePosition> position) {
        this(source, List.of(index), position);
    }

    public Expression index() {
        return arguments.getFirst();
    }
}
