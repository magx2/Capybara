package dev.capylang.compiler.parser;

import java.util.Optional;

public record UnwrapExpression(Expression expression, Optional<SourcePosition> position) implements Expression {
}
