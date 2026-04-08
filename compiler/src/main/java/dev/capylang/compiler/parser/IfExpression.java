package dev.capylang.compiler.parser;

import java.util.Optional;

public record IfExpression(Expression condition, Expression thenBranch,
                           Expression elseBranch, Optional<SourcePosition> position) implements Expression {
}


