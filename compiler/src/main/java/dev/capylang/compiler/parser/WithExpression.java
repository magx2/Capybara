package dev.capylang.compiler.parser;

import java.util.List;
import java.util.Optional;

public record WithExpression(
        Expression source,
        List<NewData.FieldAssignment> assignments,
        Optional<SourcePosition> position
) implements Expression {
}

