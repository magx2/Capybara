package dev.capylang.compiler.parser;

import java.util.List;
import java.util.Optional;

public record ConstructorData(
        List<NewData.FieldAssignment> assignments,
        List<Expression> positionalArguments,
        List<Expression> spreads,
        Optional<SourcePosition> position
) implements Expression {
}
