package dev.capylang.compiler.parser;

import java.util.List;
import java.util.Optional;

public record NewData(Type type, boolean bypassConstructor, List<FieldAssignment> assignments,
                      List<Expression> positionalArguments,
                      List<Expression> spreads,
                      Optional<SourcePosition> position) implements Expression {
    public record FieldAssignment(String name, Expression value) {
    }
}
