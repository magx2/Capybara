package pl.grzeslowski.capybara.compiler.parser;

import java.util.List;
import java.util.Optional;

public record NewData(Type type, List<FieldAssignment> assignments,
                      List<Expression> positionalArguments,
                      List<Expression> spreads,
                      Optional<SourcePosition> position) implements Expression {
    public record FieldAssignment(String name, Expression value) {
    }
}
