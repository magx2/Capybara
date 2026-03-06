package pl.grzeslowski.capybara.parser;

import java.util.List;
import java.util.Optional;

public record NewData(Type type, List<FieldAssignment> assignments, Optional<SourcePosition> position) implements Expression {
    public record FieldAssignment(String name, Expression value) {
    }
}


