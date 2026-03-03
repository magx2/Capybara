package pl.grzeslowski.capybara.parser;

import java.util.List;

public record NewData(Type type, List<FieldAssignment> assignments) implements Expression {
    public record FieldAssignment(String name, Expression value) {
    }
}
