package pl.grzeslowski.capybara.parser;

import java.util.List;

public record NewData(Type type, List<FieldAssignment> assignments) implements Expression {
    @Override
    public Expression value() {
        // todo implement it
        return null;
    }

    public record FieldAssignment(String name, Expression value) {
    }
}
