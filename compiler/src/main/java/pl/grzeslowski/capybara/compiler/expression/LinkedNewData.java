package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.LinkedType;

import java.util.List;

public record LinkedNewData(LinkedType type, List<FieldAssignment> assignments) implements LinkedExpression {
    public record FieldAssignment(String name, LinkedExpression value) {
    }
}
