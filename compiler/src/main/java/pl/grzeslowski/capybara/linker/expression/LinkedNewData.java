package pl.grzeslowski.capybara.linker.expression;

import pl.grzeslowski.capybara.linker.LinkedType;

import java.util.List;

public record LinkedNewData(LinkedType type, List<FieldAssignment> assignments) implements LinkedExpression {
    public record FieldAssignment(String name, LinkedExpression value) {
    }
}
