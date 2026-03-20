package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.CompiledType;

import java.util.List;

public record CompiledNewData(CompiledType type, List<FieldAssignment> assignments) implements CompiledExpression {
    public record FieldAssignment(String name, CompiledExpression value) {
    }
}
