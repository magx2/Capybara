package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.CompiledType;

import java.util.List;

public record CompiledNewDict(List<Entry> entries, CompiledType type) implements CompiledExpression {
    public record Entry(CompiledExpression key, CompiledExpression value) {
    }
}
