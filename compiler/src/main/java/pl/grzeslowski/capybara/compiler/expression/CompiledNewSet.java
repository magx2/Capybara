package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.CompiledType;

import java.util.List;

public record CompiledNewSet(List<CompiledExpression> values, CompiledType type) implements CompiledExpression {
}
