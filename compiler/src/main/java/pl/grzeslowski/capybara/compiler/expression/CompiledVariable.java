package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.CompiledType;

public record CompiledVariable(String name, CompiledType type) implements CompiledExpression {
}
