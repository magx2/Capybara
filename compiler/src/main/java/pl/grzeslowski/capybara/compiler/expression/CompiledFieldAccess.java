package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.CompiledType;

public record CompiledFieldAccess(CompiledExpression source, String field, CompiledType type) implements CompiledExpression {
}
