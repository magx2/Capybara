package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.CompiledType;

public record CompiledPipeExpression(CompiledExpression source,
                                   String argumentName,
                                   CompiledExpression mapper,
                                   CompiledType type) implements CompiledExpression {
}
