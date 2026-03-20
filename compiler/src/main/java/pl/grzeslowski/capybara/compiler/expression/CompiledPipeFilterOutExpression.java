package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.CompiledType;

public record CompiledPipeFilterOutExpression(CompiledExpression source,
                                            String argumentName,
                                            CompiledExpression predicate,
                                            CompiledType type) implements CompiledExpression {
}
