package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.CompiledType;

public record CompiledPipeFlatMapExpression(CompiledExpression source,
                                          String argumentName,
                                          CompiledExpression mapper,
                                          CompiledType type) implements CompiledExpression {
}
