package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.CompiledType;

public record CompiledIfExpression(CompiledExpression condition,
                                 CompiledExpression thenBranch,
                                 CompiledExpression elseBranch,
                                 CompiledType type) implements CompiledExpression {
}
