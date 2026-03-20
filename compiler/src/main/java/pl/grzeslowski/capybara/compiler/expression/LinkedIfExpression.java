package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.LinkedType;

public record LinkedIfExpression(LinkedExpression condition,
                                 LinkedExpression thenBranch,
                                 LinkedExpression elseBranch,
                                 LinkedType type) implements LinkedExpression {
}
