package pl.grzeslowski.capybara.linker.expression;

import pl.grzeslowski.capybara.linker.LinkedType;

public record LinkedIfExpression(LinkedExpression condition,
                                 LinkedExpression thenBranch,
                                 LinkedExpression elseBranch,
                                 LinkedType type) implements LinkedExpression {
}
