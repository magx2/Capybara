package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.LinkedType;

public record LinkedPipeFilterOutExpression(LinkedExpression source,
                                            String argumentName,
                                            LinkedExpression predicate,
                                            LinkedType type) implements LinkedExpression {
}
