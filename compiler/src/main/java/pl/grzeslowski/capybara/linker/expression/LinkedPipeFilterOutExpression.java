package pl.grzeslowski.capybara.linker.expression;

import pl.grzeslowski.capybara.linker.LinkedType;

public record LinkedPipeFilterOutExpression(LinkedExpression source,
                                            String argumentName,
                                            LinkedExpression predicate,
                                            LinkedType type) implements LinkedExpression {
}
