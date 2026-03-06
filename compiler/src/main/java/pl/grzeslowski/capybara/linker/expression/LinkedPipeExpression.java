package pl.grzeslowski.capybara.linker.expression;

import pl.grzeslowski.capybara.linker.LinkedType;

public record LinkedPipeExpression(LinkedExpression source,
                                   String argumentName,
                                   LinkedExpression mapper,
                                   LinkedType type) implements LinkedExpression {
}
