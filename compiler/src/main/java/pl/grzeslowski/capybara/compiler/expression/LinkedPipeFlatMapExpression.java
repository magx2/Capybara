package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.LinkedType;

public record LinkedPipeFlatMapExpression(LinkedExpression source,
                                          String argumentName,
                                          LinkedExpression mapper,
                                          LinkedType type) implements LinkedExpression {
}
