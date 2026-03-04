package pl.grzeslowski.capybara.linker.expression;

import pl.grzeslowski.capybara.linker.LinkedType;
import pl.grzeslowski.capybara.parser.InfixOperator;

public record LinkedInfixExpression(LinkedExpression left,
                                    InfixOperator operator,
                                    LinkedExpression right,
                                    LinkedType type) implements LinkedExpression {
}
