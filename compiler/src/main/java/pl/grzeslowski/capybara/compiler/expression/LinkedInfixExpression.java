package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.LinkedType;
import pl.grzeslowski.capybara.parser.InfixOperator;

public record LinkedInfixExpression(LinkedExpression left,
                                    InfixOperator operator,
                                    LinkedExpression right,
                                    LinkedType type) implements LinkedExpression {
}
