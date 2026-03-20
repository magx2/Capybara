package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.CompiledType;
import pl.grzeslowski.capybara.parser.InfixOperator;

public record CompiledInfixExpression(CompiledExpression left,
                                    InfixOperator operator,
                                    CompiledExpression right,
                                    CompiledType type) implements CompiledExpression {
}
