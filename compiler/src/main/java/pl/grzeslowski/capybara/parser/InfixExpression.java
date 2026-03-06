package pl.grzeslowski.capybara.parser;

import java.util.Optional;

public record InfixExpression(Expression left,
                              InfixOperator operator,
                              Expression right,
                              Optional<SourcePosition> position) implements Expression {
}


