package pl.grzeslowski.capybara.compiler.parser;

import java.util.Optional;

public record InfixExpression(Expression left,
                              InfixOperator operator,
                              Expression right,
                              Optional<SourcePosition> position) implements Expression {
}


