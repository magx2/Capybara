package pl.grzeslowski.capybara.parser;

import java.util.Optional;

public record LetExpression(String name, Optional<Type> declaredType, Expression value, Expression rest,
                            Optional<SourcePosition> position) implements Expression {
}

