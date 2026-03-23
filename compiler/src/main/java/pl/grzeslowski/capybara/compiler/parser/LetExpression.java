package pl.grzeslowski.capybara.compiler.parser;

import java.util.Optional;

public record LetExpression(String name, Optional<Type> declaredType, Expression value, Expression rest,
                            Optional<SourcePosition> position) implements Expression {
}

