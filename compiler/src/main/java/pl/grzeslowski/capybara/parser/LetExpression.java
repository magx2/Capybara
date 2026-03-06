package pl.grzeslowski.capybara.parser;

import java.util.Optional;

public record LetExpression(String name, Expression value, Expression rest,
                            Optional<SourcePosition> position) implements Expression {
}


