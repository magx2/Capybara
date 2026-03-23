package pl.grzeslowski.capybara.compiler.parser;

import java.util.List;
import java.util.Optional;

public record NewSetExpression(List<Expression> values, Optional<SourcePosition> position) implements Expression {
}
