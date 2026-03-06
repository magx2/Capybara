package pl.grzeslowski.capybara.parser;

import java.util.List;
import java.util.Optional;

public record FunctionCall(String name, List<Expression> arguments, Optional<SourcePosition> position) implements Expression {
}


