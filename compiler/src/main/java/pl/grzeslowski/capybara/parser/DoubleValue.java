package pl.grzeslowski.capybara.parser;

import java.util.Optional;

public record DoubleValue(String doubleValue, Optional<SourcePosition> position) implements Expression {
}

