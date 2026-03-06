package pl.grzeslowski.capybara.parser;

import java.util.Optional;

public record BooleanValue(boolean value, Optional<SourcePosition> position) implements Expression {
}

