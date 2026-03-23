package pl.grzeslowski.capybara.compiler.parser;

import java.util.Optional;

public record BooleanValue(boolean value, Optional<SourcePosition> position) implements Expression {
}

