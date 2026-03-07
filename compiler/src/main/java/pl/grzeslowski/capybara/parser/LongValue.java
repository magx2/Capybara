package pl.grzeslowski.capybara.parser;

import java.util.Optional;

public record LongValue(String longValue, Optional<SourcePosition> position) implements Expression {
}

