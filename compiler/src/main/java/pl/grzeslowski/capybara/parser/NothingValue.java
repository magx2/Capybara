package pl.grzeslowski.capybara.parser;

import java.util.Optional;

public record NothingValue(Optional<SourcePosition> position) implements Expression {
}

