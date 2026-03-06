package pl.grzeslowski.capybara.parser;

import java.util.Optional;

public record IntValue(String intValue, Optional<SourcePosition> position) implements Expression {
}


