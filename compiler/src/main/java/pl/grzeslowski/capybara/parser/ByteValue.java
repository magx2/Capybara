package pl.grzeslowski.capybara.parser;

import java.util.Optional;

public record ByteValue(String byteValue, Optional<SourcePosition> position) implements Expression {
}

