package pl.grzeslowski.capybara.parser;

import java.util.Optional;

public record FloatValue(String floatValue, Optional<SourcePosition> position) implements Expression {
}


