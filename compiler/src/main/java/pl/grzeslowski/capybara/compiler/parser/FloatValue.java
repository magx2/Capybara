package pl.grzeslowski.capybara.compiler.parser;

import java.util.Optional;

public record FloatValue(String floatValue, Optional<SourcePosition> position) implements Expression {
}


