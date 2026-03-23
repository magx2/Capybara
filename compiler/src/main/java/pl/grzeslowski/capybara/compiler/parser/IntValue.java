package pl.grzeslowski.capybara.compiler.parser;

import java.util.Optional;

public record IntValue(String intValue, Optional<SourcePosition> position) implements Expression {
}


