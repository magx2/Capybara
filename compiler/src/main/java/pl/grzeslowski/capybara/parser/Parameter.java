package pl.grzeslowski.capybara.parser;

import java.util.Optional;

public record Parameter(Type type, String name, Optional<SourcePosition> position) {
}


