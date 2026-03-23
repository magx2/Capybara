package pl.grzeslowski.capybara.compiler.parser;

import java.util.Optional;

public record Parameter(Type type, String name, Optional<SourcePosition> position) {
}


