package pl.grzeslowski.capybara.compiler.parser;

import java.util.List;
import java.util.Optional;

public record EnumDeclaration(String name, List<String> values, Optional<SourcePosition> position) implements Definition {
}
