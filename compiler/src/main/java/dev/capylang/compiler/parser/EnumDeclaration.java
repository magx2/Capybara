package dev.capylang.compiler.parser;

import java.util.List;
import java.util.Optional;

public record EnumDeclaration(String name, List<String> values, List<String> comments, Optional<SourcePosition> position) implements Definition {
}
