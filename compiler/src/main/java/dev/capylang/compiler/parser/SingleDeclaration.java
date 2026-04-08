package dev.capylang.compiler.parser;

import java.util.Optional;

public record SingleDeclaration(String name, Optional<SourcePosition> position) implements Definition {
}
