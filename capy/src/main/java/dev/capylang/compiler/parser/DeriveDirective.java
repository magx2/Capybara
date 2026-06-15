package dev.capylang.compiler.parser;

import java.util.Optional;

public record DeriveDirective(String name, Optional<SourcePosition> position) {
}
