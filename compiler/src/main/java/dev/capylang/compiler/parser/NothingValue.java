package dev.capylang.compiler.parser;

import java.util.Optional;

public record NothingValue(Optional<SourcePosition> position) implements Expression {
}

