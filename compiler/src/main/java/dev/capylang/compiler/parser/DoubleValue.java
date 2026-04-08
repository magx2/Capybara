package dev.capylang.compiler.parser;

import java.util.Optional;

public record DoubleValue(String doubleValue, Optional<SourcePosition> position) implements Expression {
}

