package dev.capylang.compiler.parser;

import java.util.Optional;

public record PlaceholderExpression(Optional<SourcePosition> position) implements Expression {
}
