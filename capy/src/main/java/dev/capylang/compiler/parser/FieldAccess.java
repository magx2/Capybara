package dev.capylang.compiler.parser;

import java.util.Optional;

public record FieldAccess(Expression source, String field, Optional<SourcePosition> position) implements Expression {
}
