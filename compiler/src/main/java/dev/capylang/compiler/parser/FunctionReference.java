package dev.capylang.compiler.parser;

import java.util.Optional;

public record FunctionReference(String name, Optional<SourcePosition> position) implements Expression {
}
