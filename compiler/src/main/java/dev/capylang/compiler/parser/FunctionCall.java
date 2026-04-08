package dev.capylang.compiler.parser;

import java.util.List;
import java.util.Optional;

public record FunctionCall(Optional<String> moduleName, String name, List<Expression> arguments,
                           Optional<SourcePosition> position) implements Expression {
}

