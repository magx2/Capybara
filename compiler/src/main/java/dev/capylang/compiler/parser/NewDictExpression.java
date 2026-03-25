package dev.capylang.compiler.parser;

import java.util.List;
import java.util.Optional;

public record NewDictExpression(List<Entry> entries, Optional<SourcePosition> position) implements Expression {
    public record Entry(Expression key, Expression value) {
    }
}
