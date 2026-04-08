package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledType;

import java.util.List;

public record CompiledNewDict(List<Entry> entries, CompiledType type) implements CompiledExpression {
    public record Entry(CompiledExpression key, CompiledExpression value) {
    }
}
