package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledType;

import java.util.List;

public record CompiledNewData(CompiledType type, List<FieldAssignment> assignments) implements CompiledExpression {
    public record FieldAssignment(String name, CompiledExpression value) {
    }
}
