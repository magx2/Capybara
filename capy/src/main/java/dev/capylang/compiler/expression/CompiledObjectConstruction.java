package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledDataParentType;
import dev.capylang.compiler.CompiledObjectType;
import dev.capylang.compiler.CompiledType;

import java.util.List;

public record CompiledObjectConstruction(
        CompiledObjectType objectType,
        List<CompiledExpression> arguments,
        CompiledDataParentType effectType
) implements CompiledExpression {
    public CompiledObjectConstruction {
        arguments = List.copyOf(arguments);
    }

    @Override
    public CompiledType type() {
        return effectType;
    }
}
