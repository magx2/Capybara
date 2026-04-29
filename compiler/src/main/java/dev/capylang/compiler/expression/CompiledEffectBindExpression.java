package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledDataParentType;
import dev.capylang.compiler.CompiledType;

import java.util.Optional;

public record CompiledEffectBindExpression(String name,
                                           CompiledExpression source,
                                           CompiledType payloadType,
                                           CompiledType letType,
                                           Optional<CompiledType> declaredType,
                                           CompiledExpression rest,
                                           CompiledDataParentType effectType) implements CompiledExpression {
    @Override
    public CompiledType type() {
        return effectType;
    }
}
