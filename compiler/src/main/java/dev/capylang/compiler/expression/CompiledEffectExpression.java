package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledDataParentType;
import dev.capylang.compiler.CompiledType;

public record CompiledEffectExpression(CompiledExpression body,
                                       CompiledDataParentType effectType) implements CompiledExpression {
    @Override
    public CompiledType type() {
        return effectType;
    }
}
