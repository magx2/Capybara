package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledType;
import dev.capylang.compiler.PrimitiveLinkedType;
import dev.capylang.compiler.parser.SourcePosition;

import java.util.Optional;

public record CompiledNothingValue(Optional<SourcePosition> position, String message) implements CompiledExpression {
    @Override
    public CompiledType type() {
        return PrimitiveLinkedType.NOTHING;
    }
}
