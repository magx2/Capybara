package dev.capylang.compiler;

public record CompiledFunctionType(CompiledType argumentType, CompiledType returnType) implements CompiledType {
    @Override
    public String name() {
        return argumentType.name() + "=>" + returnType.name();
    }
}
