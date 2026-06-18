package dev.capylang.compiler;

public record CompiledFunctionType(CompiledType argumentType, CompiledType returnType) implements CompiledType {
    @Override
    public String name() {
        return argumentType.name() + "=>" + returnType.name();
    }

    @Override
    public String toString() {
        return "CompiledFunctionType[argumentType=" + TypeStrings.describe(argumentType)
                + ", returnType=" + TypeStrings.describe(returnType)
                + "]";
    }
}
