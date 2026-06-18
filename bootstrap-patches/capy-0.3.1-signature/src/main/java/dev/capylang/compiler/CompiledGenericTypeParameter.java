package dev.capylang.compiler;

public record CompiledGenericTypeParameter(String name) implements CompiledType {
    @Override
    public String toString() {
        return "CompiledGenericTypeParameter[name=" + name + "]";
    }
}
