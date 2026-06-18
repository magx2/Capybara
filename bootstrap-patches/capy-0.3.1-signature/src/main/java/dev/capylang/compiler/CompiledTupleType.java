package dev.capylang.compiler;

import java.util.List;

public record CompiledTupleType(List<CompiledType> elementTypes) implements CompiledType {
    @Override
    public String name() {
        return "Tuple";
    }

    @Override
    public String toString() {
        return "CompiledTupleType[elementTypes=" + TypeStrings.describeAll(elementTypes) + "]";
    }
}
