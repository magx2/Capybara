package dev.capylang.compiler.parser;

import java.util.List;

public record TupleType(List<Type> elementTypes) implements Type {
    @Override
    public String name() {
        return "Tuple";
    }
}


