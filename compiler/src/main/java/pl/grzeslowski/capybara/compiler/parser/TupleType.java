package pl.grzeslowski.capybara.compiler.parser;

import java.util.List;

public record TupleType(List<Type> elementTypes) implements Type {
    @Override
    public String name() {
        return "tuple";
    }
}


