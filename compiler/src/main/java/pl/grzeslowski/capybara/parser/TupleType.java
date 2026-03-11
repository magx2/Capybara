package pl.grzeslowski.capybara.parser;

import java.util.List;

public record TupleType(List<Type> elementTypes) implements Type {
    @Override
    public String name() {
        return "tuple";
    }
}


