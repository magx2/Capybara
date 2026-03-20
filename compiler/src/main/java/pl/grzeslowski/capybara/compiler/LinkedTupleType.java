package pl.grzeslowski.capybara.compiler;

import java.util.List;

public record LinkedTupleType(List<LinkedType> elementTypes) implements LinkedType {
    @Override
    public String name() {
        return "tuple";
    }
}


