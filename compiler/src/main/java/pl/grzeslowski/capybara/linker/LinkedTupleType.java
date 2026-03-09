package pl.grzeslowski.capybara.linker;

import java.util.List;

public record LinkedTupleType(List<LinkedType> elementTypes) implements LinkedType {
    @Override
    public String name() {
        return "Tuple";
    }
}

