package pl.grzeslowski.capybara.compiler;

import java.util.List;

public record CompiledTupleType(List<CompiledType> elementTypes) implements CompiledType {
    @Override
    public String name() {
        return "tuple";
    }
}


