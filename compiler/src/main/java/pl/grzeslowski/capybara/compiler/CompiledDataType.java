package pl.grzeslowski.capybara.compiler;

import java.util.List;

public record CompiledDataType(String name,
                             List<CompiledField> fields,
                             List<String> typeParameters,
                             List<String> extendedTypes,
                             boolean singleton) implements GenericDataType {

    @Override
    public final boolean equals(Object o) {
        if (!(o instanceof CompiledDataType that)) return false;

        return name.equals(that.name);
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }

    public record CompiledField(String name, CompiledType type) {
    }
}
