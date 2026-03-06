package pl.grzeslowski.capybara.linker;

import java.util.List;

public record LinkedDataType(String name, List<LinkedField> fields, List<String> typeParameters) implements GenericDataType {

    @Override
    public final boolean equals(Object o) {
        if (!(o instanceof LinkedDataType that)) return false;

        return name.equals(that.name);
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }

    public record LinkedField(String name, LinkedType type) {
    }
}
