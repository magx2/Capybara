package pl.grzeslowski.capybara.linker;

import pl.grzeslowski.capybara.linker.LinkedDataType.LinkedField;

import java.util.List;

public record LinkedDataParentType(String name, List<LinkedField> fields,
                                   List<LinkedDataType> subTypes,
                                   List<String> typeParameters,
                                   boolean enumType) implements GenericDataType {

    public LinkedDataParentType(String name, List<LinkedField> fields, List<LinkedDataType> subTypes, List<String> typeParameters) {
        this(name, fields, subTypes, typeParameters, false);
    }

    @Override
    public final boolean equals(Object o) {
        if (!(o instanceof LinkedDataParentType that)) return false;

        return name.equals(that.name);
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }
}
