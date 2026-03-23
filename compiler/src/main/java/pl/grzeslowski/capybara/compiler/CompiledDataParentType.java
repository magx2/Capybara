package pl.grzeslowski.capybara.compiler;

import pl.grzeslowski.capybara.compiler.CompiledDataType.CompiledField;

import java.util.List;

public record CompiledDataParentType(String name, List<CompiledField> fields,
                                   List<CompiledDataType> subTypes,
                                   List<String> typeParameters,
                                   boolean enumType) implements GenericDataType, Comparable<CompiledDataParentType> {

    public CompiledDataParentType(String name, List<CompiledField> fields, List<CompiledDataType> subTypes, List<String> typeParameters) {
        this(name, fields, subTypes, typeParameters, false);
    }

    @Override
    public final boolean equals(Object o) {
        if (!(o instanceof CompiledDataParentType that)) return false;

        return name.equals(that.name);
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }

    @Override
    public int compareTo(CompiledDataParentType o) {
        return name.compareTo(o.name);
    }
}
