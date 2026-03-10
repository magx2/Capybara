package pl.grzeslowski.capybara.generator.java;

import java.util.List;
import java.util.Set;

public record JavaRecord(JavaType name,
                         boolean isPrivate,
                         Set<JavaType> implementInterfaces,
                         List<JavaRecordField> fields,
                         List<String> typeParameters,
                         Set<JavaMethod> staticMethods,
                         Set<JavaMethod> methods) {
    @Override
    public final boolean equals(Object o) {
        if (!(o instanceof JavaRecord that)) return false;

        return name.equals(that.name);
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }

    public record JavaRecordField(String name, JavaType type) {
    }
}
