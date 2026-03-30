package dev.capylang.generator.java;

import java.util.List;
import java.util.Set;

public record JavaRecord(JavaType name,
                         boolean isPrivate,
                         List<String> comments,
                         Set<JavaType> implementInterfaces,
                         List<JavaRecordField> fields,
                         List<String> typeParameters,
                         Set<JavaMethod> staticMethods,
                         Set<JavaMethod> methods)implements Comparable<JavaRecord> {
    @Override
    public final boolean equals(Object o) {
        if (!(o instanceof JavaRecord that)) return false;

        return name.equals(that.name);
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }

    @Override
    public int compareTo(JavaRecord o) {
        return name.compareTo(o.name);
    }

    public record JavaRecordField(String name, JavaType type) {
    }
}
