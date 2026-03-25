package dev.capylang.generator.java;

import java.util.List;

public record JavaNormalInterface(JavaType name, List<JavaInterfaceMethod> methods, List<JavaMethod> defaultMethods) implements JavaInterface, Comparable<JavaNormalInterface> {
    @Override
    public final boolean equals(Object o) {
        if (!(o instanceof JavaNormalInterface that)) return false;

        return name.equals(that.name);
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }

    @Override
    public int compareTo(JavaNormalInterface o) {
        return name.compareTo(o.name);
    }
}
