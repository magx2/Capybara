package dev.capylang.generator.java;

import java.util.Set;

public record JavaEnum(
        JavaType name,
        Set<JavaType> implementInterfaces,
        java.util.List<String> values,
        java.util.List<JavaDataValueInfo> dataValueInfos
) implements Comparable<JavaEnum> {
    @Override
    public int compareTo(JavaEnum o) {
        return name.compareTo(o.name);
    }
}
