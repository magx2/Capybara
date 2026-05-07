package dev.capylang.generator.java;

import dev.capylang.compiler.CompiledType;

import java.util.List;

public record JavaDataValueInfo(
        String name,
        String packageName,
        String packagePath,
        List<Field> fields
) {
    public JavaDataValueInfo {
        fields = List.copyOf(fields);
    }

    public record Field(String name, CompiledType type, String valueExpression) {
    }
}
