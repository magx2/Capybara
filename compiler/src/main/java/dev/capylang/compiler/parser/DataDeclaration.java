package dev.capylang.compiler.parser;

import dev.capylang.compiler.Visibility;

import java.util.List;
import java.util.Optional;

public record DataDeclaration(String name, List<DataField> fields,
                              List<String> extendsTypes,
                              List<String> typeParameters,
                              Visibility visibility,
                              Optional<SourcePosition> position) implements Definition {
    public DataDeclaration(String name, List<DataField> fields,
                           List<String> extendsTypes,
                           List<String> typeParameters,
                           Optional<SourcePosition> position) {
        this(name, fields, extendsTypes, typeParameters, null, position);
    }

    public record DataField(String name, Type type) {
    }
}
