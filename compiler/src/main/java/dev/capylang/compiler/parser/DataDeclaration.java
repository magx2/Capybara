package dev.capylang.compiler.parser;

import dev.capylang.compiler.Visibility;

import java.util.List;
import java.util.Optional;

public record DataDeclaration(String name, List<DataField> fields,
                              List<String> extendsTypes,
                              List<String> typeParameters,
                              List<String> comments,
                              Visibility visibility,
                              Optional<SourcePosition> position) implements Definition {
    public DataDeclaration(String name, List<DataField> fields,
                           List<String> extendsTypes,
                           List<String> typeParameters,
                           List<String> comments,
                           Optional<SourcePosition> position) {
        this(name, fields, extendsTypes, typeParameters, comments, null, position);
    }

    public record DataField(String name, Type type) {
    }
}
