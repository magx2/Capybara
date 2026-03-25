package dev.capylang.compiler.parser;

import dev.capylang.compiler.Visibility;
import dev.capylang.compiler.parser.DataDeclaration.DataField;

import java.util.List;
import java.util.Optional;

public record TypeDeclaration(String name, List<String> subTypes, List<DataField> fields,
                              List<String> typeParameters,
                              Visibility visibility,
                              Optional<SourcePosition> position) implements Definition {
    public TypeDeclaration(String name, List<String> subTypes, List<DataField> fields,
                           List<String> typeParameters,
                           Optional<SourcePosition> position) {
        this(name, subTypes, fields, typeParameters, null, position);
    }
}
