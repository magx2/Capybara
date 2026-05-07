package dev.capylang.compiler.parser;

import dev.capylang.compiler.Visibility;
import dev.capylang.compiler.parser.DataDeclaration.DataField;

import java.util.List;
import java.util.Optional;

public record TypeDeclaration(String name, List<String> subTypes, List<DataField> fields,
                              List<String> typeParameters,
                              Optional<Expression> constructor,
                              List<DeriveDirective> derives,
                              List<String> comments,
                              Visibility visibility,
                              Optional<SourcePosition> position) implements Definition {
    public TypeDeclaration(String name, List<String> subTypes, List<DataField> fields,
                           List<String> typeParameters,
                           Optional<Expression> constructor,
                           List<String> comments,
                           Visibility visibility,
                           Optional<SourcePosition> position) {
        this(name, subTypes, fields, typeParameters, constructor, List.of(), comments, visibility, position);
    }

    public TypeDeclaration(String name, List<String> subTypes, List<DataField> fields,
                           List<String> typeParameters,
                           Optional<Expression> constructor,
                           List<String> comments,
                           Optional<SourcePosition> position) {
        this(name, subTypes, fields, typeParameters, constructor, comments, null, position);
    }
}
