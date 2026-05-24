package dev.capylang.compiler.parser;

import dev.capylang.compiler.Visibility;

import java.util.List;
import java.util.Optional;

public record DataDeclaration(String name, List<DataField> fields,
                              List<String> extendsTypes,
                              List<String> typeParameters,
                              Optional<Expression> constructor,
                              List<DeriveDirective> derives,
                              List<String> comments,
                              Visibility visibility,
                              boolean nativeType,
                              Optional<SourcePosition> position,
                              List<AnnotationUsage> annotations) implements Definition {
    public DataDeclaration(String name, List<DataField> fields,
                           List<String> extendsTypes,
                           List<String> typeParameters,
                           Optional<Expression> constructor,
                           List<DeriveDirective> derives,
                           List<String> comments,
                           Visibility visibility,
                           boolean nativeType,
                           Optional<SourcePosition> position) {
        this(name, fields, extendsTypes, typeParameters, constructor, derives, comments, visibility, nativeType, position, List.of());
    }

    public DataDeclaration(String name, List<DataField> fields,
                           List<String> extendsTypes,
                           List<String> typeParameters,
                           Optional<Expression> constructor,
                           List<String> comments,
                           Visibility visibility,
                           Optional<SourcePosition> position) {
        this(name, fields, extendsTypes, typeParameters, constructor, List.of(), comments, visibility, false, position);
    }

    public DataDeclaration(String name, List<DataField> fields,
                           List<String> extendsTypes,
                           List<String> typeParameters,
                           Optional<Expression> constructor,
                           List<String> comments,
                           Optional<SourcePosition> position) {
        this(name, fields, extendsTypes, typeParameters, constructor, comments, null, position);
    }

    public DataDeclaration(String name, List<DataField> fields,
                           List<String> extendsTypes,
                           List<String> typeParameters,
                           List<String> comments,
                           Optional<SourcePosition> position) {
        this(name, fields, extendsTypes, typeParameters, Optional.empty(), comments, null, position);
    }

    public record DataField(String name, Type type, List<AnnotationUsage> annotations) {
        public DataField {
            annotations = annotations == null ? List.of() : List.copyOf(annotations);
        }

        public DataField(String name, Type type) {
            this(name, type, List.of());
        }
    }
}
