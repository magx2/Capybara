package dev.capylang.compiler.parser;

import java.util.List;

public record ObjectOriented(List<TypeDeclaration> definitions) {
    public sealed interface TypeDeclaration permits ClassDeclaration, TraitDeclaration, InterfaceDeclaration {
        String name();

        List<TypeReference> parents();

        List<MemberDeclaration> members();
    }

    public sealed interface MemberDeclaration permits FieldDeclaration, MethodDeclaration, InitBlock {
    }

    public record ClassDeclaration(
            String name,
            List<Parameter> constructorParameters,
            List<TypeReference> parents,
            List<MemberDeclaration> members,
            List<String> modifiers
    ) implements TypeDeclaration {
    }

    public record TraitDeclaration(
            String name,
            List<TypeReference> parents,
            List<MemberDeclaration> members
    ) implements TypeDeclaration {
    }

    public record InterfaceDeclaration(
            String name,
            List<TypeReference> parents,
            List<MemberDeclaration> members
    ) implements TypeDeclaration {
    }

    public record FieldDeclaration(
            String name,
            String type,
            String visibility,
            boolean hasInitializer
    ) implements MemberDeclaration {
    }

    public record MethodDeclaration(
            String name,
            List<Parameter> parameters,
            String returnType,
            String visibility,
            List<String> modifiers,
            boolean hasBody
    ) implements MemberDeclaration {
    }

    public record InitBlock() implements MemberDeclaration {
    }

    public record Parameter(String name, String type) {
    }

    public record TypeReference(String name) {
    }
}
