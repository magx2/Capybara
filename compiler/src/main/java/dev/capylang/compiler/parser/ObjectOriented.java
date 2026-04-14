package dev.capylang.compiler.parser;

import java.util.List;
import java.util.Optional;

public record ObjectOriented(List<TypeDeclaration> definitions) {
    public sealed interface TypeDeclaration permits ClassDeclaration, TraitDeclaration, InterfaceDeclaration {
        String name();

        List<TypeReference> parents();

        List<MemberDeclaration> members();
    }

    public sealed interface MemberDeclaration permits FieldDeclaration, MethodDeclaration, InitBlock {
    }

    public sealed interface MethodBody permits ExpressionBody, StatementBlock {
    }

    public sealed interface Statement permits LetStatement, ReturnStatement, IfStatement, WhileStatement, DoWhileStatement, ForEachStatement, StatementBlock {
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
            Optional<String> initializer
    ) implements MemberDeclaration {
    }

    public record MethodDeclaration(
            String name,
            List<Parameter> parameters,
            String returnType,
            String visibility,
            List<String> modifiers,
            Optional<MethodBody> body
    ) implements MemberDeclaration {
    }

    public record InitBlock(StatementBlock body) implements MemberDeclaration {
    }

    public record ExpressionBody(String expression) implements MethodBody {
    }

    public record StatementBlock(List<Statement> statements) implements MethodBody, Statement {
    }

    public record LetStatement(String name, Optional<String> type, String expression) implements Statement {
    }

    public record ReturnStatement(String expression) implements Statement {
    }

    public record IfStatement(String condition, StatementBlock thenBranch, Optional<Statement> elseBranch) implements Statement {
    }

    public record WhileStatement(String condition, StatementBlock body) implements Statement {
    }

    public record DoWhileStatement(StatementBlock body, String condition) implements Statement {
    }

    public record ForEachStatement(String name, Optional<String> type, String iterable, StatementBlock body) implements Statement {
    }

    public record Parameter(String name, String type) {
    }

    public record TypeReference(String name) {
    }
}
