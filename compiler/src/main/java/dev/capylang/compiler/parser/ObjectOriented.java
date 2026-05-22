package dev.capylang.compiler.parser;

import dev.capylang.compiler.CompiledAnnotation;

import java.util.List;
import java.util.Optional;

public record ObjectOriented(List<TypeDeclaration> definitions, List<NativeProviderDeclaration> nativeProviders) {
    public ObjectOriented(List<TypeDeclaration> definitions) {
        this(definitions, List.of());
    }

    public sealed interface TypeDeclaration permits ClassDeclaration, TraitDeclaration, InterfaceDeclaration {
        String name();

        List<TypeReference> parents();

        List<MemberDeclaration> members();

        List<CompiledAnnotation> linkedAnnotations();
    }

    public sealed interface MemberDeclaration permits FieldDeclaration, MethodDeclaration, InitBlock {
        List<CompiledAnnotation> linkedAnnotations();
    }

    public sealed interface MethodBody permits ExpressionBody, StatementBlock {
    }

    public sealed interface Statement permits LetStatement, LocalMethodStatement, MutableVariableStatement, AssignmentStatement, ExpressionStatement, ThrowStatement, ReturnStatement, IfStatement, TryCatchStatement, WhileStatement, DoWhileStatement, ForEachStatement, StatementBlock {
    }

    public record ClassDeclaration(
            String name,
            List<Parameter> constructorParameters,
            List<TypeReference> parents,
            List<MemberDeclaration> members,
            List<String> modifiers,
            List<String> comments,
            List<AnnotationUsage> annotations,
            List<CompiledAnnotation> linkedAnnotations
    ) implements TypeDeclaration {
        public ClassDeclaration {
            annotations = annotations == null ? List.of() : List.copyOf(annotations);
            linkedAnnotations = linkedAnnotations == null ? List.of() : List.copyOf(linkedAnnotations);
        }

        public ClassDeclaration(
                String name,
                List<Parameter> constructorParameters,
                List<TypeReference> parents,
                List<MemberDeclaration> members,
                List<String> modifiers,
                List<String> comments,
                List<AnnotationUsage> annotations
        ) {
            this(name, constructorParameters, parents, members, modifiers, comments, annotations, List.of());
        }

        public ClassDeclaration(
                String name,
                List<Parameter> constructorParameters,
                List<TypeReference> parents,
                List<MemberDeclaration> members,
                List<String> modifiers,
                List<String> comments
        ) {
            this(name, constructorParameters, parents, members, modifiers, comments, List.of(), List.of());
        }
    }

    public record TraitDeclaration(
            String name,
            List<TypeReference> parents,
            List<MemberDeclaration> members,
            List<String> comments,
            List<AnnotationUsage> annotations,
            List<CompiledAnnotation> linkedAnnotations
    ) implements TypeDeclaration {
        public TraitDeclaration {
            annotations = annotations == null ? List.of() : List.copyOf(annotations);
            linkedAnnotations = linkedAnnotations == null ? List.of() : List.copyOf(linkedAnnotations);
        }

        public TraitDeclaration(
                String name,
                List<TypeReference> parents,
                List<MemberDeclaration> members,
                List<String> comments,
                List<AnnotationUsage> annotations
        ) {
            this(name, parents, members, comments, annotations, List.of());
        }

        public TraitDeclaration(
                String name,
                List<TypeReference> parents,
                List<MemberDeclaration> members,
                List<String> comments
        ) {
            this(name, parents, members, comments, List.of(), List.of());
        }
    }

    public record InterfaceDeclaration(
            String name,
            List<TypeReference> parents,
            List<MemberDeclaration> members,
            List<String> comments,
            List<AnnotationUsage> annotations,
            List<CompiledAnnotation> linkedAnnotations
    ) implements TypeDeclaration {
        public InterfaceDeclaration {
            annotations = annotations == null ? List.of() : List.copyOf(annotations);
            linkedAnnotations = linkedAnnotations == null ? List.of() : List.copyOf(linkedAnnotations);
        }

        public InterfaceDeclaration(
                String name,
                List<TypeReference> parents,
                List<MemberDeclaration> members,
                List<String> comments,
                List<AnnotationUsage> annotations
        ) {
            this(name, parents, members, comments, annotations, List.of());
        }

        public InterfaceDeclaration(
                String name,
                List<TypeReference> parents,
                List<MemberDeclaration> members,
                List<String> comments
        ) {
            this(name, parents, members, comments, List.of(), List.of());
        }
    }

    public record NativeProviderDeclaration(String name, String targetType, String qualifier, List<String> comments) {
    }

    public record FieldDeclaration(
            String name,
            String type,
            String visibility,
            Optional<String> initializer,
            List<String> comments,
            List<AnnotationUsage> annotations,
            List<CompiledAnnotation> linkedAnnotations
    ) implements MemberDeclaration {
        public FieldDeclaration {
            annotations = annotations == null ? List.of() : List.copyOf(annotations);
            linkedAnnotations = linkedAnnotations == null ? List.of() : List.copyOf(linkedAnnotations);
        }

        public FieldDeclaration(
                String name,
                String type,
                String visibility,
                Optional<String> initializer,
                List<String> comments,
                List<AnnotationUsage> annotations
        ) {
            this(name, type, visibility, initializer, comments, annotations, List.of());
        }

        public FieldDeclaration(
                String name,
                String type,
                String visibility,
                Optional<String> initializer,
                List<String> comments
        ) {
            this(name, type, visibility, initializer, comments, List.of(), List.of());
        }
    }

    public record MethodDeclaration(
            String name,
            List<Parameter> parameters,
            String returnType,
            String visibility,
            List<String> modifiers,
            Optional<MethodBody> body,
            List<String> comments,
            List<AnnotationUsage> annotations,
            List<CompiledAnnotation> linkedAnnotations
    ) implements MemberDeclaration {
        public MethodDeclaration {
            annotations = annotations == null ? List.of() : List.copyOf(annotations);
            linkedAnnotations = linkedAnnotations == null ? List.of() : List.copyOf(linkedAnnotations);
        }

        public MethodDeclaration(
                String name,
                List<Parameter> parameters,
                String returnType,
                String visibility,
                List<String> modifiers,
                Optional<MethodBody> body,
                List<String> comments,
                List<AnnotationUsage> annotations
        ) {
            this(name, parameters, returnType, visibility, modifiers, body, comments, annotations, List.of());
        }

        public MethodDeclaration(
                String name,
                List<Parameter> parameters,
                String returnType,
                String visibility,
                List<String> modifiers,
                Optional<MethodBody> body,
                List<String> comments
        ) {
            this(name, parameters, returnType, visibility, modifiers, body, comments, List.of(), List.of());
        }
    }

    public record InitBlock(
            StatementBlock body,
            List<String> comments,
            List<AnnotationUsage> annotations,
            List<CompiledAnnotation> linkedAnnotations
    ) implements MemberDeclaration {
        public InitBlock {
            annotations = annotations == null ? List.of() : List.copyOf(annotations);
            linkedAnnotations = linkedAnnotations == null ? List.of() : List.copyOf(linkedAnnotations);
        }

        public InitBlock(StatementBlock body, List<String> comments, List<AnnotationUsage> annotations) {
            this(body, comments, annotations, List.of());
        }

        public InitBlock(StatementBlock body, List<String> comments) {
            this(body, comments, List.of(), List.of());
        }
    }

    public record ExpressionBody(String expression) implements MethodBody {
    }

    public record StatementBlock(List<Statement> statements) implements MethodBody, Statement {
    }

    public record LetStatement(String name, Optional<String> type, String expression) implements Statement {
    }

    public record LocalMethodStatement(
            String name,
            List<Parameter> parameters,
            String returnType,
            MethodBody body,
            List<String> comments
    ) implements Statement {
    }

    public record MutableVariableStatement(String name, Optional<String> type, String expression) implements Statement {
    }

    public record AssignmentStatement(String name, String expression) implements Statement {
    }

    public record ExpressionStatement(String expression) implements Statement {
    }

    public record ThrowStatement(String expression) implements Statement {
    }

    public record ReturnStatement(String expression) implements Statement {
    }

    public record IfStatement(String condition, StatementBlock thenBranch, Optional<Statement> elseBranch) implements Statement {
    }

    public record TryCatchStatement(StatementBlock tryBlock, List<CatchClause> catches) implements Statement {
    }

    public record CatchClause(String name, StatementBlock body) {
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
