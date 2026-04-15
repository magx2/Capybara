package dev.capylang.compiler;

import dev.capylang.compiler.parser.ObjectOriented;
import dev.capylang.compiler.parser.ObjectOrientedModule;

import java.util.HashSet;
import java.util.List;
import java.util.TreeSet;

public final class ObjectOrientedValidator {
    public static final ObjectOrientedValidator INSTANCE = new ObjectOrientedValidator();

    public Result<List<ObjectOrientedModule>> validate(List<ObjectOrientedModule> modules) {
        var errors = new TreeSet<Result.Error.SingleError>();
        modules.forEach(module -> validateModule(module, errors));
        return errors.isEmpty() ? Result.success(modules) : new Result.Error<>(errors);
    }

    private void validateModule(ObjectOrientedModule module, TreeSet<Result.Error.SingleError> errors) {
        module.objectOriented().definitions().forEach(definition -> validateDefinition(module, definition, errors));
    }

    private void validateDefinition(
            ObjectOrientedModule module,
            ObjectOriented.TypeDeclaration definition,
            TreeSet<Result.Error.SingleError> errors
    ) {
        var constructorParameters = definition instanceof ObjectOriented.ClassDeclaration classDeclaration
                ? classDeclaration.constructorParameters().stream().map(ObjectOriented.Parameter::name).toList()
                : List.<String>of();
        definition.members().forEach(member -> validateMember(module, definition.name(), member, constructorParameters, errors));
    }

    private void validateMember(
            ObjectOrientedModule module,
            String typeName,
            ObjectOriented.MemberDeclaration member,
            List<String> constructorParameters,
            TreeSet<Result.Error.SingleError> errors
    ) {
        if (member instanceof ObjectOriented.MethodDeclaration method
            && method.body().orElse(null) instanceof ObjectOriented.StatementBlock body) {
            validateBlock(module, typeName + "." + method.name(), body, Scope.root(method.parameters().stream().map(ObjectOriented.Parameter::name).toList()), errors);
        }
        if (member instanceof ObjectOriented.InitBlock initBlock) {
            validateBlock(module, typeName + ".init", initBlock.body(), Scope.root(constructorParameters), errors);
        }
    }

    private void validateBlock(
            ObjectOrientedModule module,
            String owner,
            ObjectOriented.StatementBlock block,
            Scope parentScope,
            TreeSet<Result.Error.SingleError> errors
    ) {
        var scope = parentScope.child();
        for (var statement : block.statements()) {
            switch (statement) {
                case ObjectOriented.LetStatement letStatement -> scope.declareImmutable(letStatement.name());
                case ObjectOriented.MutableVariableStatement mutableVariableStatement -> scope.declareMutable(mutableVariableStatement.name());
                case ObjectOriented.AssignmentStatement assignmentStatement -> validateAssignment(module, owner, assignmentStatement, scope, errors);
                case ObjectOriented.IfStatement ifStatement -> {
                    validateBlock(module, owner, ifStatement.thenBranch(), scope, errors);
                    ifStatement.elseBranch().ifPresent(elseBranch -> validateNestedStatement(module, owner, elseBranch, scope, errors));
                }
                case ObjectOriented.WhileStatement whileStatement -> validateBlock(module, owner, whileStatement.body(), scope, errors);
                case ObjectOriented.DoWhileStatement doWhileStatement -> validateBlock(module, owner, doWhileStatement.body(), scope, errors);
                case ObjectOriented.ForEachStatement forEachStatement -> {
                    var loopScope = scope.child();
                    loopScope.declareImmutable(forEachStatement.name());
                    validateBlock(module, owner, forEachStatement.body(), loopScope, errors);
                }
                case ObjectOriented.StatementBlock nestedBlock -> validateBlock(module, owner, nestedBlock, scope, errors);
                case ObjectOriented.ReturnStatement ignored -> {
                }
            }
        }
    }

    private void validateNestedStatement(
            ObjectOrientedModule module,
            String owner,
            ObjectOriented.Statement statement,
            Scope scope,
            TreeSet<Result.Error.SingleError> errors
    ) {
        switch (statement) {
            case ObjectOriented.StatementBlock block -> validateBlock(module, owner, block, scope, errors);
            case ObjectOriented.IfStatement ifStatement -> {
                validateBlock(module, owner, ifStatement.thenBranch(), scope, errors);
                ifStatement.elseBranch().ifPresent(elseBranch -> validateNestedStatement(module, owner, elseBranch, scope, errors));
            }
            case ObjectOriented.WhileStatement whileStatement -> validateBlock(module, owner, whileStatement.body(), scope, errors);
            case ObjectOriented.DoWhileStatement doWhileStatement -> validateBlock(module, owner, doWhileStatement.body(), scope, errors);
            case ObjectOriented.ForEachStatement forEachStatement -> {
                var loopScope = scope.child();
                loopScope.declareImmutable(forEachStatement.name());
                validateBlock(module, owner, forEachStatement.body(), loopScope, errors);
            }
            case ObjectOriented.AssignmentStatement assignmentStatement -> validateAssignment(module, owner, assignmentStatement, scope, errors);
            case ObjectOriented.LetStatement letStatement -> scope.declareImmutable(letStatement.name());
            case ObjectOriented.MutableVariableStatement mutableVariableStatement -> scope.declareMutable(mutableVariableStatement.name());
            case ObjectOriented.ReturnStatement ignored -> {
            }
        }
    }

    private void validateAssignment(
            ObjectOrientedModule module,
            String owner,
            ObjectOriented.AssignmentStatement assignmentStatement,
            Scope scope,
            TreeSet<Result.Error.SingleError> errors
    ) {
        var resolution = scope.resolve(assignmentStatement.name());
        if (resolution == Resolution.MUTABLE) {
            return;
        }
        var message = switch (resolution) {
            case IMMUTABLE -> "Cannot assign to immutable local `" + assignmentStatement.name() + "` in `" + owner + "`; use `def` for mutable locals";
            case UNDECLARED -> "Assignment target `" + assignmentStatement.name() + "` in `" + owner + "` is not a mutable local variable";
            case MUTABLE -> throw new IllegalStateException("unreachable");
        };
        errors.add(new Result.Error.SingleError(0, 0, module.moduleFile(), message));
    }

    private enum Resolution {
        MUTABLE,
        IMMUTABLE,
        UNDECLARED
    }

    private static final class Scope {
        private final Scope parent;
        private final HashSet<String> mutable = new HashSet<>();
        private final HashSet<String> immutable = new HashSet<>();

        private Scope(Scope parent) {
            this.parent = parent;
        }

        static Scope root(List<String> immutableNames) {
            var root = new Scope(null);
            immutableNames.forEach(root::declareImmutable);
            return root;
        }

        Scope child() {
            return new Scope(this);
        }

        void declareMutable(String name) {
            mutable.add(name);
            immutable.remove(name);
        }

        void declareImmutable(String name) {
            immutable.add(name);
            mutable.remove(name);
        }

        Resolution resolve(String name) {
            if (mutable.contains(name)) {
                return Resolution.MUTABLE;
            }
            if (immutable.contains(name)) {
                return Resolution.IMMUTABLE;
            }
            return parent == null ? Resolution.UNDECLARED : parent.resolve(name);
        }
    }
}
