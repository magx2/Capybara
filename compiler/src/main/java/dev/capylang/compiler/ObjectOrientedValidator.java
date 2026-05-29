package dev.capylang.compiler;

import capy.lang.Result;

import dev.capylang.compiler.parser.ObjectOriented;
import dev.capylang.compiler.parser.ObjectOrientedModule;

import java.util.HashSet;
import java.util.List;
import java.util.TreeSet;
import java.util.regex.Pattern;

public final class ObjectOrientedValidator {
    public static final ObjectOrientedValidator INSTANCE = new ObjectOrientedValidator();
    private static final Pattern IDENTIFIER_REFERENCE = Pattern.compile("\\b[_a-z][_A-Za-z0-9]*\\b");
    private static final Pattern STRING_LITERAL = Pattern.compile("\"(?:[^\"\\\\]|\\\\.)*\"|'(?:[^'\\\\]|\\\\.)*'");
    // OO expressions are raw strings until backend generation, so reject the unsafe escape hatch by call name.
    private static final Pattern UNSAFE_RUN_CALL = Pattern.compile(
            "(?s)(?:\\.\\s*|(?:^|[^A-Za-z0-9_\\.]))(?:unsafe_run|unsafeRun)\\s*\\("
    );

    public Result<List<ObjectOrientedModule>> validate(List<ObjectOrientedModule> modules) {
        var errors = new TreeSet<CompilerError>();
        modules.forEach(module -> validateModule(module, errors));
        return errors.isEmpty() ? Results.success(modules) : CompilerErrors.result(errors);
    }

    private void validateModule(ObjectOrientedModule module, TreeSet<CompilerError> errors) {
        module.objectOriented().definitions().forEach(definition -> validateDefinition(module, definition, errors));
    }

    private void validateDefinition(
            ObjectOrientedModule module,
            ObjectOriented.TypeDeclaration definition,
            TreeSet<CompilerError> errors
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
            TreeSet<CompilerError> errors
    ) {
        if (member instanceof ObjectOriented.MethodDeclaration method
            && method.body().isPresent()) {
            var owner = typeName + "." + method.name();
            var body = method.body().orElseThrow();
            validateUnsafeEffectRunUsage(module, owner, body, errors);
            if (body instanceof ObjectOriented.StatementBlock block) {
                validateBlock(module, owner, block, Scope.root(method.parameters().stream().map(ObjectOriented.Parameter::name).toList()), errors);
            }
        }
        if (member instanceof ObjectOriented.InitBlock initBlock) {
            validateUnsafeEffectRunUsage(module, typeName + ".init", initBlock.body(), errors);
            validateBlock(module, typeName + ".init", initBlock.body(), Scope.root(constructorParameters), errors);
        }
    }

    private void validateBlock(
            ObjectOrientedModule module,
            String owner,
            ObjectOriented.StatementBlock block,
            Scope parentScope,
            TreeSet<CompilerError> errors
    ) {
        var scope = parentScope.child();
        for (var statement : block.statements()) {
            switch (statement) {
                case ObjectOriented.LetStatement letStatement -> scope.declareImmutable(letStatement.name());
                case ObjectOriented.LocalMethodStatement localMethodStatement -> {
                    validateLocalMethod(module, owner, localMethodStatement, scope, errors);
                    scope.declareImmutable(localMethodStatement.name());
                }
                case ObjectOriented.MutableVariableStatement mutableVariableStatement -> scope.declareMutable(mutableVariableStatement.name());
                case ObjectOriented.AssignmentStatement assignmentStatement -> validateAssignment(module, owner, assignmentStatement, scope, errors);
                case ObjectOriented.ExpressionStatement expressionStatement -> validateExpressionStatement(module, owner, expressionStatement, errors);
                case ObjectOriented.ThrowStatement ignored -> {
                }
                case ObjectOriented.IfStatement ifStatement -> {
                    validateBlock(module, owner, ifStatement.thenBranch(), scope, errors);
                    ifStatement.elseBranch().ifPresent(elseBranch -> validateNestedStatement(module, owner, elseBranch, scope, errors));
                }
                case ObjectOriented.TryCatchStatement tryCatchStatement -> {
                    validateBlock(module, owner, tryCatchStatement.tryBlock(), scope, errors);
                    for (var catchClause : tryCatchStatement.catches()) {
                        var catchScope = scope.child();
                        catchScope.declareImmutable(catchClause.name());
                        validateBlock(module, owner, catchClause.body(), catchScope, errors);
                    }
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
            TreeSet<CompilerError> errors
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
            case ObjectOriented.TryCatchStatement tryCatchStatement -> {
                validateBlock(module, owner, tryCatchStatement.tryBlock(), scope, errors);
                for (var catchClause : tryCatchStatement.catches()) {
                    var catchScope = scope.child();
                    catchScope.declareImmutable(catchClause.name());
                    validateBlock(module, owner, catchClause.body(), catchScope, errors);
                }
            }
            case ObjectOriented.AssignmentStatement assignmentStatement -> validateAssignment(module, owner, assignmentStatement, scope, errors);
            case ObjectOriented.ExpressionStatement expressionStatement -> validateExpressionStatement(module, owner, expressionStatement, errors);
            case ObjectOriented.LetStatement letStatement -> scope.declareImmutable(letStatement.name());
            case ObjectOriented.LocalMethodStatement localMethodStatement -> {
                validateLocalMethod(module, owner, localMethodStatement, scope, errors);
                scope.declareImmutable(localMethodStatement.name());
            }
            case ObjectOriented.MutableVariableStatement mutableVariableStatement -> scope.declareMutable(mutableVariableStatement.name());
            case ObjectOriented.ThrowStatement ignored -> {
            }
            case ObjectOriented.ReturnStatement ignored -> {
            }
        }
    }

    private void validateAssignment(
            ObjectOrientedModule module,
            String owner,
            ObjectOriented.AssignmentStatement assignmentStatement,
            Scope scope,
            TreeSet<CompilerError> errors
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
        errors.add(new CompilerError(0, 0, module.moduleFile(), message));
    }

    private void validateLocalMethod(
            ObjectOrientedModule module,
            String owner,
            ObjectOriented.LocalMethodStatement localMethodStatement,
            Scope scope,
            TreeSet<CompilerError> errors
    ) {
        var mutableCaptures = new TreeSet<String>();
        var localScope = scope.child();
        localMethodStatement.parameters().forEach(parameter -> localScope.declareImmutable(parameter.name()));
        localScope.declareImmutable(localMethodStatement.name());
        collectMutableCaptures(localMethodStatement.body(), localScope, mutableCaptures);
        if (!mutableCaptures.isEmpty()) {
            errors.add(new CompilerError(
                    0,
                    0,
                    module.moduleFile(),
                    "Local method `" + owner + "." + localMethodStatement.name() + "` cannot capture mutable locals: " + String.join(", ", mutableCaptures)
            ));
        }
        if (localMethodStatement.body() instanceof ObjectOriented.StatementBlock block) {
            validateBlock(module, owner + "." + localMethodStatement.name(), block, localScope, errors);
        }
    }

    private void validateExpressionStatement(
            ObjectOrientedModule module,
            String owner,
            ObjectOriented.ExpressionStatement expressionStatement,
            TreeSet<CompilerError> errors
    ) {
        if (isCallExpression(expressionStatement.expression())) {
            return;
        }
        errors.add(new CompilerError(
                0,
                0,
                module.moduleFile(),
                "Only call expressions can be used as stand-alone statements in `" + owner + "`"
        ));
    }

    private boolean isCallExpression(String expression) {
        var trimmed = expression.trim();
        if (trimmed.isBlank()) {
            return false;
        }
        if (!trimmed.endsWith(")")) {
            return false;
        }
        var depth = 0;
        char stringDelimiter = 0;
        var outerCallOpenParen = -1;
        for (int i = 0; i < trimmed.length(); i++) {
            var current = trimmed.charAt(i);
            if (stringDelimiter != 0) {
                if (current == '\\') {
                    i++;
                    continue;
                }
                if (current == stringDelimiter) {
                    stringDelimiter = 0;
                }
                continue;
            }
            if (current == '"' || current == '\'') {
                stringDelimiter = current;
                continue;
            }
            switch (current) {
                case '(' -> {
                    if (depth == 0) {
                        outerCallOpenParen = i;
                    }
                    depth++;
                }
                case ')' -> {
                    depth--;
                    if (depth < 0) {
                        return false;
                    }
                }
                default -> {
                }
            }
        }
        if (depth != 0 || outerCallOpenParen <= 0) {
            return false;
        }
        var callee = trimmed.substring(0, outerCallOpenParen).trim();
        return callee.endsWith(")")
               || callee.endsWith("]")
               || IDENTIFIER_REFERENCE.matcher(callee).matches();
    }

    private void validateUnsafeEffectRunUsage(
            ObjectOrientedModule module,
            String owner,
            ObjectOriented.MethodBody body,
            TreeSet<CompilerError> errors
    ) {
        switch (body) {
            case ObjectOriented.ExpressionBody expressionBody -> validateUnsafeEffectRunUsage(module, owner, expressionBody.expression(), errors);
            case ObjectOriented.StatementBlock statementBlock -> validateUnsafeEffectRunUsage(module, owner, statementBlock, errors);
        }
    }

    private void validateUnsafeEffectRunUsage(
            ObjectOrientedModule module,
            String owner,
            ObjectOriented.StatementBlock block,
            TreeSet<CompilerError> errors
    ) {
        for (var statement : block.statements()) {
            validateUnsafeEffectRunUsage(module, owner, statement, errors);
        }
    }

    private void validateUnsafeEffectRunUsage(
            ObjectOrientedModule module,
            String owner,
            ObjectOriented.Statement statement,
            TreeSet<CompilerError> errors
    ) {
        switch (statement) {
            case ObjectOriented.LetStatement letStatement -> validateUnsafeEffectRunUsage(module, owner, letStatement.expression(), errors);
            case ObjectOriented.LocalMethodStatement localMethodStatement ->
                    validateUnsafeEffectRunUsage(module, owner + "." + localMethodStatement.name(), localMethodStatement.body(), errors);
            case ObjectOriented.MutableVariableStatement mutableVariableStatement ->
                    validateUnsafeEffectRunUsage(module, owner, mutableVariableStatement.expression(), errors);
            case ObjectOriented.AssignmentStatement assignmentStatement -> validateUnsafeEffectRunUsage(module, owner, assignmentStatement.expression(), errors);
            case ObjectOriented.ExpressionStatement expressionStatement -> validateUnsafeEffectRunUsage(module, owner, expressionStatement.expression(), errors);
            case ObjectOriented.ThrowStatement throwStatement -> validateUnsafeEffectRunUsage(module, owner, throwStatement.expression(), errors);
            case ObjectOriented.ReturnStatement returnStatement -> validateUnsafeEffectRunUsage(module, owner, returnStatement.expression(), errors);
            case ObjectOriented.IfStatement ifStatement -> {
                validateUnsafeEffectRunUsage(module, owner, ifStatement.condition(), errors);
                validateUnsafeEffectRunUsage(module, owner, ifStatement.thenBranch(), errors);
                ifStatement.elseBranch().ifPresent(elseBranch -> validateUnsafeEffectRunUsage(module, owner, elseBranch, errors));
            }
            case ObjectOriented.TryCatchStatement tryCatchStatement -> {
                validateUnsafeEffectRunUsage(module, owner, tryCatchStatement.tryBlock(), errors);
                tryCatchStatement.catches().forEach(catchClause -> validateUnsafeEffectRunUsage(module, owner, catchClause.body(), errors));
            }
            case ObjectOriented.WhileStatement whileStatement -> {
                validateUnsafeEffectRunUsage(module, owner, whileStatement.condition(), errors);
                validateUnsafeEffectRunUsage(module, owner, whileStatement.body(), errors);
            }
            case ObjectOriented.DoWhileStatement doWhileStatement -> {
                validateUnsafeEffectRunUsage(module, owner, doWhileStatement.body(), errors);
                validateUnsafeEffectRunUsage(module, owner, doWhileStatement.condition(), errors);
            }
            case ObjectOriented.ForEachStatement forEachStatement -> {
                validateUnsafeEffectRunUsage(module, owner, forEachStatement.iterable(), errors);
                validateUnsafeEffectRunUsage(module, owner, forEachStatement.body(), errors);
            }
            case ObjectOriented.StatementBlock nestedBlock -> validateUnsafeEffectRunUsage(module, owner, nestedBlock, errors);
        }
    }

    private void validateUnsafeEffectRunUsage(
            ObjectOrientedModule module,
            String owner,
            String expression,
            TreeSet<CompilerError> errors
    ) {
        var withoutStrings = STRING_LITERAL.matcher(expression).replaceAll(" ");
        if (!UNSAFE_RUN_CALL.matcher(withoutStrings).find()) {
            return;
        }
        errors.add(new CompilerError(
                0,
                0,
                module.moduleFile(),
                "`unsafe_run` cannot be called from Capybara OO source in `" + owner + "`; use effect binding or return `Effect[...]` to the runtime/test runner."
        ));
    }

    private void collectMutableCaptures(ObjectOriented.MethodBody body, Scope scope, TreeSet<String> mutableCaptures) {
        switch (body) {
            case ObjectOriented.ExpressionBody expressionBody -> collectMutableCaptures(expressionBody.expression(), scope, mutableCaptures);
            case ObjectOriented.StatementBlock statementBlock -> collectMutableCaptures(statementBlock, scope, mutableCaptures);
        }
    }

    private void collectMutableCaptures(ObjectOriented.StatementBlock block, Scope parentScope, TreeSet<String> mutableCaptures) {
        var scope = parentScope.child();
        for (var statement : block.statements()) {
            switch (statement) {
                case ObjectOriented.LetStatement letStatement -> {
                    collectMutableCaptures(letStatement.expression(), scope, mutableCaptures);
                    scope.declareImmutable(letStatement.name());
                }
                case ObjectOriented.LocalMethodStatement localMethodStatement -> {
                    var localMethodScope = scope.child();
                    localMethodStatement.parameters().forEach(parameter -> localMethodScope.declareImmutable(parameter.name()));
                    localMethodScope.declareImmutable(localMethodStatement.name());
                    collectMutableCaptures(localMethodStatement.body(), localMethodScope, mutableCaptures);
                    scope.declareImmutable(localMethodStatement.name());
                }
                case ObjectOriented.MutableVariableStatement mutableVariableStatement -> {
                    collectMutableCaptures(mutableVariableStatement.expression(), scope, mutableCaptures);
                    scope.declareMutable(mutableVariableStatement.name());
                }
                case ObjectOriented.AssignmentStatement assignmentStatement -> collectMutableCaptures(assignmentStatement.expression(), scope, mutableCaptures);
                case ObjectOriented.ExpressionStatement expressionStatement -> collectMutableCaptures(expressionStatement.expression(), scope, mutableCaptures);
                case ObjectOriented.ThrowStatement throwStatement -> collectMutableCaptures(throwStatement.expression(), scope, mutableCaptures);
                case ObjectOriented.ReturnStatement returnStatement -> collectMutableCaptures(returnStatement.expression(), scope, mutableCaptures);
                case ObjectOriented.IfStatement ifStatement -> {
                    collectMutableCaptures(ifStatement.condition(), scope, mutableCaptures);
                    collectMutableCaptures(ifStatement.thenBranch(), scope, mutableCaptures);
                    ifStatement.elseBranch().ifPresent(elseBranch -> collectMutableCaptures(elseBranch, scope, mutableCaptures));
                }
                case ObjectOriented.TryCatchStatement tryCatchStatement -> {
                    collectMutableCaptures(tryCatchStatement.tryBlock(), scope, mutableCaptures);
                    for (var catchClause : tryCatchStatement.catches()) {
                        var catchScope = scope.child();
                        catchScope.declareImmutable(catchClause.name());
                        collectMutableCaptures(catchClause.body(), catchScope, mutableCaptures);
                    }
                }
                case ObjectOriented.WhileStatement whileStatement -> {
                    collectMutableCaptures(whileStatement.condition(), scope, mutableCaptures);
                    collectMutableCaptures(whileStatement.body(), scope, mutableCaptures);
                }
                case ObjectOriented.DoWhileStatement doWhileStatement -> {
                    collectMutableCaptures(doWhileStatement.body(), scope, mutableCaptures);
                    collectMutableCaptures(doWhileStatement.condition(), scope, mutableCaptures);
                }
                case ObjectOriented.ForEachStatement forEachStatement -> {
                    collectMutableCaptures(forEachStatement.iterable(), scope, mutableCaptures);
                    var loopScope = scope.child();
                    loopScope.declareImmutable(forEachStatement.name());
                    collectMutableCaptures(forEachStatement.body(), loopScope, mutableCaptures);
                }
                case ObjectOriented.StatementBlock nestedBlock -> collectMutableCaptures(nestedBlock, scope, mutableCaptures);
            }
        }
    }

    private void collectMutableCaptures(ObjectOriented.Statement statement, Scope scope, TreeSet<String> mutableCaptures) {
        switch (statement) {
            case ObjectOriented.StatementBlock block -> collectMutableCaptures(block, scope, mutableCaptures);
            case ObjectOriented.IfStatement ifStatement -> {
                collectMutableCaptures(ifStatement.condition(), scope, mutableCaptures);
                collectMutableCaptures(ifStatement.thenBranch(), scope, mutableCaptures);
                ifStatement.elseBranch().ifPresent(elseBranch -> collectMutableCaptures(elseBranch, scope, mutableCaptures));
            }
            case ObjectOriented.WhileStatement whileStatement -> {
                collectMutableCaptures(whileStatement.condition(), scope, mutableCaptures);
                collectMutableCaptures(whileStatement.body(), scope, mutableCaptures);
            }
            case ObjectOriented.DoWhileStatement doWhileStatement -> {
                collectMutableCaptures(doWhileStatement.body(), scope, mutableCaptures);
                collectMutableCaptures(doWhileStatement.condition(), scope, mutableCaptures);
            }
            case ObjectOriented.ForEachStatement forEachStatement -> {
                collectMutableCaptures(forEachStatement.iterable(), scope, mutableCaptures);
                var loopScope = scope.child();
                loopScope.declareImmutable(forEachStatement.name());
                collectMutableCaptures(forEachStatement.body(), loopScope, mutableCaptures);
            }
            case ObjectOriented.TryCatchStatement tryCatchStatement -> {
                collectMutableCaptures(tryCatchStatement.tryBlock(), scope, mutableCaptures);
                for (var catchClause : tryCatchStatement.catches()) {
                    var catchScope = scope.child();
                    catchScope.declareImmutable(catchClause.name());
                    collectMutableCaptures(catchClause.body(), catchScope, mutableCaptures);
                }
            }
            case ObjectOriented.LetStatement letStatement -> collectMutableCaptures(letStatement.expression(), scope, mutableCaptures);
            case ObjectOriented.LocalMethodStatement localMethodStatement -> collectMutableCaptures(localMethodStatement.body(), scope.child(), mutableCaptures);
            case ObjectOriented.MutableVariableStatement mutableVariableStatement -> collectMutableCaptures(mutableVariableStatement.expression(), scope, mutableCaptures);
            case ObjectOriented.AssignmentStatement assignmentStatement -> collectMutableCaptures(assignmentStatement.expression(), scope, mutableCaptures);
            case ObjectOriented.ExpressionStatement expressionStatement -> collectMutableCaptures(expressionStatement.expression(), scope, mutableCaptures);
            case ObjectOriented.ThrowStatement throwStatement -> collectMutableCaptures(throwStatement.expression(), scope, mutableCaptures);
            case ObjectOriented.ReturnStatement returnStatement -> collectMutableCaptures(returnStatement.expression(), scope, mutableCaptures);
        }
    }

    private void collectMutableCaptures(String expression, Scope scope, TreeSet<String> mutableCaptures) {
        var withoutStrings = expression.replaceAll("\"(?:[^\"\\\\]|\\\\.)*\"", " ");
        var matcher = IDENTIFIER_REFERENCE.matcher(withoutStrings);
        while (matcher.find()) {
            var name = matcher.group();
            if (scope.resolve(name) == Resolution.MUTABLE) {
                mutableCaptures.add(name);
            }
        }
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
