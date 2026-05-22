package dev.capylang.compiler;

import dev.capylang.compiler.parser.ObjectOriented;
import dev.capylang.compiler.parser.ObjectOrientedModule;

import java.util.HashSet;
import java.util.Map;
import java.util.List;
import java.util.TreeSet;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public final class ObjectOrientedValidator {
    public static final ObjectOrientedValidator INSTANCE = new ObjectOrientedValidator();
    private static final Pattern IDENTIFIER_REFERENCE = Pattern.compile("\\b[_a-z][_A-Za-z0-9]*\\b");
    private static final Pattern CALL_EXPRESSION = Pattern.compile(".*(?:\\)|\\]|[A-Za-z_][A-Za-z0-9_]*)\\s*\\([^()]*\\)\\s*$");
    private static final Pattern STRING_LITERAL = Pattern.compile("\"(?:[^\"\\\\]|\\\\.)*\"|'(?:[^'\\\\]|\\\\.)*'");
    // OO expressions are raw strings until backend generation, so reject the unsafe escape hatch by call name.
    private static final Pattern UNSAFE_RUN_CALL = Pattern.compile(
            "(?s)(?:\\.\\s*|(?:^|[^A-Za-z0-9_\\.]))(?:unsafe_run|unsafeRun)\\s*\\("
    );

    public Result<List<ObjectOrientedModule>> validate(List<ObjectOrientedModule> modules) {
        var errors = new TreeSet<Result.Error.SingleError>();
        modules.forEach(module -> validateModule(module, errors));
        return errors.isEmpty() ? Result.success(modules) : new Result.Error<>(errors);
    }

    private void validateModule(ObjectOrientedModule module, TreeSet<Result.Error.SingleError> errors) {
        validateNativeProviders(module, errors);
        validateNativeProviderCalls(module, errors);
        module.objectOriented().definitions().forEach(definition -> validateDefinition(module, definition, errors));
    }

    private void validateNativeProviders(ObjectOrientedModule module, TreeSet<Result.Error.SingleError> errors) {
        var typeNames = module.objectOriented().definitions().stream()
                .map(ObjectOriented.TypeDeclaration::name)
                .collect(java.util.stream.Collectors.toUnmodifiableSet());
        var providerNames = new HashSet<String>();
        for (var provider : module.objectOriented().nativeProviders()) {
            if (!providerNames.add(provider.name())) {
                errors.add(nativeProviderError(
                        module,
                        provider,
                        "DuplicateProvider: Native provider `" + provider.name() + "` duplicates another provider in module `" + module.name()
                        + "` for target `" + provider.targetType() + "` with qualifier `" + provider.qualifier() + "`"
                ));
            }
            if (typeNames.contains(provider.name())) {
                errors.add(nativeProviderError(
                        module,
                        provider,
                        "TypeMismatch: Native provider `" + provider.name() + "` collides with type name `" + provider.name()
                        + "` in module `" + module.name() + "` for target `" + provider.targetType()
                        + "` with qualifier `" + provider.qualifier() + "`"
                ));
            }
        }
    }

    private Result.Error.SingleError nativeProviderError(
            ObjectOrientedModule module,
            ObjectOriented.NativeProviderDeclaration provider,
            String message
    ) {
        var sourceFile = module.moduleFile();
        return new Result.Error.SingleError(
                0,
                0,
                sourceFile,
                message + " in source `" + sourceFile + "`"
        );
    }

    private void validateNativeProviderCalls(ObjectOrientedModule module, TreeSet<Result.Error.SingleError> errors) {
        if (module.objectOriented().nativeProviders().isEmpty()) {
            return;
        }
        var providersByName = module.objectOriented().nativeProviders().stream()
                .collect(Collectors.toUnmodifiableMap(
                        ObjectOriented.NativeProviderDeclaration::name,
                        java.util.function.Function.identity(),
                        (first, ignored) -> first
                ));
        for (var definition : module.objectOriented().definitions()) {
            for (var member : definition.members()) {
                validateNativeProviderCalls(module, providersByName, member, errors);
            }
        }
    }

    private void validateNativeProviderCalls(
            ObjectOrientedModule module,
            Map<String, ObjectOriented.NativeProviderDeclaration> providersByName,
            ObjectOriented.MemberDeclaration member,
            TreeSet<Result.Error.SingleError> errors
    ) {
        switch (member) {
            case ObjectOriented.FieldDeclaration fieldDeclaration ->
                    fieldDeclaration.initializer().ifPresent(expression -> validateNativeProviderCalls(module, providersByName, expression, errors));
            case ObjectOriented.MethodDeclaration methodDeclaration ->
                    methodDeclaration.body().ifPresent(body -> validateNativeProviderCalls(module, providersByName, body, errors));
            case ObjectOriented.InitBlock initBlock ->
                    validateNativeProviderCalls(module, providersByName, initBlock.body(), errors);
        }
    }

    private void validateNativeProviderCalls(
            ObjectOrientedModule module,
            Map<String, ObjectOriented.NativeProviderDeclaration> providersByName,
            ObjectOriented.MethodBody body,
            TreeSet<Result.Error.SingleError> errors
    ) {
        switch (body) {
            case ObjectOriented.ExpressionBody expressionBody -> validateNativeProviderCalls(module, providersByName, expressionBody.expression(), errors);
            case ObjectOriented.StatementBlock statementBlock -> validateNativeProviderCalls(module, providersByName, statementBlock, errors);
        }
    }

    private void validateNativeProviderCalls(
            ObjectOrientedModule module,
            Map<String, ObjectOriented.NativeProviderDeclaration> providersByName,
            ObjectOriented.StatementBlock block,
            TreeSet<Result.Error.SingleError> errors
    ) {
        for (var statement : block.statements()) {
            validateNativeProviderCalls(module, providersByName, statement, errors);
        }
    }

    private void validateNativeProviderCalls(
            ObjectOrientedModule module,
            Map<String, ObjectOriented.NativeProviderDeclaration> providersByName,
            ObjectOriented.Statement statement,
            TreeSet<Result.Error.SingleError> errors
    ) {
        switch (statement) {
            case ObjectOriented.LetStatement letStatement -> validateNativeProviderCalls(module, providersByName, letStatement.expression(), errors);
            case ObjectOriented.LocalMethodStatement localMethodStatement -> validateNativeProviderCalls(module, providersByName, localMethodStatement.body(), errors);
            case ObjectOriented.MutableVariableStatement mutableVariableStatement -> validateNativeProviderCalls(module, providersByName, mutableVariableStatement.expression(), errors);
            case ObjectOriented.AssignmentStatement assignmentStatement -> validateNativeProviderCalls(module, providersByName, assignmentStatement.expression(), errors);
            case ObjectOriented.ExpressionStatement expressionStatement -> validateNativeProviderCalls(module, providersByName, expressionStatement.expression(), errors);
            case ObjectOriented.ThrowStatement throwStatement -> validateNativeProviderCalls(module, providersByName, throwStatement.expression(), errors);
            case ObjectOriented.ReturnStatement returnStatement -> validateNativeProviderCalls(module, providersByName, returnStatement.expression(), errors);
            case ObjectOriented.IfStatement ifStatement -> {
                validateNativeProviderCalls(module, providersByName, ifStatement.condition(), errors);
                validateNativeProviderCalls(module, providersByName, ifStatement.thenBranch(), errors);
                ifStatement.elseBranch().ifPresent(elseBranch -> validateNativeProviderCalls(module, providersByName, elseBranch, errors));
            }
            case ObjectOriented.TryCatchStatement tryCatchStatement -> {
                validateNativeProviderCalls(module, providersByName, tryCatchStatement.tryBlock(), errors);
                tryCatchStatement.catches().forEach(catchClause -> validateNativeProviderCalls(module, providersByName, catchClause.body(), errors));
            }
            case ObjectOriented.WhileStatement whileStatement -> {
                validateNativeProviderCalls(module, providersByName, whileStatement.condition(), errors);
                validateNativeProviderCalls(module, providersByName, whileStatement.body(), errors);
            }
            case ObjectOriented.DoWhileStatement doWhileStatement -> {
                validateNativeProviderCalls(module, providersByName, doWhileStatement.body(), errors);
                validateNativeProviderCalls(module, providersByName, doWhileStatement.condition(), errors);
            }
            case ObjectOriented.ForEachStatement forEachStatement -> {
                validateNativeProviderCalls(module, providersByName, forEachStatement.iterable(), errors);
                validateNativeProviderCalls(module, providersByName, forEachStatement.body(), errors);
            }
            case ObjectOriented.StatementBlock nestedBlock -> validateNativeProviderCalls(module, providersByName, nestedBlock, errors);
        }
    }

    private void validateNativeProviderCalls(
            ObjectOrientedModule module,
            Map<String, ObjectOriented.NativeProviderDeclaration> providersByName,
            String expression,
            TreeSet<Result.Error.SingleError> errors
    ) {
        for (var provider : providersByName.values()) {
            if (callsProviderWithArguments(expression, provider.name())) {
                errors.add(new Result.Error.SingleError(
                        0,
                        0,
                        module.moduleFile(),
                        "TypeMismatch: Native provider `" + provider.name() + "` for target `" + provider.targetType()
                        + "` with qualifier `" + provider.qualifier() + "` does not accept arguments; call it as `"
                        + provider.name() + "()` in source `" + module.moduleFile() + "`"
                ));
            }
        }
    }

    private boolean callsProviderWithArguments(String expression, String providerName) {
        var index = 0;
        while (index < expression.length()) {
            var current = expression.charAt(index);
            if (current == '"' || current == '\'') {
                index = skipStringLiteral(expression, index);
                continue;
            }
            if (!matchesDirectIdentifier(expression, index, providerName)) {
                index++;
                continue;
            }
            var openParen = skipWhitespace(expression, index + providerName.length());
            if (openParen < expression.length() && expression.charAt(openParen) == '(') {
                var closeParen = matchingParen(expression, openParen);
                if (closeParen > openParen && !expression.substring(openParen + 1, closeParen).trim().isEmpty()) {
                    return true;
                }
            }
            index += providerName.length();
        }
        return false;
    }

    private int skipStringLiteral(String expression, int start) {
        var delimiter = expression.charAt(start);
        var index = start + 1;
        while (index < expression.length()) {
            var current = expression.charAt(index);
            if (current == '\\') {
                index += 2;
                continue;
            }
            index++;
            if (current == delimiter) {
                break;
            }
        }
        return index;
    }

    private boolean matchesDirectIdentifier(String expression, int index, String identifier) {
        if (!expression.startsWith(identifier, index)) {
            return false;
        }
        if (index + identifier.length() < expression.length()
            && isIdentifierPart(expression.charAt(index + identifier.length()))) {
            return false;
        }
        var previous = index - 1;
        while (previous >= 0 && Character.isWhitespace(expression.charAt(previous))) {
            previous--;
        }
        return previous < 0 || (!isIdentifierPart(expression.charAt(previous)) && expression.charAt(previous) != '.');
    }

    private boolean isIdentifierPart(char value) {
        return Character.isLetterOrDigit(value) || value == '_';
    }

    private int skipWhitespace(String expression, int index) {
        while (index < expression.length() && Character.isWhitespace(expression.charAt(index))) {
            index++;
        }
        return index;
    }

    private int matchingParen(String expression, int openParen) {
        var depth = 0;
        for (var index = openParen; index < expression.length(); index++) {
            var current = expression.charAt(index);
            if (current == '"' || current == '\'') {
                index = skipStringLiteral(expression, index) - 1;
                continue;
            }
            if (current == '(') {
                depth++;
            } else if (current == ')') {
                depth--;
                if (depth == 0) {
                    return index;
                }
            }
        }
        return -1;
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
            TreeSet<Result.Error.SingleError> errors
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

    private void validateLocalMethod(
            ObjectOrientedModule module,
            String owner,
            ObjectOriented.LocalMethodStatement localMethodStatement,
            Scope scope,
            TreeSet<Result.Error.SingleError> errors
    ) {
        var mutableCaptures = new TreeSet<String>();
        var localScope = scope.child();
        localMethodStatement.parameters().forEach(parameter -> localScope.declareImmutable(parameter.name()));
        localScope.declareImmutable(localMethodStatement.name());
        collectMutableCaptures(localMethodStatement.body(), localScope, mutableCaptures);
        if (!mutableCaptures.isEmpty()) {
            errors.add(new Result.Error.SingleError(
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
            TreeSet<Result.Error.SingleError> errors
    ) {
        if (isCallExpression(expressionStatement.expression())) {
            return;
        }
        errors.add(new Result.Error.SingleError(
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
            TreeSet<Result.Error.SingleError> errors
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
            TreeSet<Result.Error.SingleError> errors
    ) {
        for (var statement : block.statements()) {
            validateUnsafeEffectRunUsage(module, owner, statement, errors);
        }
    }

    private void validateUnsafeEffectRunUsage(
            ObjectOrientedModule module,
            String owner,
            ObjectOriented.Statement statement,
            TreeSet<Result.Error.SingleError> errors
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
            TreeSet<Result.Error.SingleError> errors
    ) {
        var withoutStrings = STRING_LITERAL.matcher(expression).replaceAll(" ");
        if (!UNSAFE_RUN_CALL.matcher(withoutStrings).find()) {
            return;
        }
        errors.add(new Result.Error.SingleError(
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
