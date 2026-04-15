package dev.capylang.generator;

import dev.capylang.compiler.parser.ObjectOriented;
import dev.capylang.compiler.parser.ObjectOrientedModule;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public final class ObjectOrientedJavaGenerator {
    private static final Set<String> JAVA_KEYWORDS = Set.of(
            "abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class",
            "const", "continue", "default", "do", "double", "else", "enum", "extends", "final",
            "finally", "float", "for", "goto", "if", "implements", "import", "instanceof", "int",
            "interface", "long", "native", "new", "package", "private", "protected", "public",
            "return", "short", "static", "strictfp", "super", "switch", "synchronized", "this",
            "throw", "throws", "transient", "try", "void", "volatile", "while", "true", "false",
            "null", "record", "sealed", "permits", "var", "yield"
    );

    public List<GeneratedModule> generate(List<ObjectOrientedModule> modules) {
        return modules.stream()
                .flatMap(module -> generateModule(module).stream())
                .toList();
    }

    private List<GeneratedModule> generateModule(ObjectOrientedModule module) {
        var definitionsByName = module.objectOriented().definitions().stream()
                .collect(Collectors.toMap(
                        ObjectOriented.TypeDeclaration::name,
                        definition -> definition,
                        (left, right) -> left,
                        HashMap::new
                ));
        return module.objectOriented().definitions().stream()
                .map(definition -> new GeneratedModule(
                        relativePath(module, definition.name()),
                        renderType(module, definition, definitionsByName)
                ))
                .toList();
    }

    private Path relativePath(ObjectOrientedModule module, String typeName) {
        var normalizedPath = normalizePackageName(module.path()).replace('.', '/');
        return normalizedPath.isBlank()
                ? Path.of(typeName + ".java")
                : Path.of(normalizedPath, typeName + ".java");
    }

    private String renderType(
            ObjectOrientedModule module,
            ObjectOriented.TypeDeclaration declaration,
            Map<String, ObjectOriented.TypeDeclaration> definitionsByName
    ) {
        var code = new StringBuilder();
        var packageName = normalizePackageName(module.path());
        if (!packageName.isBlank()) {
            code.append("package ").append(packageName).append(";\n\n");
        }
        code.append("@javax.annotation.processing.Generated(\"Capybara Compiler\")\n");
        code.append(switch (declaration) {
            case ObjectOriented.ClassDeclaration classDeclaration -> renderClass(module, classDeclaration, definitionsByName);
            case ObjectOriented.InterfaceDeclaration interfaceDeclaration -> renderInterface(module, interfaceDeclaration);
            case ObjectOriented.TraitDeclaration traitDeclaration -> renderTrait(module, traitDeclaration);
        });
        return code.toString();
    }

    private String renderClass(
            ObjectOrientedModule module,
            ObjectOriented.ClassDeclaration declaration,
            Map<String, ObjectOriented.TypeDeclaration> definitionsByName
    ) {
        var code = new StringBuilder();
        var abstractClass = declaration.modifiers().contains("abstract");
        var openClass = declaration.modifiers().contains("open");
        var finalClass = !abstractClass && !openClass;
        var parents = classifyParents(declaration.parents(), definitionsByName);

        code.append("public ");
        if (abstractClass) {
            code.append("abstract ");
        } else if (finalClass) {
            code.append("final ");
        }
        code.append("class ").append(declaration.name());
        parents.classParent().ifPresent(parent -> code.append(" extends ").append(parent));
        if (!parents.interfaceParents().isEmpty()) {
            code.append(" implements ").append(String.join(", ", parents.interfaceParents()));
        }
        code.append(" {\n");

        var fields = declaration.members().stream()
                .filter(ObjectOriented.FieldDeclaration.class::isInstance)
                .map(ObjectOriented.FieldDeclaration.class::cast)
                .toList();
        var methods = declaration.members().stream()
                .filter(ObjectOriented.MethodDeclaration.class::isInstance)
                .map(ObjectOriented.MethodDeclaration.class::cast)
                .toList();
        var initBlocks = declaration.members().stream()
                .filter(ObjectOriented.InitBlock.class::isInstance)
                .map(ObjectOriented.InitBlock.class::cast)
                .toList();
        var constructorParameterNames = declaration.constructorParameters().stream()
                .map(ObjectOriented.Parameter::name)
                .collect(Collectors.toCollection(HashSet::new));

        for (var field : fields) {
            code.append("    ").append(renderFieldDeclaration(field, constructorParameterNames)).append("\n");
        }
        if (!fields.isEmpty()) {
            code.append("\n");
        }

        if (requiresConstructor(declaration, fields, initBlocks)) {
            code.append(renderConstructor(module, declaration, fields, initBlocks)).append("\n");
        }

        var parentNames = Stream.concat(
                        parents.classParent().stream(),
                        parents.interfaceParents().stream()
                )
                .map(this::simpleTypeName)
                .collect(Collectors.toUnmodifiableSet());
        for (int i = 0; i < methods.size(); i++) {
            code.append(renderClassMethod(module, declaration, methods.get(i), parentNames));
            if (i < methods.size() - 1) {
                code.append("\n");
            }
        }
        code.append("}\n");
        return code.toString();
    }

    private boolean requiresConstructor(
            ObjectOriented.ClassDeclaration declaration,
            List<ObjectOriented.FieldDeclaration> fields,
            List<ObjectOriented.InitBlock> initBlocks
    ) {
        if (!declaration.constructorParameters().isEmpty() || !initBlocks.isEmpty()) {
            return true;
        }
        var constructorParameterNames = declaration.constructorParameters().stream()
                .map(ObjectOriented.Parameter::name)
                .collect(Collectors.toCollection(HashSet::new));
        return fields.stream().anyMatch(field -> field.initializer()
                .filter(initializer -> referencesAny(initializer, constructorParameterNames))
                .isPresent());
    }

    private String renderConstructor(
            ObjectOrientedModule module,
            ObjectOriented.ClassDeclaration declaration,
            List<ObjectOriented.FieldDeclaration> fields,
            List<ObjectOriented.InitBlock> initBlocks
    ) {
        var code = new StringBuilder();
        code.append("    public ").append(declaration.name()).append("(");
        code.append(declaration.constructorParameters().stream()
                .map(parameter -> renderType(parameter.type(), false) + " " + sanitizeIdentifier(parameter.name()))
                .collect(Collectors.joining(", ")));
        code.append(") {\n");
        var constructorParameterNames = declaration.constructorParameters().stream()
                .map(ObjectOriented.Parameter::name)
                .collect(Collectors.toCollection(HashSet::new));

        for (var field : fields) {
            if (field.initializer().isPresent() && referencesAny(field.initializer().orElseThrow(), constructorParameterNames)) {
                code.append("        this.")
                        .append(sanitizeIdentifier(field.name()))
                        .append(" = ")
                        .append(renderExpression(module, field.initializer().orElseThrow(), Set.of()))
                        .append(";\n");
                continue;
            }
            if (field.initializer().isEmpty() && constructorParameterNames.contains(field.name())) {
                code.append("        this.")
                        .append(sanitizeIdentifier(field.name()))
                        .append(" = ")
                        .append(sanitizeIdentifier(field.name()))
                        .append(";\n");
            }
        }

        for (var initBlock : initBlocks) {
            appendStatementBlockContents(code, module, initBlock.body(), 2, Set.of());
        }
        code.append("    }\n");
        return code.toString();
    }

    private String renderFieldDeclaration(ObjectOriented.FieldDeclaration field, Set<String> constructorParameterNames) {
        var code = new StringBuilder();
        var visibility = renderClassVisibility(field.visibility());
        if (!visibility.isBlank()) {
            code.append(visibility).append(' ');
        }
        code.append(renderType(field.type(), false)).append(' ').append(sanitizeIdentifier(field.name()));
        if (field.initializer().isPresent() && !referencesAny(field.initializer().orElseThrow(), constructorParameterNames)) {
            code.append(" = ").append(renderExpression(null, field.initializer().orElseThrow(), Set.of()));
        }
        code.append(";");
        return code.toString();
    }

    private String renderClassMethod(
            ObjectOrientedModule module,
            ObjectOriented.ClassDeclaration owner,
            ObjectOriented.MethodDeclaration method,
            Set<String> parentNames
    ) {
        var code = new StringBuilder();
        var javaEntrypoint = isJavaEntrypoint(method);
        if (method.modifiers().contains("override")) {
            code.append("    @Override\n");
        }
        code.append("    ");
        var visibility = renderClassVisibility(method.visibility());
        if (!visibility.isBlank()) {
            code.append(visibility).append(' ');
        }
        var abstractMethod = method.modifiers().contains("abstract") || method.body().isEmpty();
        if (method.modifiers().contains("final")) {
            code.append("final ");
        } else if ((owner.modifiers().contains("open") || owner.modifiers().contains("abstract"))
                   && method.body().isPresent()
                   && !method.modifiers().contains("open")
                   && !method.modifiers().contains("override")
                   && !"private".equals(method.visibility())) {
            code.append("final ");
        }
        if (abstractMethod) {
            if (!owner.modifiers().contains("abstract")) {
                throw unsupported(module, "Concrete class `" + owner.name() + "` cannot declare body-less method `" + method.name() + "`");
            }
            code.append("abstract ");
        }
        if (javaEntrypoint) {
            ensureEntrypointCompatible(module, owner, method, parentNames);
            code.append("static ");
        }
        code.append(renderType(method.returnType(), false))
                .append(' ')
                .append(sanitizeIdentifier(method.name()))
                .append('(')
                .append(renderParameters(method.parameters()))
                .append(')');
        if (abstractMethod) {
            code.append(";\n");
            return code.toString();
        }
        code.append(" {\n");
        appendMethodBody(code, module, method, 2, parentNames);
        code.append("    }\n");
        if (javaEntrypoint) {
            code.append("\n").append(renderJavaEntrypointWrapper());
        }
        return code.toString();
    }

    private String renderInterface(ObjectOrientedModule module, ObjectOriented.InterfaceDeclaration declaration) {
        var code = new StringBuilder();
        code.append("public interface ").append(declaration.name());
        if (!declaration.parents().isEmpty()) {
            code.append(" extends ")
                    .append(declaration.parents().stream().map(parent -> renderTypeReference(parent.name())).collect(Collectors.joining(", ")));
        }
        code.append(" {\n");
        for (var member : declaration.members()) {
            if (!(member instanceof ObjectOriented.MethodDeclaration method)) {
                throw unsupported(module, "Interface `" + declaration.name() + "` can only contain methods");
            }
            code.append(renderInterfaceMethod(module, declaration.name(), method));
        }
        code.append("}\n");
        return code.toString();
    }

    private String renderTrait(ObjectOrientedModule module, ObjectOriented.TraitDeclaration declaration) {
        for (var member : declaration.members()) {
            if (member instanceof ObjectOriented.FieldDeclaration) {
                throw unsupported(module, "Trait fields are not supported by the Java backend in v1");
            }
            if (member instanceof ObjectOriented.InitBlock) {
                throw unsupported(module, "Trait init blocks are not supported by the Java backend in v1");
            }
        }
        var code = new StringBuilder();
        code.append("public interface ").append(declaration.name());
        if (!declaration.parents().isEmpty()) {
            code.append(" extends ")
                    .append(declaration.parents().stream().map(parent -> renderTypeReference(parent.name())).collect(Collectors.joining(", ")));
        }
        code.append(" {\n");
        for (var member : declaration.members()) {
            code.append(renderTraitMethod(module, declaration.name(), (ObjectOriented.MethodDeclaration) member));
        }
        code.append("}\n");
        return code.toString();
    }

    private String renderInterfaceMethod(ObjectOrientedModule module, String ownerName, ObjectOriented.MethodDeclaration method) {
        if (method.body().isPresent()) {
            throw unsupported(module, "Interface `" + ownerName + "` default methods are not supported in v1");
        }
        if (!"public".equals(method.visibility())) {
            throw unsupported(module, "Interface method `" + method.name() + "` must be public");
        }
        return "    " + renderType(method.returnType(), false) + " " + sanitizeIdentifier(method.name()) + "(" + renderParameters(method.parameters()) + ");\n";
    }

    private String renderTraitMethod(ObjectOrientedModule module, String ownerName, ObjectOriented.MethodDeclaration method) {
        var code = new StringBuilder();
        code.append("    ");
        if (method.modifiers().contains("override")) {
            throw unsupported(module, "Trait method `" + ownerName + "." + method.name() + "` cannot use `override` in the Java backend v1");
        }
        if (method.modifiers().contains("final")) {
            throw unsupported(module, "Trait method `" + ownerName + "." + method.name() + "` cannot be `final` in the Java backend v1");
        }
        if ("private".equals(method.visibility())) {
            if (method.body().isEmpty()) {
                throw unsupported(module, "Private trait method `" + method.name() + "` must have a body");
            }
            code.append("private ");
        } else if (!"public".equals(method.visibility())) {
            throw unsupported(module, "Trait method `" + ownerName + "." + method.name() + "` must be public or private");
        }

        if (method.body().isPresent() && !"private".equals(method.visibility())) {
            code.append("default ");
        }
        code.append(renderType(method.returnType(), false))
                .append(' ')
                .append(sanitizeIdentifier(method.name()))
                .append('(')
                .append(renderParameters(method.parameters()))
                .append(')');
        if (method.body().isEmpty()) {
            code.append(";\n");
            return code.toString();
        }
        code.append(" {\n");
        appendMethodBody(code, module, method, 2, Set.of());
        code.append("    }\n");
        return code.toString();
    }

    private void appendMethodBody(
            StringBuilder code,
            ObjectOrientedModule module,
            ObjectOriented.MethodDeclaration method,
            int indentLevel,
            Set<String> parentNames
    ) {
        method.body().ifPresent(body -> {
            if (body instanceof ObjectOriented.ExpressionBody expressionBody) {
                if ("void".equals(renderType(method.returnType(), false))) {
                    code.append(indent(indentLevel))
                            .append(renderExpression(module, expressionBody.expression(), parentNames))
                            .append(";\n");
                    return;
                }
                code.append(indent(indentLevel))
                        .append("return ")
                        .append(renderExpression(module, expressionBody.expression(), parentNames))
                        .append(";\n");
                return;
            }
            appendStatementBlockContents(code, module, (ObjectOriented.StatementBlock) body, indentLevel, parentNames);
        });
    }

    private void appendStatementBlockContents(
            StringBuilder code,
            ObjectOrientedModule module,
            ObjectOriented.StatementBlock block,
            int indentLevel,
            Set<String> parentNames
    ) {
        for (var statement : block.statements()) {
            appendStatement(code, module, statement, indentLevel, parentNames);
        }
    }

    private void appendStatement(
            StringBuilder code,
            ObjectOrientedModule module,
            ObjectOriented.Statement statement,
            int indentLevel,
            Set<String> parentNames
    ) {
        switch (statement) {
            case ObjectOriented.LetStatement letStatement -> code.append(indent(indentLevel))
                    .append("final ")
                    .append(letStatement.type().map(type -> renderType(type, false)).orElse("var"))
                    .append(' ')
                    .append(sanitizeIdentifier(letStatement.name()))
                    .append(" = ")
                    .append(renderExpression(module, letStatement.expression(), parentNames))
                    .append(";\n");
            case ObjectOriented.MutableVariableStatement mutableVariableStatement -> code.append(indent(indentLevel))
                    .append(mutableVariableStatement.type().map(type -> renderType(type, false)).orElse("var"))
                    .append(' ')
                    .append(sanitizeIdentifier(mutableVariableStatement.name()))
                    .append(" = ")
                    .append(renderExpression(module, mutableVariableStatement.expression(), parentNames))
                    .append(";\n");
            case ObjectOriented.AssignmentStatement assignmentStatement -> code.append(indent(indentLevel))
                    .append(sanitizeIdentifier(assignmentStatement.name()))
                    .append(" = ")
                    .append(renderExpression(module, assignmentStatement.expression(), parentNames))
                    .append(";\n");
            case ObjectOriented.ReturnStatement returnStatement -> code.append(indent(indentLevel))
                    .append("return ")
                    .append(renderExpression(module, returnStatement.expression(), parentNames))
                    .append(";\n");
            case ObjectOriented.IfStatement ifStatement -> {
                code.append(indent(indentLevel))
                        .append("if (")
                        .append(renderExpression(module, ifStatement.condition(), parentNames))
                        .append(") {\n");
                appendStatementBlockContents(code, module, ifStatement.thenBranch(), indentLevel + 1, parentNames);
                code.append(indent(indentLevel)).append('}');
                ifStatement.elseBranch().ifPresent(elseBranch -> {
                    if (elseBranch instanceof ObjectOriented.IfStatement nestedIf) {
                        code.append(" else ");
                        appendInlineIf(code, module, nestedIf, indentLevel, parentNames);
                    } else {
                        code.append(" else {\n");
                        appendStatementBlockContents(code, module, (ObjectOriented.StatementBlock) elseBranch, indentLevel + 1, parentNames);
                        code.append(indent(indentLevel)).append('}');
                    }
                });
                code.append("\n");
            }
            case ObjectOriented.WhileStatement whileStatement -> {
                code.append(indent(indentLevel))
                        .append("while (")
                        .append(renderExpression(module, whileStatement.condition(), parentNames))
                        .append(") {\n");
                appendStatementBlockContents(code, module, whileStatement.body(), indentLevel + 1, parentNames);
                code.append(indent(indentLevel)).append("}\n");
            }
            case ObjectOriented.DoWhileStatement doWhileStatement -> {
                code.append(indent(indentLevel)).append("do {\n");
                appendStatementBlockContents(code, module, doWhileStatement.body(), indentLevel + 1, parentNames);
                code.append(indent(indentLevel))
                        .append("} while (")
                        .append(renderExpression(module, doWhileStatement.condition(), parentNames))
                        .append(");\n");
            }
            case ObjectOriented.ForEachStatement forEachStatement -> {
                code.append(indent(indentLevel))
                        .append("for (")
                        .append(forEachStatement.type().map(type -> renderType(type, false)).orElse("var"))
                        .append(' ')
                        .append(sanitizeIdentifier(forEachStatement.name()))
                        .append(" : ")
                        .append(renderExpression(module, forEachStatement.iterable(), parentNames))
                        .append(") {\n");
                appendStatementBlockContents(code, module, forEachStatement.body(), indentLevel + 1, parentNames);
                code.append(indent(indentLevel)).append("}\n");
            }
            case ObjectOriented.StatementBlock nestedBlock -> {
                code.append(indent(indentLevel)).append("{\n");
                appendStatementBlockContents(code, module, nestedBlock, indentLevel + 1, parentNames);
                code.append(indent(indentLevel)).append("}\n");
            }
        }
    }

    private void appendInlineIf(
            StringBuilder code,
            ObjectOrientedModule module,
            ObjectOriented.IfStatement statement,
            int indentLevel,
            Set<String> parentNames
    ) {
        code.append("if (")
                .append(renderExpression(module, statement.condition(), parentNames))
                .append(") {\n");
        appendStatementBlockContents(code, module, statement.thenBranch(), indentLevel + 1, parentNames);
        code.append(indent(indentLevel)).append('}');
        statement.elseBranch().ifPresent(elseBranch -> {
            if (elseBranch instanceof ObjectOriented.IfStatement nestedIf) {
                code.append(" else ");
                appendInlineIf(code, module, nestedIf, indentLevel, parentNames);
            } else {
                code.append(" else {\n");
                appendStatementBlockContents(code, module, (ObjectOriented.StatementBlock) elseBranch, indentLevel + 1, parentNames);
                code.append(indent(indentLevel)).append('}');
            }
        });
    }

    private boolean isJavaEntrypoint(ObjectOriented.MethodDeclaration method) {
        return method.name().equals("main")
               && method.returnType().equals("int")
               && method.parameters().size() == 1
               && method.parameters().getFirst().type().equals("list[string]")
               && method.body().isPresent()
               && !method.modifiers().contains("abstract");
    }

    private void ensureEntrypointCompatible(
            ObjectOrientedModule module,
            ObjectOriented.ClassDeclaration owner,
            ObjectOriented.MethodDeclaration method,
            Set<String> parentNames
    ) {
        if (referencesThis(method) || referencesParents(method, parentNames)) {
            throw unsupported(module, "Entrypoint method `" + owner.name() + ".main` cannot use instance state or parent-qualified calls");
        }
    }

    private boolean referencesThis(ObjectOriented.MethodDeclaration method) {
        return methodExpressions(method).stream().anyMatch(expression -> expression.contains("this"));
    }

    private boolean referencesParents(ObjectOriented.MethodDeclaration method, Set<String> parentNames) {
        return methodExpressions(method).stream()
                .anyMatch(expression -> parentNames.stream().anyMatch(parent -> expression.contains(parent + ".")));
    }

    private List<String> methodExpressions(ObjectOriented.MethodDeclaration method) {
        return method.body().stream()
                .flatMap(body -> switch (body) {
                    case ObjectOriented.ExpressionBody expressionBody -> Stream.of(expressionBody.expression());
                    case ObjectOriented.StatementBlock statementBlock -> collectExpressions(statementBlock).stream();
                })
                .toList();
    }

    private List<String> collectExpressions(ObjectOriented.StatementBlock block) {
        var expressions = new ArrayList<String>();
        collectExpressions(block, expressions);
        return List.copyOf(expressions);
    }

    private void collectExpressions(ObjectOriented.StatementBlock block, List<String> expressions) {
        for (var statement : block.statements()) {
            switch (statement) {
                case ObjectOriented.LetStatement letStatement -> expressions.add(letStatement.expression());
                case ObjectOriented.MutableVariableStatement mutableVariableStatement -> expressions.add(mutableVariableStatement.expression());
                case ObjectOriented.AssignmentStatement assignmentStatement -> expressions.add(assignmentStatement.expression());
                case ObjectOriented.ReturnStatement returnStatement -> expressions.add(returnStatement.expression());
                case ObjectOriented.IfStatement ifStatement -> {
                    expressions.add(ifStatement.condition());
                    collectExpressions(ifStatement.thenBranch(), expressions);
                    ifStatement.elseBranch().ifPresent(elseBranch -> collectExpressions(elseBranch, expressions));
                }
                case ObjectOriented.WhileStatement whileStatement -> {
                    expressions.add(whileStatement.condition());
                    collectExpressions(whileStatement.body(), expressions);
                }
                case ObjectOriented.DoWhileStatement doWhileStatement -> {
                    collectExpressions(doWhileStatement.body(), expressions);
                    expressions.add(doWhileStatement.condition());
                }
                case ObjectOriented.ForEachStatement forEachStatement -> {
                    expressions.add(forEachStatement.iterable());
                    collectExpressions(forEachStatement.body(), expressions);
                }
                case ObjectOriented.StatementBlock nestedBlock -> collectExpressions(nestedBlock, expressions);
            }
        }
    }

    private void collectExpressions(ObjectOriented.Statement statement, List<String> expressions) {
        switch (statement) {
            case ObjectOriented.StatementBlock block -> collectExpressions(block, expressions);
            case ObjectOriented.IfStatement ifStatement -> {
                expressions.add(ifStatement.condition());
                collectExpressions(ifStatement.thenBranch(), expressions);
                ifStatement.elseBranch().ifPresent(elseBranch -> collectExpressions(elseBranch, expressions));
            }
            case ObjectOriented.WhileStatement whileStatement -> {
                expressions.add(whileStatement.condition());
                collectExpressions(whileStatement.body(), expressions);
            }
            case ObjectOriented.DoWhileStatement doWhileStatement -> {
                collectExpressions(doWhileStatement.body(), expressions);
                expressions.add(doWhileStatement.condition());
            }
            case ObjectOriented.ForEachStatement forEachStatement -> {
                expressions.add(forEachStatement.iterable());
                collectExpressions(forEachStatement.body(), expressions);
            }
            case ObjectOriented.LetStatement letStatement -> expressions.add(letStatement.expression());
            case ObjectOriented.MutableVariableStatement mutableVariableStatement -> expressions.add(mutableVariableStatement.expression());
            case ObjectOriented.AssignmentStatement assignmentStatement -> expressions.add(assignmentStatement.expression());
            case ObjectOriented.ReturnStatement returnStatement -> expressions.add(returnStatement.expression());
        }
    }

    private String renderJavaEntrypointWrapper() {
        return "    public static void main(java.lang.String[] args) {\n"
               + "        System.exit(main(java.util.List.of(args)));\n"
               + "    }\n";
    }

    private String renderParameters(List<ObjectOriented.Parameter> parameters) {
        return parameters.stream()
                .map(parameter -> renderType(parameter.type(), false) + " " + sanitizeIdentifier(parameter.name()))
                .collect(Collectors.joining(", "));
    }

    private ParentKinds classifyParents(
            List<ObjectOriented.TypeReference> parents,
            Map<String, ObjectOriented.TypeDeclaration> definitionsByName
    ) {
        Optional<String> classParent = Optional.empty();
        var interfaceParents = new ArrayList<String>();
        for (int index = 0; index < parents.size(); index++) {
            var parentName = parents.get(index).name();
            var localDeclaration = definitionsByName.get(parentName);
            if (classParent.isEmpty() && (localDeclaration instanceof ObjectOriented.ClassDeclaration || localDeclaration == null && index == 0)) {
                classParent = Optional.of(renderTypeReference(parentName));
            } else {
                interfaceParents.add(renderTypeReference(parentName));
            }
        }
        return new ParentKinds(classParent, List.copyOf(interfaceParents));
    }

    private String renderExpression(ObjectOrientedModule module, String expression, Set<String> parentNames) {
        var trimmed = expression.trim();
        if (trimmed.startsWith("match ")) {
            throw unsupported(module, "`match` expressions are not supported by the Java backend in v1");
        }
        var arrayWithValues = renderArrayWithValues(module, trimmed, parentNames);
        if (arrayWithValues.isPresent()) {
            return arrayWithValues.orElseThrow();
        }
        var sizedArray = renderSizedArray(module, trimmed, parentNames);
        if (sizedArray.isPresent()) {
            return sizedArray.orElseThrow();
        }
        if (trimmed.contains("=>") || trimmed.contains("|>") || trimmed.contains("|-") || trimmed.contains("|*")
            || trimmed.contains(".and.") || trimmed.contains(".or.") || trimmed.contains(".xor.") || trimmed.contains(".nand.")
            || trimmed.startsWith("[") || trimmed.startsWith("{")) {
            throw unsupported(module, "Expression `" + preview(trimmed) + "` is not supported by the Java backend in v1");
        }
        if (trimmed.startsWith("if ")) {
            return renderIfExpression(module, trimmed, parentNames);
        }
        if ("???".equals(trimmed)) {
            return "null";
        }
        for (var parentName : parentNames) {
            trimmed = trimmed.replaceAll("(^|[^A-Za-z0-9_])" + Pattern.quote(parentName) + "\\s*\\.", "$1super.");
        }
        return trimmed;
    }

    private Optional<String> renderArrayWithValues(ObjectOrientedModule module, String expression, Set<String> parentNames) {
        if (!expression.endsWith("}")) {
            return Optional.empty();
        }
        var braceIndex = findTopLevelChar(expression, '{');
        if (braceIndex < 0) {
            return Optional.empty();
        }
        var type = expression.substring(0, braceIndex).trim();
        if (!type.endsWith("[]")) {
            return Optional.empty();
        }
        var valuesSource = expression.substring(braceIndex + 1, expression.length() - 1).trim();
        var values = splitTopLevel(valuesSource).stream()
                .map(value -> renderExpression(module, value, parentNames))
                .collect(Collectors.joining(", "));
        return Optional.of("new " + renderType(type, false) + "{" + values + "}");
    }

    private Optional<String> renderSizedArray(ObjectOrientedModule module, String expression, Set<String> parentNames) {
        if (!expression.endsWith("]")) {
            return Optional.empty();
        }
        var sizeBracketIndex = findTopLevelChar(expression, '[');
        if (sizeBracketIndex < 0) {
            return Optional.empty();
        }
        var type = expression.substring(0, sizeBracketIndex).trim();
        if (type.isBlank() || type.endsWith("[]") || !isTypeLikePrefix(type)) {
            return Optional.empty();
        }
        var sizeExpression = expression.substring(sizeBracketIndex + 1, expression.length() - 1).trim();
        if (sizeExpression.isBlank()) {
            return Optional.empty();
        }
        return Optional.of("new " + renderType(type, false) + "[" + renderExpression(module, sizeExpression, parentNames) + "]");
    }

    private String renderIfExpression(ObjectOrientedModule module, String expression, Set<String> parentNames) {
        var thenIndex = findTopLevelKeyword(expression, " then ");
        var elseIndex = findTopLevelKeyword(expression, " else ");
        if (thenIndex < 0 || elseIndex < 0 || elseIndex <= thenIndex) {
            throw unsupported(module, "Unable to lower `if` expression `" + preview(expression) + "`");
        }
        var condition = expression.substring("if ".length(), thenIndex).trim();
        var thenBranch = expression.substring(thenIndex + " then ".length(), elseIndex).trim();
        var elseBranch = expression.substring(elseIndex + " else ".length()).trim();
        return "((" + renderExpression(module, condition, parentNames) + ") ? (" + renderExpression(module, thenBranch, parentNames) + ") : (" + renderExpression(module, elseBranch, parentNames) + "))";
    }

    private int findTopLevelKeyword(String value, String keyword) {
        var parens = 0;
        var brackets = 0;
        var braces = 0;
        char stringDelimiter = 0;
        for (int i = 0; i <= value.length() - keyword.length(); i++) {
            var current = value.charAt(i);
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
                case '(' -> parens++;
                case ')' -> parens--;
                case '[' -> brackets++;
                case ']' -> brackets--;
                case '{' -> braces++;
                case '}' -> braces--;
                default -> {
                }
            }
            if (parens == 0 && brackets == 0 && braces == 0 && value.startsWith(keyword, i)) {
                return i;
            }
        }
        return -1;
    }

    private int findTopLevelChar(String value, char target) {
        var parens = 0;
        var brackets = 0;
        var braces = 0;
        char stringDelimiter = 0;
        for (int i = 0; i < value.length(); i++) {
            var current = value.charAt(i);
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
            if (parens == 0 && brackets == 0 && braces == 0 && current == target) {
                return i;
            }
            switch (current) {
                case '(' -> parens++;
                case ')' -> parens--;
                case '[' -> brackets++;
                case ']' -> brackets--;
                case '{' -> braces++;
                case '}' -> braces--;
                default -> {
                }
            }
        }
        return -1;
    }

    private List<String> splitTopLevel(String value) {
        if (value.isBlank()) {
            return List.of();
        }
        var result = new ArrayList<String>();
        var current = new StringBuilder();
        var parens = 0;
        var brackets = 0;
        var braces = 0;
        char stringDelimiter = 0;
        for (int i = 0; i < value.length(); i++) {
            var ch = value.charAt(i);
            if (stringDelimiter != 0) {
                current.append(ch);
                if (ch == '\\' && i + 1 < value.length()) {
                    current.append(value.charAt(++i));
                    continue;
                }
                if (ch == stringDelimiter) {
                    stringDelimiter = 0;
                }
                continue;
            }
            if (ch == '"' || ch == '\'') {
                stringDelimiter = ch;
                current.append(ch);
                continue;
            }
            switch (ch) {
                case '(' -> parens++;
                case ')' -> parens--;
                case '[' -> brackets++;
                case ']' -> brackets--;
                case '{' -> braces++;
                case '}' -> braces--;
                case ',' -> {
                    if (parens == 0 && brackets == 0 && braces == 0) {
                        result.add(current.toString().trim());
                        current.setLength(0);
                        continue;
                    }
                }
                default -> {
                }
            }
            current.append(ch);
        }
        if (!current.isEmpty()) {
            result.add(current.toString().trim());
        }
        return result;
    }

    private boolean isTypeLikePrefix(String value) {
        return switch (value) {
            case "byte", "int", "long", "double", "float", "bool", "string", "any", "data", "void" -> true;
            default -> value.startsWith("/") || Character.isUpperCase(value.charAt(0)) || value.startsWith("_");
        };
    }

    private String renderType(String type, boolean boxed) {
        var trimmed = type.trim();
        if (trimmed.endsWith("[]")) {
            return renderType(trimmed.substring(0, trimmed.length() - 2), false) + "[]";
        }
        if (trimmed.startsWith("list[") && trimmed.endsWith("]")) {
            return "java.util.List<" + renderType(innerType(trimmed), true) + ">";
        }
        if (trimmed.startsWith("set[") && trimmed.endsWith("]")) {
            return "java.util.Set<" + renderType(innerType(trimmed), true) + ">";
        }
        if (trimmed.startsWith("dict[") && trimmed.endsWith("]")) {
            return "java.util.Map<String, " + renderType(innerType(trimmed), true) + ">";
        }
        return switch (trimmed) {
            case "byte" -> boxed ? "Byte" : "byte";
            case "int" -> boxed ? "Integer" : "int";
            case "long" -> boxed ? "Long" : "long";
            case "double" -> boxed ? "Double" : "double";
            case "float" -> boxed ? "Float" : "float";
            case "bool" -> boxed ? "Boolean" : "boolean";
            case "string" -> "String";
            case "any" -> "Object";
            case "void" -> "void";
            default -> renderTypeReference(trimmed);
        };
    }

    private String innerType(String collectionType) {
        return collectionType.substring(collectionType.indexOf('[') + 1, collectionType.length() - 1).trim();
    }

    private String renderTypeReference(String name) {
        if (name.startsWith("/")) {
            var path = name.substring(1).replace('/', '.');
            var parts = path.split("\\.");
            var normalized = new ArrayList<String>();
            for (int i = 0; i < parts.length; i++) {
                normalized.add(i == parts.length - 1 ? parts[i] : normalizePackageSegment(parts[i]));
            }
            return String.join(".", normalized);
        }
        return name;
    }

    private String simpleTypeName(String name) {
        var reference = renderTypeReference(name);
        var separator = reference.lastIndexOf('.');
        return separator >= 0 ? reference.substring(separator + 1) : reference;
    }

    private String renderClassVisibility(String visibility) {
        return switch (visibility) {
            case "public" -> "public";
            case "private" -> "private";
            case "local" -> "";
            default -> throw new IllegalArgumentException("Unknown visibility `" + visibility + "`");
        };
    }

    private boolean referencesAny(String expression, Set<String> names) {
        return names.stream()
                .map(name -> Pattern.compile("(^|[^A-Za-z0-9_])" + Pattern.quote(name) + "([^A-Za-z0-9_]|$)"))
                .anyMatch(pattern -> pattern.matcher(expression).find());
    }

    private String normalizePackageName(String path) {
        var normalized = path.replace('\\', '/');
        return List.of(normalized.split("/")).stream()
                .filter(part -> !part.isBlank())
                .map(this::normalizePackageSegment)
                .collect(Collectors.joining("."));
    }

    private String normalizePackageSegment(String rawSegment) {
        var sanitized = sanitizeIdentifier(rawSegment.replace('-', '_'));
        return sanitized.isBlank() ? "pkg" : sanitized;
    }

    private String sanitizeIdentifier(String value) {
        if (JAVA_KEYWORDS.contains(value)) {
            return value + "_";
        }
        return value;
    }

    private String indent(int level) {
        return "    ".repeat(level);
    }

    private IllegalArgumentException unsupported(ObjectOrientedModule module, String message) {
        var file = module == null ? ".coo" : module.sourceKind().moduleFile(module.path(), module.name());
        return new IllegalArgumentException(file + ": " + message);
    }

    private String preview(String expression) {
        return expression.replaceAll("\\s+", " ").trim();
    }

    private record ParentKinds(Optional<String> classParent, List<String> interfaceParents) {
    }
}
