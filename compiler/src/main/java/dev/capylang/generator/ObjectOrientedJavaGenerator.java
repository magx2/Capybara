package dev.capylang.generator;

import dev.capylang.compiler.parser.ObjectOriented;
import dev.capylang.compiler.parser.ObjectOrientedModule;

import java.nio.file.Path;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public final class ObjectOrientedJavaGenerator {
    private static final Pattern SIMPLE_TYPE_REFERENCE = Pattern.compile("\\b_*[A-Z][A-Za-z0-9_]*\\b");
    private static final Set<String> JAVA_KEYWORDS = Set.of(
            "abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class",
            "const", "continue", "default", "do", "double", "else", "enum", "extends", "final",
            "finally", "float", "for", "goto", "if", "implements", "import", "instanceof", "int",
            "interface", "long", "native", "new", "package", "private", "protected", "public",
            "return", "short", "static", "strictfp", "super", "switch", "synchronized", "this",
            "throw", "throws", "transient", "try", "void", "volatile", "while", "true", "false",
            "null", "record", "sealed", "permits", "var", "yield"
    );
    private int syntheticCounter = 0;

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
        appendImports(code, module);
        if (!module.imports().isEmpty()) {
            code.append("\n");
        }
        code.append("@javax.annotation.processing.Generated(\"Capybara Compiler\")\n");
        code.append(switch (declaration) {
            case ObjectOriented.ClassDeclaration classDeclaration -> renderClass(module, classDeclaration, definitionsByName);
            case ObjectOriented.InterfaceDeclaration interfaceDeclaration -> renderInterface(module, interfaceDeclaration, definitionsByName);
            case ObjectOriented.TraitDeclaration traitDeclaration -> renderTrait(module, traitDeclaration, definitionsByName);
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

        appendDocComments(code, declaration.comments(), 0);
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
        var requiresConstructor = requiresConstructor(declaration, fields, initBlocks);
        var constructorParameterNames = declaration.constructorParameters().stream()
                .map(ObjectOriented.Parameter::name)
                .collect(Collectors.toCollection(HashSet::new));

        for (var field : fields) {
            code.append(renderFieldDeclaration(field, constructorParameterNames)).append("\n");
        }
        if (!fields.isEmpty()) {
            code.append("\n");
        }

        if (requiresConstructor) {
            code.append(renderConstructor(module, declaration, fields, initBlocks)).append("\n");
        }

        if (usesThrowSupport(declaration)) {
            code.append(renderThrowHelper()).append("\n");
        }
        if (usesMatchSupport(declaration)) {
            code.append(renderMatchHelper()).append("\n");
        }
        code.append(renderReflectionMethod(module, declaration, definitionsByName)).append("\n");

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

    private void appendImports(StringBuilder code, ObjectOrientedModule module) {
        var imports = Stream.concat(
                        module.imports().stream()
                .flatMap(importDeclaration -> importDeclaration.symbols().stream()
                        .filter(symbol -> !"*".equals(symbol))
                        .filter(symbol -> !importDeclaration.excludedSymbols().contains(symbol))
                        .filter(symbol -> !symbol.isBlank())
                        .filter(symbol -> Character.isUpperCase(symbol.charAt(0)))
                        .map(symbol -> renderImportedSymbolReference(module, importDeclaration.moduleName(), symbol))),
                        inferSameModuleImports(module).stream()
                )
                .distinct()
                .sorted()
                .toList();
        imports.forEach(importRef -> code.append("import ").append(importRef).append(";\n"));
    }

    private List<String> inferSameModuleImports(ObjectOrientedModule module) {
        var localDefinitions = module.objectOriented().definitions().stream()
                .map(ObjectOriented.TypeDeclaration::name)
                .collect(Collectors.toUnmodifiableSet());
        var explicitlyImportedSymbols = module.imports().stream()
                .flatMap(importDeclaration -> importDeclaration.symbols().stream())
                .filter(symbol -> !"*".equals(symbol))
                .filter(symbol -> !symbol.isBlank())
                .collect(Collectors.toUnmodifiableSet());
        var ownerReference = renderOwnerReference(normalizePackageName(module.path()), module.name());
        return referencedTypeTokens(module).stream()
                .filter(symbol -> !localDefinitions.contains(symbol))
                .filter(symbol -> !module.name().equals(symbol))
                .filter(symbol -> !explicitlyImportedSymbols.contains(symbol))
                .map(symbol -> ownerReference + "." + symbol)
                .distinct()
                .sorted()
                .toList();
    }

    private Set<String> referencedTypeTokens(ObjectOrientedModule module) {
        var references = new HashSet<String>();
        for (var definition : module.objectOriented().definitions()) {
            definition.parents().forEach(parent -> collectTypeTokens(parent.name(), references));
            for (var member : definition.members()) {
                switch (member) {
                    case ObjectOriented.FieldDeclaration field -> {
                        collectTypeTokens(field.type(), references);
                        field.initializer().ifPresent(initializer -> collectTypeTokens(initializer, references));
                    }
                    case ObjectOriented.MethodDeclaration method -> {
                        method.parameters().forEach(parameter -> collectTypeTokens(parameter.type(), references));
                        collectTypeTokens(method.returnType(), references);
                        method.body().ifPresent(body -> collectTypeTokens(body, references));
                    }
                    case ObjectOriented.InitBlock initBlock -> collectTypeTokens(initBlock.body(), references);
                }
            }
        }
        return Set.copyOf(references);
    }

    private void collectTypeTokens(ObjectOriented.MethodBody body, Set<String> references) {
        switch (body) {
            case ObjectOriented.ExpressionBody expressionBody -> collectTypeTokens(expressionBody.expression(), references);
            case ObjectOriented.StatementBlock statementBlock -> collectTypeTokens(statementBlock, references);
        }
    }

    private void collectTypeTokens(ObjectOriented.StatementBlock block, Set<String> references) {
        block.statements().forEach(statement -> collectTypeTokens(statement, references));
    }

    private void collectTypeTokens(ObjectOriented.Statement statement, Set<String> references) {
        switch (statement) {
            case ObjectOriented.LetStatement letStatement -> {
                letStatement.type().ifPresent(type -> collectTypeTokens(type, references));
                collectTypeTokens(letStatement.expression(), references);
            }
            case ObjectOriented.LocalMethodStatement localMethodStatement -> {
                localMethodStatement.parameters().forEach(parameter -> collectTypeTokens(parameter.type(), references));
                collectTypeTokens(localMethodStatement.returnType(), references);
                collectTypeTokens(localMethodStatement.body(), references);
            }
            case ObjectOriented.MutableVariableStatement mutableVariableStatement -> {
                mutableVariableStatement.type().ifPresent(type -> collectTypeTokens(type, references));
                collectTypeTokens(mutableVariableStatement.expression(), references);
            }
            case ObjectOriented.AssignmentStatement assignmentStatement -> collectTypeTokens(assignmentStatement.expression(), references);
            case ObjectOriented.ExpressionStatement expressionStatement -> collectTypeTokens(expressionStatement.expression(), references);
            case ObjectOriented.ThrowStatement throwStatement -> collectTypeTokens(throwStatement.expression(), references);
            case ObjectOriented.ReturnStatement returnStatement -> collectTypeTokens(returnStatement.expression(), references);
            case ObjectOriented.IfStatement ifStatement -> {
                collectTypeTokens(ifStatement.condition(), references);
                collectTypeTokens(ifStatement.thenBranch(), references);
                ifStatement.elseBranch().ifPresent(elseBranch -> collectTypeTokens(elseBranch, references));
            }
            case ObjectOriented.TryCatchStatement tryCatchStatement -> {
                collectTypeTokens(tryCatchStatement.tryBlock(), references);
                tryCatchStatement.catches().forEach(catchClause -> collectTypeTokens(catchClause.body(), references));
            }
            case ObjectOriented.WhileStatement whileStatement -> {
                collectTypeTokens(whileStatement.condition(), references);
                collectTypeTokens(whileStatement.body(), references);
            }
            case ObjectOriented.DoWhileStatement doWhileStatement -> {
                collectTypeTokens(doWhileStatement.body(), references);
                collectTypeTokens(doWhileStatement.condition(), references);
            }
            case ObjectOriented.ForEachStatement forEachStatement -> {
                forEachStatement.type().ifPresent(type -> collectTypeTokens(type, references));
                collectTypeTokens(forEachStatement.iterable(), references);
                collectTypeTokens(forEachStatement.body(), references);
            }
            case ObjectOriented.StatementBlock nestedBlock -> collectTypeTokens(nestedBlock, references);
        }
    }

    private void collectTypeTokens(String source, Set<String> references) {
        var withoutStringLiterals = source
                .replaceAll("\"(?:[^\"\\\\]|\\\\.)*\"", " ")
                .replaceAll("'(?:[^'\\\\]|\\\\.)*'", " ");
        var matcher = SIMPLE_TYPE_REFERENCE.matcher(withoutStringLiterals);
        while (matcher.find()) {
            var start = matcher.start();
            var end = matcher.end();
            var previous = start > 0 ? withoutStringLiterals.charAt(start - 1) : '\0';
            var next = end < withoutStringLiterals.length() ? withoutStringLiterals.charAt(end) : '\0';
            if (previous == '.' || next == '.') {
                continue;
            }
            references.add(matcher.group());
        }
    }

    private String renderImportedSymbolReference(ObjectOrientedModule module, String moduleName, String symbol) {
        return renderImportedOwnerReference(module, moduleName) + "." + symbol;
    }

    private String renderImportedOwnerReference(ObjectOrientedModule module, String moduleName) {
        return moduleName.startsWith("/")
                ? renderTypeReference(moduleName)
                : renderOwnerReference(normalizePackageName(module.path()), moduleName);
    }

    private String renderOwnerReference(String packageName, String ownerName) {
        return packageName.isBlank() ? ownerName : packageName + "." + ownerName;
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
                        .append(renderExpression(module, field.initializer().orElseThrow(), Set.of(), LocalMethodBindings.empty()))
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
            appendStatementBlockContents(code, module, initBlock.body(), 2, Set.of(), LocalMethodBindings.empty());
        }
        code.append("    }\n");
        return code.toString();
    }

    private String renderFieldDeclaration(ObjectOriented.FieldDeclaration field, Set<String> constructorParameterNames) {
        var code = new StringBuilder();
        appendDocComments(code, field.comments(), 1);
        var visibility = renderClassVisibility(field.visibility());
        if (!visibility.isBlank()) {
            code.append(visibility).append(' ');
        }
        code.append(renderType(field.type(), false)).append(' ').append(sanitizeIdentifier(field.name()));
        if (field.initializer().isPresent() && !referencesAny(field.initializer().orElseThrow(), constructorParameterNames)) {
            code.append(" = ").append(renderExpression(null, field.initializer().orElseThrow(), Set.of(), LocalMethodBindings.empty()));
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
        if (javaEntrypoint) {
            ensureEntrypointCompatible(module, owner, method, parentNames);
        }
        appendDocComments(code, method.comments(), 1);
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

    private String renderInterface(
            ObjectOrientedModule module,
            ObjectOriented.InterfaceDeclaration declaration,
            Map<String, ObjectOriented.TypeDeclaration> definitionsByName
    ) {
        var code = new StringBuilder();
        appendDocComments(code, declaration.comments(), 0);
        code.append("public interface ").append(declaration.name());
        if (!declaration.parents().isEmpty()) {
            code.append(" extends ")
                    .append(declaration.parents().stream().map(parent -> renderTypeReference(parent.name())).collect(Collectors.joining(", ")));
        }
        code.append(" {\n");
        code.append(renderReflectionMethod(module, declaration, definitionsByName)).append("\n");
        for (var member : declaration.members()) {
            if (!(member instanceof ObjectOriented.MethodDeclaration method)) {
                throw unsupported(module, "Interface `" + declaration.name() + "` can only contain methods");
            }
            code.append(renderInterfaceMethod(module, declaration.name(), method));
        }
        code.append("}\n");
        return code.toString();
    }

    private String renderTrait(
            ObjectOrientedModule module,
            ObjectOriented.TraitDeclaration declaration,
            Map<String, ObjectOriented.TypeDeclaration> definitionsByName
    ) {
        for (var member : declaration.members()) {
            if (member instanceof ObjectOriented.FieldDeclaration) {
                throw unsupported(module, "Trait fields are not supported by the Java backend in v1");
            }
            if (member instanceof ObjectOriented.InitBlock) {
                throw unsupported(module, "Trait init blocks are not supported by the Java backend in v1");
            }
        }
        var code = new StringBuilder();
        appendDocComments(code, declaration.comments(), 0);
        code.append("public interface ").append(declaration.name());
        if (!declaration.parents().isEmpty()) {
            code.append(" extends ")
                    .append(declaration.parents().stream().map(parent -> renderTypeReference(parent.name())).collect(Collectors.joining(", ")));
        }
        code.append(" {\n");
        if (usesThrowSupport(declaration)) {
            code.append(renderThrowHelper()).append("\n");
        }
        if (usesMatchSupport(declaration)) {
            code.append(renderMatchHelper()).append("\n");
        }
        code.append(renderReflectionMethod(module, declaration, definitionsByName)).append("\n");
        for (var member : declaration.members()) {
            code.append(renderTraitMethod(module, declaration.name(), (ObjectOriented.MethodDeclaration) member));
        }
        code.append("}\n");
        return code.toString();
    }

    private String renderReflectionMethod(
            ObjectOrientedModule module,
            ObjectOriented.TypeDeclaration declaration,
            Map<String, ObjectOriented.TypeDeclaration> definitionsByName
    ) {
        return "    public static " + reflectionInfoJavaType(declaration) + " type() {\n"
               + "        return " + renderObjectOrientedInfo(module, declaration, definitionsByName, true) + ";\n"
               + "    }\n";
    }

    private String reflectionInfoJavaType(ObjectOriented.TypeDeclaration declaration) {
        if (declaration instanceof ObjectOriented.InterfaceDeclaration) {
            return "capy.metaProg.Reflection.InterfaceInfo";
        }
        if (declaration instanceof ObjectOriented.TraitDeclaration) {
            return "capy.metaProg.Reflection.TraitInfo";
        }
        return "capy.metaProg.Reflection.ObjectInfo";
    }

    private String renderObjectOrientedInfo(
            ObjectOrientedModule module,
            ObjectOriented.TypeDeclaration declaration,
            Map<String, ObjectOriented.TypeDeclaration> definitionsByName,
            boolean full
    ) {
        if (declaration instanceof ObjectOriented.InterfaceDeclaration) {
            return "new capy.metaProg.Reflection.InterfaceInfo("
                   + javaString(declaration.name()) + ", "
                   + renderReflectionPackage(module) + ", "
                   + renderReflectionMethods(module, declaration.members(), definitionsByName, full) + ", "
                   + renderReflectionParents(module, declaration.parents(), definitionsByName, full)
                   + ")";
        }
        if (declaration instanceof ObjectOriented.TraitDeclaration) {
            return "new capy.metaProg.Reflection.TraitInfo("
                   + javaString(declaration.name()) + ", "
                   + renderReflectionPackage(module) + ", "
                   + renderReflectionMethods(module, declaration.members(), definitionsByName, full) + ", "
                   + renderReflectionParents(module, declaration.parents(), definitionsByName, full)
                   + ")";
        }
        var classDeclaration = (ObjectOriented.ClassDeclaration) declaration;
        var fields = declaration.members().stream()
                .filter(ObjectOriented.FieldDeclaration.class::isInstance)
                .map(ObjectOriented.FieldDeclaration.class::cast)
                .toList();
        return "new capy.metaProg.Reflection.ObjectInfo("
               + javaString(declaration.name()) + ", "
               + renderReflectionPackage(module) + ", "
               + classDeclaration.modifiers().contains("open") + ", "
               + renderReflectionFields(module, fields, definitionsByName, full) + ", "
               + renderReflectionMethods(module, declaration.members(), definitionsByName, full) + ", "
               + renderReflectionParents(module, declaration.parents(), definitionsByName, full)
               + ")";
    }

    private String renderReflectionParents(
            ObjectOrientedModule module,
            List<ObjectOriented.TypeReference> parents,
            Map<String, ObjectOriented.TypeDeclaration> definitionsByName,
            boolean full
    ) {
        if (!full || parents.isEmpty()) {
            return "java.util.Set.<capy.metaProg.Reflection.AnyInfo>of()";
        }
        return parents.stream()
                .map(parent -> {
                    var declaration = definitionsByName.get(simpleTypeName(parent.name()));
                    if (declaration == null) {
                        return renderExternalObjectInfo(module, simpleTypeName(parent.name()));
                    }
                    return renderObjectOrientedInfo(module, declaration, definitionsByName, false);
                })
                .collect(Collectors.joining(", ", "java.util.Set.<capy.metaProg.Reflection.AnyInfo>of(", ")"));
    }

    private String renderReflectionFields(
            ObjectOrientedModule module,
            List<ObjectOriented.FieldDeclaration> fields,
            Map<String, ObjectOriented.TypeDeclaration> definitionsByName,
            boolean full
    ) {
        if (!full || fields.isEmpty()) {
            return "java.util.List.<capy.metaProg.Reflection.FieldInfo>of()";
        }
        return fields.stream()
                .map(field -> "new capy.metaProg.Reflection.FieldInfo("
                              + javaString(field.name()) + ", "
                              + renderReflectionTypeInfo(module, field.type(), definitionsByName)
                              + ")")
                .collect(Collectors.joining(", ", "java.util.List.<capy.metaProg.Reflection.FieldInfo>of(", ")"));
    }

    private String renderReflectionMethods(
            ObjectOrientedModule module,
            List<ObjectOriented.MemberDeclaration> members,
            Map<String, ObjectOriented.TypeDeclaration> definitionsByName,
            boolean full
    ) {
        if (!full) {
            return "java.util.List.<capy.metaProg.Reflection.MethodInfo>of()";
        }
        var methods = members.stream()
                .filter(ObjectOriented.MethodDeclaration.class::isInstance)
                .map(ObjectOriented.MethodDeclaration.class::cast)
                .toList();
        if (methods.isEmpty()) {
            return "java.util.List.<capy.metaProg.Reflection.MethodInfo>of()";
        }
        return methods.stream()
                .map(method -> renderReflectionMethodInfo(module, method, definitionsByName))
                .collect(Collectors.joining(", ", "java.util.List.<capy.metaProg.Reflection.MethodInfo>of(", ")"));
    }

    private String renderReflectionMethodInfo(
            ObjectOrientedModule module,
            ObjectOriented.MethodDeclaration method,
            Map<String, ObjectOriented.TypeDeclaration> definitionsByName
    ) {
        return "new capy.metaProg.Reflection.MethodInfo("
               + javaString(method.name()) + ", "
               + renderReflectionPackage(module) + ", "
               + renderReflectionParams(module, method.parameters(), definitionsByName) + ", "
               + renderReflectionTypeInfo(module, method.returnType(), definitionsByName)
               + ")";
    }

    private String renderReflectionParams(
            ObjectOrientedModule module,
            List<ObjectOriented.Parameter> parameters,
            Map<String, ObjectOriented.TypeDeclaration> definitionsByName
    ) {
        if (parameters.isEmpty()) {
            return "java.util.List.<capy.metaProg.Reflection.FieldInfo>of()";
        }
        return parameters.stream()
                .map(parameter -> "new capy.metaProg.Reflection.FieldInfo("
                                  + javaString(parameter.name()) + ", "
                                  + renderReflectionTypeInfo(module, parameter.type(), definitionsByName)
                                  + ")")
                .collect(Collectors.joining(", ", "java.util.List.<capy.metaProg.Reflection.FieldInfo>of(", ")"));
    }

    private String renderReflectionTypeInfo(
            ObjectOrientedModule module,
            String rawType,
            Map<String, ObjectOriented.TypeDeclaration> definitionsByName
    ) {
        var trimmed = rawType.trim();
        if (trimmed.endsWith("[]")) {
            return "new capy.metaProg.Reflection.ListInfo("
                   + javaString("array") + ", "
                   + renderEmptyReflectionPackage() + ", "
                   + renderReflectionTypeInfo(module, trimmed.substring(0, trimmed.length() - 2), definitionsByName)
                   + ")";
        }
        if (trimmed.startsWith("list[") && trimmed.endsWith("]")) {
            return "new capy.metaProg.Reflection.ListInfo("
                   + javaString("list") + ", "
                   + renderEmptyReflectionPackage() + ", "
                   + renderReflectionTypeInfo(module, trimmed.substring(5, trimmed.length() - 1), definitionsByName)
                   + ")";
        }
        if (trimmed.startsWith("set[") && trimmed.endsWith("]")) {
            return "new capy.metaProg.Reflection.SetInfo("
                   + javaString("set") + ", "
                   + renderEmptyReflectionPackage() + ", "
                   + renderReflectionTypeInfo(module, trimmed.substring(4, trimmed.length() - 1), definitionsByName)
                   + ")";
        }
        if (trimmed.startsWith("dict[") && trimmed.endsWith("]")) {
            return "new capy.metaProg.Reflection.DictInfo("
                   + javaString("dict") + ", "
                   + renderEmptyReflectionPackage() + ", "
                   + renderReflectionTypeInfo(module, trimmed.substring(5, trimmed.length() - 1), definitionsByName)
                   + ")";
        }
        if (trimmed.startsWith("tuple[") && trimmed.endsWith("]")) {
            var inner = trimmed.substring(6, trimmed.length() - 1);
            var elements = splitTopLevel(inner).stream()
                    .map(type -> renderReflectionTypeInfo(module, type, definitionsByName))
                    .collect(Collectors.joining(", ", "java.util.List.<capy.metaProg.Reflection.AnyInfo>of(", ")"));
            return "new capy.metaProg.Reflection.TupleInfo("
                   + javaString("tuple") + ", "
                   + renderEmptyReflectionPackage() + ", "
                   + elements
                   + ")";
        }
        if (isPrimitiveReflectionType(trimmed)) {
            return "new capy.metaProg.Reflection.DataInfo("
                   + javaString(trimmed) + ", "
                   + renderEmptyReflectionPackage()
                   + ")";
        }
        var declaration = definitionsByName.get(simpleTypeName(trimmed));
        if (declaration != null) {
            return renderObjectOrientedInfo(module, declaration, definitionsByName, false);
        }
        return "new capy.metaProg.Reflection.DataInfo("
               + javaString(simpleTypeName(trimmed)) + ", "
               + renderReflectionPackageForType(module, trimmed)
               + ")";
    }

    private boolean isPrimitiveReflectionType(String type) {
        return switch (type) {
            case "byte", "int", "long", "double", "float", "bool", "string", "any", "data", "void", "nothing" -> true;
            default -> false;
        };
    }

    private String renderExternalObjectInfo(ObjectOrientedModule module, String name) {
        return "new capy.metaProg.Reflection.ObjectInfo("
               + javaString(name) + ", "
               + renderReflectionPackageForType(module, name) + ", "
               + "false, "
               + "java.util.List.<capy.metaProg.Reflection.FieldInfo>of(), "
               + "java.util.List.<capy.metaProg.Reflection.MethodInfo>of(), "
               + "java.util.Set.<capy.metaProg.Reflection.AnyInfo>of()"
               + ")";
    }

    private String renderReflectionPackage(ObjectOrientedModule module) {
        var path = module.path().replaceFirst("^/", "");
        var name = path.isBlank() ? "" : simpleTypeName(path);
        return "new capy.metaProg.Reflection.PackageInfo(" + javaString(name) + ", " + javaString(path) + ")";
    }

    private String renderReflectionPackageForType(ObjectOrientedModule module, String rawType) {
        var normalized = rawType.replace('\\', '/');
        if (normalized.startsWith("/")) {
            var slash = normalized.lastIndexOf('/');
            var path = slash > 0 ? normalized.substring(1, slash) : "";
            var name = path.isBlank() ? "" : simpleTypeName(path);
            return "new capy.metaProg.Reflection.PackageInfo(" + javaString(name) + ", " + javaString(path) + ")";
        }
        return renderReflectionPackage(module);
    }

    private String renderEmptyReflectionPackage() {
        return "new capy.metaProg.Reflection.PackageInfo(\"\", \"\")";
    }

    private String renderInterfaceMethod(ObjectOrientedModule module, String ownerName, ObjectOriented.MethodDeclaration method) {
        if (method.body().isPresent()) {
            throw unsupported(module, "Interface `" + ownerName + "` default methods are not supported in v1");
        }
        if (!"public".equals(method.visibility())) {
            throw unsupported(module, "Interface method `" + method.name() + "` must be public");
        }
        var code = new StringBuilder();
        appendDocComments(code, method.comments(), 1);
        code.append("    ")
                .append(renderType(method.returnType(), false))
                .append(" ")
                .append(sanitizeIdentifier(method.name()))
                .append("(")
                .append(renderParameters(method.parameters()))
                .append(");\n");
        return code.toString();
    }

    private String renderTraitMethod(ObjectOrientedModule module, String ownerName, ObjectOriented.MethodDeclaration method) {
        var code = new StringBuilder();
        appendDocComments(code, method.comments(), 1);
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
                            .append(renderExpression(module, expressionBody.expression(), parentNames, LocalMethodBindings.empty()))
                            .append(";\n");
                    return;
                }
                code.append(indent(indentLevel))
                        .append("return ")
                        .append(renderExpression(module, expressionBody.expression(), parentNames, LocalMethodBindings.empty()))
                        .append(";\n");
                return;
            }
            appendStatementBlockContents(code, module, (ObjectOriented.StatementBlock) body, indentLevel, parentNames, LocalMethodBindings.empty());
        });
    }

    private void appendStatementBlockContents(
            StringBuilder code,
            ObjectOrientedModule module,
            ObjectOriented.StatementBlock block,
            int indentLevel,
            Set<String> parentNames,
            LocalMethodBindings localMethodBindings
    ) {
        for (int index = 0; index < block.statements().size(); index++) {
            var statement = block.statements().get(index);
            if (statement instanceof ObjectOriented.LocalMethodStatement) {
                var groupedLocalMethods = new ArrayList<ObjectOriented.LocalMethodStatement>();
                while (index < block.statements().size() && block.statements().get(index) instanceof ObjectOriented.LocalMethodStatement localMethodStatement) {
                    groupedLocalMethods.add(localMethodStatement);
                    index++;
                }
                index--;
                localMethodBindings = appendLocalMethodGroup(code, module, groupedLocalMethods, indentLevel, parentNames, localMethodBindings);
                continue;
            }
            appendStatement(code, module, statement, indentLevel, parentNames, localMethodBindings);
        }
    }

    private LocalMethodBindings appendLocalMethodGroup(
            StringBuilder code,
            ObjectOrientedModule module,
            List<ObjectOriented.LocalMethodStatement> localMethods,
            int indentLevel,
            Set<String> parentNames,
            LocalMethodBindings outerBindings
    ) {
        var helperType = "__CapybaraLocalMethods" + syntheticCounter++;
        var helperVariable = "__capybaraLocalMethods" + syntheticCounter++;
        code.append(indent(indentLevel)).append("final class ").append(helperType).append(" {\n");
        for (int index = 0; index < localMethods.size(); index++) {
            appendLocalMethod(code, module, localMethods.get(index), indentLevel + 1, parentNames, outerBindings);
            if (index < localMethods.size() - 1) {
                code.append("\n");
            }
        }
        code.append(indent(indentLevel)).append("}\n");
        code.append(indent(indentLevel)).append("final var ").append(helperVariable).append(" = new ").append(helperType).append("();\n");
        return outerBindings.withGroup(localMethods, helperVariable);
    }

    private void appendLocalMethod(
            StringBuilder code,
            ObjectOrientedModule module,
            ObjectOriented.LocalMethodStatement method,
            int indentLevel,
            Set<String> parentNames,
            LocalMethodBindings localMethodBindings
    ) {
        appendDocComments(code, method.comments(), indentLevel);
        code.append(indent(indentLevel))
                .append(renderType(method.returnType(), false))
                .append(' ')
                .append(sanitizeIdentifier(method.name()))
                .append("(")
                .append(method.parameters().stream()
                        .map(parameter -> renderType(parameter.type(), false) + " " + sanitizeIdentifier(parameter.name()))
                        .collect(Collectors.joining(", ")))
                .append(")");
        if (method.body() instanceof ObjectOriented.ExpressionBody expressionBody) {
            code.append(" {\n")
                    .append(indent(indentLevel + 1));
            if ("void".equals(renderType(method.returnType(), false))) {
                code.append(renderExpression(module, expressionBody.expression(), parentNames, localMethodBindings)).append(";\n");
            } else {
                code.append("return ")
                        .append(renderExpression(module, expressionBody.expression(), parentNames, localMethodBindings))
                        .append(";\n");
            }
            code.append(indent(indentLevel)).append("}\n");
            return;
        }
        code.append(" {\n");
        appendStatementBlockContents(code, module, (ObjectOriented.StatementBlock) method.body(), indentLevel + 1, parentNames, localMethodBindings);
        code.append(indent(indentLevel)).append("}\n");
    }

    private void appendStatement(
            StringBuilder code,
            ObjectOrientedModule module,
            ObjectOriented.Statement statement,
            int indentLevel,
            Set<String> parentNames,
            LocalMethodBindings localMethodBindings
    ) {
        switch (statement) {
            case ObjectOriented.LetStatement letStatement -> code.append(indent(indentLevel))
                    .append("final ")
                    .append(letStatement.type().map(type -> renderType(type, false)).orElse("var"))
                    .append(' ')
                    .append(sanitizeIdentifier(letStatement.name()))
                    .append(" = ")
                    .append(renderExpression(module, letStatement.expression(), parentNames, localMethodBindings))
                    .append(";\n");
            case ObjectOriented.LocalMethodStatement ignored -> {
            }
            case ObjectOriented.MutableVariableStatement mutableVariableStatement -> code.append(indent(indentLevel))
                    .append(mutableVariableStatement.type().map(type -> renderType(type, false)).orElse("var"))
                    .append(' ')
                    .append(sanitizeIdentifier(mutableVariableStatement.name()))
                    .append(" = ")
                    .append(renderExpression(module, mutableVariableStatement.expression(), parentNames, localMethodBindings))
                    .append(";\n");
            case ObjectOriented.AssignmentStatement assignmentStatement -> code.append(indent(indentLevel))
                    .append(sanitizeIdentifier(assignmentStatement.name()))
                    .append(" = ")
                    .append(renderExpression(module, assignmentStatement.expression(), parentNames, localMethodBindings))
                    .append(";\n");
            case ObjectOriented.ExpressionStatement expressionStatement -> code.append(indent(indentLevel))
                    .append(renderExpression(module, expressionStatement.expression(), parentNames, localMethodBindings))
                    .append(";\n");
            case ObjectOriented.ThrowStatement throwStatement -> code.append(indent(indentLevel))
                    .append("throw capybara$toException(")
                    .append(renderExpression(module, throwStatement.expression(), parentNames, localMethodBindings))
                    .append(");\n");
            case ObjectOriented.ReturnStatement returnStatement -> code.append(indent(indentLevel))
                    .append("return ")
                    .append(renderExpression(module, returnStatement.expression(), parentNames, localMethodBindings))
                    .append(";\n");
            case ObjectOriented.IfStatement ifStatement -> {
                code.append(indent(indentLevel))
                        .append("if (")
                        .append(renderExpression(module, ifStatement.condition(), parentNames, localMethodBindings))
                        .append(") {\n");
                appendStatementBlockContents(code, module, ifStatement.thenBranch(), indentLevel + 1, parentNames, localMethodBindings);
                code.append(indent(indentLevel)).append('}');
                ifStatement.elseBranch().ifPresent(elseBranch -> {
                    if (elseBranch instanceof ObjectOriented.IfStatement nestedIf) {
                        code.append(" else ");
                        appendInlineIf(code, module, nestedIf, indentLevel, parentNames, localMethodBindings);
                    } else {
                        code.append(" else {\n");
                        appendStatementBlockContents(code, module, (ObjectOriented.StatementBlock) elseBranch, indentLevel + 1, parentNames, localMethodBindings);
                        code.append(indent(indentLevel)).append('}');
                    }
                });
                code.append("\n");
            }
            case ObjectOriented.TryCatchStatement tryCatchStatement -> {
                code.append(indent(indentLevel)).append("try {\n");
                appendStatementBlockContents(code, module, tryCatchStatement.tryBlock(), indentLevel + 1, parentNames, localMethodBindings);
                code.append(indent(indentLevel)).append("}");
                for (var catchClause : tryCatchStatement.catches()) {
                    code.append(" catch (java.lang.RuntimeException ")
                            .append(sanitizeIdentifier(catchClause.name()))
                            .append(") {\n");
                    appendStatementBlockContents(code, module, catchClause.body(), indentLevel + 1, parentNames, localMethodBindings);
                    code.append(indent(indentLevel)).append("}");
                }
                code.append("\n");
            }
            case ObjectOriented.WhileStatement whileStatement -> {
                code.append(indent(indentLevel))
                        .append("while (")
                        .append(renderExpression(module, whileStatement.condition(), parentNames, localMethodBindings))
                        .append(") {\n");
                appendStatementBlockContents(code, module, whileStatement.body(), indentLevel + 1, parentNames, localMethodBindings);
                code.append(indent(indentLevel)).append("}\n");
            }
            case ObjectOriented.DoWhileStatement doWhileStatement -> {
                code.append(indent(indentLevel)).append("do {\n");
                appendStatementBlockContents(code, module, doWhileStatement.body(), indentLevel + 1, parentNames, localMethodBindings);
                code.append(indent(indentLevel))
                        .append("} while (")
                        .append(renderExpression(module, doWhileStatement.condition(), parentNames, localMethodBindings))
                        .append(");\n");
            }
            case ObjectOriented.ForEachStatement forEachStatement -> {
                code.append(indent(indentLevel))
                        .append("for (")
                        .append(forEachStatement.type().map(type -> renderType(type, false)).orElse("var"))
                        .append(' ')
                        .append(sanitizeIdentifier(forEachStatement.name()))
                        .append(" : ")
                        .append(renderExpression(module, forEachStatement.iterable(), parentNames, localMethodBindings))
                        .append(") {\n");
                appendStatementBlockContents(code, module, forEachStatement.body(), indentLevel + 1, parentNames, localMethodBindings);
                code.append(indent(indentLevel)).append("}\n");
            }
            case ObjectOriented.StatementBlock nestedBlock -> {
                code.append(indent(indentLevel)).append("{\n");
                appendStatementBlockContents(code, module, nestedBlock, indentLevel + 1, parentNames, localMethodBindings);
                code.append(indent(indentLevel)).append("}\n");
            }
        }
    }

    private void appendInlineIf(
            StringBuilder code,
            ObjectOrientedModule module,
            ObjectOriented.IfStatement statement,
            int indentLevel,
            Set<String> parentNames,
            LocalMethodBindings localMethodBindings
    ) {
        code.append("if (")
                .append(renderExpression(module, statement.condition(), parentNames, localMethodBindings))
                .append(") {\n");
        appendStatementBlockContents(code, module, statement.thenBranch(), indentLevel + 1, parentNames, localMethodBindings);
        code.append(indent(indentLevel)).append('}');
        statement.elseBranch().ifPresent(elseBranch -> {
            if (elseBranch instanceof ObjectOriented.IfStatement nestedIf) {
                code.append(" else ");
                appendInlineIf(code, module, nestedIf, indentLevel, parentNames, localMethodBindings);
            } else {
                code.append(" else {\n");
                appendStatementBlockContents(code, module, (ObjectOriented.StatementBlock) elseBranch, indentLevel + 1, parentNames, localMethodBindings);
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
        var fields = owner.members().stream()
                .filter(ObjectOriented.FieldDeclaration.class::isInstance)
                .map(ObjectOriented.FieldDeclaration.class::cast)
                .toList();
        var initBlocks = owner.members().stream()
                .filter(ObjectOriented.InitBlock.class::isInstance)
                .map(ObjectOriented.InitBlock.class::cast)
                .toList();
        if (requiresConstructor(owner, fields, initBlocks)) {
            throw unsupported(module, "Entrypoint class `" + owner.name() + "` cannot declare constructor state or init blocks");
        }
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
                case ObjectOriented.LocalMethodStatement localMethodStatement -> {
                    switch (localMethodStatement.body()) {
                        case ObjectOriented.ExpressionBody expressionBody -> expressions.add(expressionBody.expression());
                        case ObjectOriented.StatementBlock statementBlock -> collectExpressions(statementBlock, expressions);
                    }
                }
                case ObjectOriented.MutableVariableStatement mutableVariableStatement -> expressions.add(mutableVariableStatement.expression());
                case ObjectOriented.AssignmentStatement assignmentStatement -> expressions.add(assignmentStatement.expression());
                case ObjectOriented.ExpressionStatement expressionStatement -> expressions.add(expressionStatement.expression());
                case ObjectOriented.ThrowStatement throwStatement -> expressions.add(throwStatement.expression());
                case ObjectOriented.ReturnStatement returnStatement -> expressions.add(returnStatement.expression());
                case ObjectOriented.IfStatement ifStatement -> {
                    expressions.add(ifStatement.condition());
                    collectExpressions(ifStatement.thenBranch(), expressions);
                    ifStatement.elseBranch().ifPresent(elseBranch -> collectExpressions(elseBranch, expressions));
                }
                case ObjectOriented.TryCatchStatement tryCatchStatement -> {
                    collectExpressions(tryCatchStatement.tryBlock(), expressions);
                    tryCatchStatement.catches().forEach(catchClause -> collectExpressions(catchClause.body(), expressions));
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
            case ObjectOriented.TryCatchStatement tryCatchStatement -> {
                collectExpressions(tryCatchStatement.tryBlock(), expressions);
                tryCatchStatement.catches().forEach(catchClause -> collectExpressions(catchClause.body(), expressions));
            }
            case ObjectOriented.LetStatement letStatement -> expressions.add(letStatement.expression());
            case ObjectOriented.LocalMethodStatement localMethodStatement -> {
                switch (localMethodStatement.body()) {
                    case ObjectOriented.ExpressionBody expressionBody -> expressions.add(expressionBody.expression());
                    case ObjectOriented.StatementBlock statementBlock -> collectExpressions(statementBlock, expressions);
                }
            }
            case ObjectOriented.MutableVariableStatement mutableVariableStatement -> expressions.add(mutableVariableStatement.expression());
            case ObjectOriented.AssignmentStatement assignmentStatement -> expressions.add(assignmentStatement.expression());
            case ObjectOriented.ExpressionStatement expressionStatement -> expressions.add(expressionStatement.expression());
            case ObjectOriented.ThrowStatement throwStatement -> expressions.add(throwStatement.expression());
            case ObjectOriented.ReturnStatement returnStatement -> expressions.add(returnStatement.expression());
        }
    }

    private String renderJavaEntrypointWrapper() {
        return "    public static void main(java.lang.String[] args) {\n"
               + "        System.exit(main(java.util.List.of(args)));\n"
               + "    }\n";
    }

    private boolean usesThrowSupport(ObjectOriented.TypeDeclaration declaration) {
        return declaration.members().stream().anyMatch(this::containsThrow);
    }

    private boolean containsThrow(ObjectOriented.MemberDeclaration member) {
        return switch (member) {
            case ObjectOriented.FieldDeclaration ignored -> false;
            case ObjectOriented.MethodDeclaration method -> method.body().stream().anyMatch(this::containsThrow);
            case ObjectOriented.InitBlock initBlock -> containsThrow(initBlock.body());
        };
    }

    private boolean containsThrow(ObjectOriented.MethodBody methodBody) {
        return switch (methodBody) {
            case ObjectOriented.ExpressionBody ignored -> false;
            case ObjectOriented.StatementBlock statementBlock -> containsThrow(statementBlock);
        };
    }

    private boolean containsThrow(ObjectOriented.StatementBlock block) {
        return block.statements().stream().anyMatch(this::containsThrow);
    }

    private boolean containsThrow(ObjectOriented.Statement statement) {
        return switch (statement) {
            case ObjectOriented.ThrowStatement ignored -> true;
            case ObjectOriented.LocalMethodStatement localMethodStatement -> switch (localMethodStatement.body()) {
                case ObjectOriented.ExpressionBody ignored -> false;
                case ObjectOriented.StatementBlock statementBlock -> containsThrow(statementBlock);
            };
            case ObjectOriented.IfStatement ifStatement ->
                    containsThrow(ifStatement.thenBranch()) || ifStatement.elseBranch().stream().anyMatch(this::containsThrow);
            case ObjectOriented.TryCatchStatement tryCatchStatement ->
                    containsThrow(tryCatchStatement.tryBlock())
                    || tryCatchStatement.catches().stream().anyMatch(catchClause -> containsThrow(catchClause.body()));
            case ObjectOriented.WhileStatement whileStatement -> containsThrow(whileStatement.body());
            case ObjectOriented.DoWhileStatement doWhileStatement -> containsThrow(doWhileStatement.body());
            case ObjectOriented.ForEachStatement forEachStatement -> containsThrow(forEachStatement.body());
            case ObjectOriented.StatementBlock nestedBlock -> containsThrow(nestedBlock);
            case ObjectOriented.LetStatement letStatement -> false;
            case ObjectOriented.MutableVariableStatement mutableVariableStatement -> false;
            case ObjectOriented.AssignmentStatement assignmentStatement -> false;
            case ObjectOriented.ExpressionStatement expressionStatement -> false;
            case ObjectOriented.ReturnStatement returnStatement -> false;
        };
    }

    private String renderThrowHelper() {
        return "    private static java.lang.RuntimeException capybara$toException(java.lang.Object value) {\n"
               + "        if (value instanceof java.lang.RuntimeException runtimeException) {\n"
               + "            return runtimeException;\n"
               + "        }\n"
               + "        if (value instanceof java.lang.Throwable throwable) {\n"
               + "            return new java.lang.RuntimeException(throwable.getMessage(), throwable);\n"
               + "        }\n"
               + "        return new java.lang.RuntimeException(java.lang.String.valueOf(value));\n"
               + "    }\n";
    }

    private boolean usesMatchSupport(ObjectOriented.TypeDeclaration declaration) {
        return declaration.members().stream().anyMatch(this::containsMatch);
    }

    private boolean containsMatch(ObjectOriented.MemberDeclaration member) {
        return switch (member) {
            case ObjectOriented.FieldDeclaration field -> field.initializer().stream().anyMatch(this::containsMatch);
            case ObjectOriented.MethodDeclaration method -> method.body().stream().anyMatch(this::containsMatch);
            case ObjectOriented.InitBlock initBlock -> containsMatch(initBlock.body());
        };
    }

    private boolean containsMatch(ObjectOriented.MethodBody methodBody) {
        return switch (methodBody) {
            case ObjectOriented.ExpressionBody expressionBody -> containsMatch(expressionBody.expression());
            case ObjectOriented.StatementBlock statementBlock -> containsMatch(statementBlock);
        };
    }

    private boolean containsMatch(ObjectOriented.StatementBlock block) {
        return collectExpressions(block).stream().anyMatch(this::containsMatch);
    }

    private boolean containsMatch(String expression) {
        return expression.trim().startsWith("match");
    }

    private String renderMatchHelper() {
        return "    private static <T, R> R capybara$match(T value, java.util.function.Function<T, R> matcher) {\n"
               + "        return matcher.apply(value);\n"
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

    private String renderExpression(ObjectOrientedModule module, String expression, Set<String> parentNames, LocalMethodBindings localMethodBindings) {
        var trimmed = rewriteQualifiedTypeReferences(expression.trim());
        if (trimmed.startsWith("match")) {
            return renderMatchExpression(module, trimmed, parentNames, localMethodBindings);
        }
        var arrayWithValues = renderArrayWithValues(module, trimmed, parentNames, localMethodBindings);
        if (arrayWithValues.isPresent()) {
            return arrayWithValues.orElseThrow();
        }
        var dataCreation = renderDataCreation(module, trimmed, parentNames, localMethodBindings);
        if (dataCreation.isPresent()) {
            return dataCreation.orElseThrow();
        }
        var sizedArray = renderSizedArray(module, trimmed, parentNames, localMethodBindings);
        if (sizedArray.isPresent()) {
            return sizedArray.orElseThrow();
        }
        if (trimmed.contains("=>") || trimmed.contains("|>") || trimmed.contains("|-") || trimmed.contains("|*")
            || trimmed.contains(".and.") || trimmed.contains(".or.") || trimmed.contains(".xor.") || trimmed.contains(".nand.")
            || trimmed.startsWith("[") || trimmed.startsWith("{")) {
            throw unsupported(module, "Expression `" + preview(trimmed) + "` is not supported by the Java backend in v1");
        }
        if (trimmed.startsWith("if ")) {
            return renderIfExpression(module, trimmed, parentNames, localMethodBindings);
        }
        if ("???".equals(trimmed)) {
            return "null";
        }
        for (var parentName : parentNames) {
            trimmed = trimmed.replaceAll("(^|[^A-Za-z0-9_])" + Pattern.quote(parentName) + "\\s*\\.", "$1super.");
        }
        trimmed = rewriteLocalMethodCalls(trimmed, localMethodBindings);
        trimmed = rewriteImportedFunctionCalls(module, trimmed);
        return trimmed;
    }

    private String rewriteImportedFunctionCalls(ObjectOrientedModule module, String expression) {
        if (module == null || module.imports().isEmpty()) {
            return expression;
        }
        var rewritten = expression;
        for (var entry : importedFunctionOwners(module).entrySet().stream()
                .sorted((left, right) -> Integer.compare(right.getKey().length(), left.getKey().length()))
                .toList()) {
            rewritten = rewritten.replaceAll(
                    "(^|[^A-Za-z0-9_\\.])" + Pattern.quote(entry.getKey()) + "\\s*\\(",
                    "$1" + Matcher.quoteReplacement(entry.getValue() + "." + entry.getKey()) + "("
            );
        }
        return rewritten;
    }

    private Map<String, String> importedFunctionOwners(ObjectOrientedModule module) {
        var owners = new LinkedHashMap<String, String>();
        for (var importDeclaration : module.imports()) {
            var ownerReference = renderImportedOwnerReference(module, importDeclaration.moduleName());
            if (importDeclaration.isStarImport() && "/capy/io/Stdout".equals(importDeclaration.moduleName())) {
                owners.put("print", ownerReference);
                owners.put("println", ownerReference);
            }
            for (var symbol : importDeclaration.symbols()) {
                if ("*".equals(symbol) || symbol.isBlank() || Character.isUpperCase(symbol.charAt(0))) {
                    continue;
                }
                if (importDeclaration.excludedSymbols().contains(symbol)) {
                    continue;
                }
                owners.put(symbol, ownerReference);
            }
        }
        return owners;
    }

    private String rewriteQualifiedTypeReferences(String expression) {
        var matcher = Pattern.compile("(?<![A-Za-z0-9_])/(?:[A-Za-z_][A-Za-z0-9_]*/)+[A-Za-z_][A-Za-z0-9_]*").matcher(expression);
        var rewritten = new StringBuilder();
        while (matcher.find()) {
            matcher.appendReplacement(rewritten, Matcher.quoteReplacement(renderTypeReference(matcher.group())));
        }
        matcher.appendTail(rewritten);
        return rewritten.toString();
    }

    private Optional<String> renderDataCreation(ObjectOrientedModule module, String expression, Set<String> parentNames, LocalMethodBindings localMethodBindings) {
        if (!expression.endsWith("}")) {
            return Optional.empty();
        }
        var braceIndex = findTopLevelChar(expression, '{');
        if (braceIndex < 0) {
            return Optional.empty();
        }
        var type = expression.substring(0, braceIndex).trim();
        if (type.isBlank() || type.endsWith("[]") || !isTypeLikePrefix(type)) {
            return Optional.empty();
        }
        var body = expression.substring(braceIndex + 1, expression.length() - 1).trim();
        var arguments = splitTopLevel(body).stream()
                .map(assignment -> renderDataCreationArgument(module, assignment, parentNames, localMethodBindings))
                .collect(Collectors.joining(", "));
        return Optional.of("new " + renderType(type, false) + "(" + arguments + ")");
    }

    private String renderDataCreationArgument(ObjectOrientedModule module, String assignment, Set<String> parentNames, LocalMethodBindings localMethodBindings) {
        var colonIndex = findTopLevelChar(assignment, ':');
        if (colonIndex < 0) {
            return renderExpression(module, assignment, parentNames, localMethodBindings);
        }
        return renderExpression(module, assignment.substring(colonIndex + 1).trim(), parentNames, localMethodBindings);
    }

    private String renderMatchExpression(ObjectOrientedModule module, String expression, Set<String> parentNames, LocalMethodBindings localMethodBindings) {
        var withIndex = findTopLevelToken(expression, "with", "match".length());
        if (withIndex < 0) {
            throw unsupported(module, "Unable to lower `match` expression `" + preview(expression) + "`");
        }
        var matchedExpression = expression.substring("match".length(), withIndex).trim();
        var casesSource = expression.substring(withIndex + "with".length()).trim();
        var matchCases = splitMatchCases(casesSource);
        if (matchCases.isEmpty()) {
            throw unsupported(module, "Match expression `" + preview(expression) + "` must contain at least one case");
        }
        var matchVar = "__capybaraMatch" + syntheticCounter++;
        var code = new StringBuilder();
        code.append("capybara$match(")
                .append(renderExpression(module, matchedExpression, parentNames, localMethodBindings))
                .append(", ")
                .append(matchVar)
                .append(" -> switch (")
                .append(matchVar)
                .append(") {\n");
        for (var matchCase : matchCases) {
            code.append(renderMatchCase(module, matchCase, matchVar, parentNames, localMethodBindings));
        }
        code.append("        default -> throw new java.lang.IllegalStateException(\"Non-exhaustive match: \" + ").append(matchVar).append(");\n")
                .append("    })");
        return code.toString();
    }

    private String renderMatchCase(ObjectOrientedModule module, String matchCase, String matchVar, Set<String> parentNames, LocalMethodBindings localMethodBindings) {
        if (!matchCase.startsWith("case")) {
            throw unsupported(module, "Unsupported match branch `" + preview(matchCase) + "`");
        }
        var arrowIndex = findTopLevelToken(matchCase, "->", "case".length());
        if (arrowIndex < 0) {
            throw unsupported(module, "Unsupported match branch `" + preview(matchCase) + "`");
        }
        var pattern = matchCase.substring("case".length(), arrowIndex).trim();
        var body = matchCase.substring(arrowIndex + "->".length()).trim();
        var braceIndex = findTopLevelChar(pattern, '{');
        var code = new StringBuilder();
        if (braceIndex < 0) {
            var typeName = pattern;
            code.append("        case ").append(renderTypeReference(typeName)).append(" ignored -> ")
                    .append(renderExpression(module, body, parentNames, localMethodBindings))
                    .append(";\n");
            return code.toString();
        }

        var typeName = pattern.substring(0, braceIndex).trim();
        var bindingsSource = pattern.substring(braceIndex + 1, pattern.length() - 1).trim();
        var boundValue = "__capybaraCase" + syntheticCounter++;
        code.append("        case ").append(renderTypeReference(typeName)).append(" ").append(boundValue).append(" -> {\n");
        for (var binding : splitTopLevel(bindingsSource)) {
            var bindingName = binding.trim();
            if (bindingName.isBlank()) {
                continue;
            }
            code.append("        var ")
                    .append(sanitizeIdentifier(bindingName))
                    .append(" = ")
                    .append(boundValue)
                    .append(".")
                    .append(sanitizeIdentifier(bindingName))
                    .append("();\n");
        }
        code.append("            yield ").append(renderExpression(module, body, parentNames, localMethodBindings)).append(";\n")
                .append("        }\n");
        return code.toString();
    }

    private List<String> splitMatchCases(String casesSource) {
        var normalized = casesSource.replace("\r", "");
        var indexes = new ArrayList<Integer>();
        for (int i = 0; i < normalized.length(); i++) {
            if (startsWithTopLevelToken(normalized, i, "case")) {
                indexes.add(i);
            }
        }
        if (indexes.isEmpty()) {
            return List.of();
        }
        var cases = new ArrayList<String>();
        for (int i = 0; i < indexes.size(); i++) {
            var start = indexes.get(i);
            var end = i + 1 < indexes.size() ? indexes.get(i + 1) : normalized.length();
            cases.add(normalized.substring(start, end).trim());
        }
        return List.copyOf(cases);
    }

    private int findTopLevelToken(String value, String token, int startAt) {
        var parens = 0;
        var brackets = 0;
        var braces = 0;
        char stringDelimiter = 0;
        for (int i = startAt; i <= value.length() - token.length(); i++) {
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
            if (parens == 0 && brackets == 0 && braces == 0 && value.startsWith(token, i)) {
                return i;
            }
        }
        return -1;
    }

    private boolean startsWithTopLevelToken(String value, int index, String token) {
        if (!value.startsWith(token, index)) {
            return false;
        }
        var parens = 0;
        var brackets = 0;
        var braces = 0;
        char stringDelimiter = 0;
        for (int i = 0; i < index; i++) {
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
        }
        return parens == 0 && brackets == 0 && braces == 0;
    }

    private Optional<String> renderArrayWithValues(ObjectOrientedModule module, String expression, Set<String> parentNames, LocalMethodBindings localMethodBindings) {
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
                .map(value -> renderExpression(module, value, parentNames, localMethodBindings))
                .collect(Collectors.joining(", "));
        return Optional.of("new " + renderType(type, false) + "{" + values + "}");
    }

    private Optional<String> renderSizedArray(ObjectOrientedModule module, String expression, Set<String> parentNames, LocalMethodBindings localMethodBindings) {
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
        return Optional.of("new " + renderType(type, false) + "[" + renderExpression(module, sizeExpression, parentNames, localMethodBindings) + "]");
    }

    private String renderIfExpression(ObjectOrientedModule module, String expression, Set<String> parentNames, LocalMethodBindings localMethodBindings) {
        var thenIndex = findTopLevelKeyword(expression, " then ");
        var elseIndex = findTopLevelKeyword(expression, " else ");
        if (thenIndex < 0 || elseIndex < 0 || elseIndex <= thenIndex) {
            throw unsupported(module, "Unable to lower `if` expression `" + preview(expression) + "`");
        }
        var condition = expression.substring("if ".length(), thenIndex).trim();
        var thenBranch = expression.substring(thenIndex + " then ".length(), elseIndex).trim();
        var elseBranch = expression.substring(elseIndex + " else ".length()).trim();
        return "((" + renderExpression(module, condition, parentNames, localMethodBindings) + ") ? (" + renderExpression(module, thenBranch, parentNames, localMethodBindings) + ") : (" + renderExpression(module, elseBranch, parentNames, localMethodBindings) + "))";
    }

    private String rewriteLocalMethodCalls(String expression, LocalMethodBindings localMethodBindings) {
        var rewritten = expression;
        for (var entry : localMethodBindings.replacements().entrySet().stream()
                .sorted((left, right) -> Integer.compare(right.getKey().length(), left.getKey().length()))
                .toList()) {
            rewritten = rewritten.replaceAll(
                    "(^|[^A-Za-z0-9_\\.])" + Pattern.quote(entry.getKey()) + "\\s*\\(",
                    "$1" + entry.getValue() + "." + entry.getKey() + "("
            );
        }
        return rewritten;
    }

    private record LocalMethodBindings(Map<String, String> replacements) {
        static LocalMethodBindings empty() {
            return new LocalMethodBindings(Map.of());
        }

        LocalMethodBindings withGroup(List<ObjectOriented.LocalMethodStatement> localMethods, String helperVariable) {
            var updated = new LinkedHashMap<>(replacements);
            localMethods.forEach(localMethod -> updated.put(localMethod.name(), helperVariable));
            return new LocalMethodBindings(Map.copyOf(updated));
        }
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

    private String javaString(String value) {
        return "\"" + value
                .replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\n", "\\n")
                .replace("\r", "\\r")
                .replace("\t", "\\t")
                + "\"";
    }

    private String indent(int level) {
        return "    ".repeat(level);
    }

    private void appendDocComments(StringBuilder code, List<String> comments, int indentLevel) {
        if (comments.isEmpty()) {
            return;
        }
        var indent = indent(indentLevel);
        comments.forEach(line -> code.append(indent).append(line.isEmpty() ? "///" : "/// " + line).append("\n"));
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
