package dev.capylang.generator;

import dev.capylang.generator.java.*;
import dev.capylang.compiler.CompiledModule;
import dev.capylang.compiler.CompiledProgram;

import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Supplier;
import java.util.logging.Logger;

import static java.lang.String.join;
import static java.util.stream.Collectors.joining;
import static dev.capylang.generator.java.JavaExpressionEvaluator.evaluateExpression;

public final class JavaGenerator implements Generator {
    private static final Logger log = Logger.getLogger(JavaGenerator.class.getName());
    private final ObjectOrientedJavaGenerator objectOrientedJavaGenerator = new ObjectOrientedJavaGenerator();
    private static final String METHOD_DECL_PREFIX = "__method__";

    @Override
    public GeneratedProgram generate(CompiledProgram program) {
        var timings = new GenerationTimings();
        var functionNameOverrides = buildFunctionNameOverrides(program);
        var astBuilder = new JavaAstBuilder(functionNameOverrides);
        JavaExpressionEvaluator.setFunctionNameOverrides(functionNameOverrides);
        var modules = program.modules().stream()
                .map(module -> modules(module, timings, astBuilder))
                .flatMap(List::stream)
                .collect(java.util.stream.Collectors.toCollection(ArrayList::new));
        modules.addAll(time(timings::addSourceRenderNanos, () -> objectOrientedJavaGenerator.generate(program.objectOrientedModules())));
        log.info(() -> "Java generation timings: AST build="
                       + Duration.ofNanos(timings.astBuildNanos())
                       + ", Java source rendering="
                       + Duration.ofNanos(timings.sourceRenderNanos()));
        return new GeneratedProgram(modules);
    }

    private List<GeneratedModule> modules(CompiledModule module, GenerationTimings timings, JavaAstBuilder astBuilder) {
        var javaClass = time(timings::addAstBuildNanos, () -> astBuilder.build(module));
        if (!hasTypeOrDataNameConflictWithFile(javaClass)) {
            return List.of(new GeneratedModule(
                    relativePath(javaClass, javaClass.name().toString()),
                    time(timings::addSourceRenderNanos, () -> code(javaClass, javaClass.name().toString(), true))
            ));
        }

        var ownerInterface = javaClass.interfaces().stream()
                .filter(javaInterface -> javaInterface.name().toString().equals(javaClass.name().toString()))
                .findFirst();
        if (ownerInterface.isPresent()) {
            return List.of(new GeneratedModule(
                    relativePath(javaClass, javaClass.name().toString()),
                    time(timings::addSourceRenderNanos, () -> codeNestedInOwnerInterface(javaClass, ownerInterface.get(), ownerInterface.get().name().toString()))
            ));
        }

        var helperCallOwnerName = javaClass.name() + "Module";
        var compiled = new ArrayList<GeneratedModule>();
        for (var declaration : topLevelDeclarations(javaClass, helperCallOwnerName)) {
            compiled.add(new GeneratedModule(
                    relativePath(javaClass, declaration.name()),
                    time(timings::addSourceRenderNanos, () -> codeTopLevelDeclaration(javaClass, declaration.code()))
            ));
        }
            var utilityStaticImports = new TreeSet<>(javaClass.staticImports());
            javaClass.enums().forEach(javaEnum -> utilityStaticImports.add(javaClass.javaPackage() + "." + javaEnum.name() + ".*"));
        if (!javaClass.staticMethods().isEmpty() || !javaClass.staticConsts().isEmpty()) {
            var utilityClass = new JavaClass(
                    javaClass.annotations(),
                    new JavaType(javaClass.name() + "Module"),
                    javaClass.javaPackage(),
                    utilityStaticImports,
                    javaClass.staticConsts(),
                    javaClass.staticMethods(),
                    new TreeSet<>(),
                    new TreeSet<>(),
                    new TreeSet<>()
            );
            compiled.add(new GeneratedModule(
                    relativePath(javaClass, utilityClass.name().toString()),
                    time(timings::addSourceRenderNanos, () -> code(utilityClass, utilityClass.name().toString(), false))
            ));
        }
        return List.copyOf(compiled);
    }


    private java.util.Map<String, String> buildFunctionNameOverrides(CompiledProgram program) {
        var overrides = new java.util.LinkedHashMap<String, String>();
        var collisions = new java.util.LinkedHashMap<String, java.util.List<dev.capylang.compiler.CompiledFunction>>();
        var ownerModuleNames = new java.util.IdentityHashMap<dev.capylang.compiler.CompiledFunction, String>();
        for (var module : program.modules()) {
            for (var function : module.functions()) {
                ownerModuleNames.put(function, module.name());
                var ownerKey = function.name().startsWith(METHOD_DECL_PREFIX)
                        ? function.name().substring(0, Math.max(function.name().lastIndexOf("__"), METHOD_DECL_PREFIX.length()))
                        : module.name();
                var normalizedBaseName = normalizeJavaMethodIdentifier(baseMethodName(function.name()));
                var erasedSignature = function.parameters().stream()
                        .map(parameter -> erasedJavaType(parameter.type()))
                        .collect(joining(","));
                collisions.computeIfAbsent(ownerKey + "|" + normalizedBaseName + "|" + erasedSignature, ignored -> new java.util.ArrayList<>()).add(function);
            }
        }
        for (var entry : collisions.entrySet()) {
            var functions = entry.getValue();
            if (functions.size() < 2) {
                continue;
            }
            var canonicalNamedFunction = canonicalNamedFunction(functions);
            var mixedRawNames = functions.stream()
                    .map(function -> baseMethodName(function.name()))
                    .distinct()
                    .count() > 1;
            for (var function : functions) {
                var rawBaseName = baseMethodName(function.name());
                var normalizedBaseName = normalizeJavaMethodIdentifier(rawBaseName);
                var overloadSuffix = overloadSuffix(function);
                var legacyEmittedName = normalizedBaseName + overloadSuffix;
                var namedCanonical = canonicalNamedFunction.filter(named -> named == function).isPresent();
                var emittedName = mixedRawNames
                        ? (namedCanonical
                                ? normalizedBaseName
                                : normalizedBaseName + "__" + methodVariantSuffix(rawBaseName) + overloadSuffix)
                        : legacyEmittedName;
                var parameterTypes = function.parameters().stream().map(dev.capylang.compiler.CompiledFunction.CompiledFunctionParameter::type).toList();
                overrides.put(signatureKey(function.name(), parameterTypes), emittedName);
                if (!function.name().startsWith(METHOD_DECL_PREFIX)) {
                    overrides.put(signatureKey(moduleQualifiedName(ownerModuleNames, function), parameterTypes), emittedName);
                }
                if (mixedRawNames || !function.name().startsWith(METHOD_DECL_PREFIX)) {
                    overrides.put(signatureKey(baseMethodName(function.name()), parameterTypes), emittedName);
                }
                if (!emittedName.equals(legacyEmittedName)) {
                    overrides.put(signatureKey(legacyEmittedName, parameterTypes), emittedName);
                }
            }
        }
        return java.util.Map.copyOf(overrides);
    }

    private String moduleQualifiedName(
            java.util.IdentityHashMap<dev.capylang.compiler.CompiledFunction, String> ownerModuleNames,
            dev.capylang.compiler.CompiledFunction function
    ) {
        var ownerModuleName = ownerModuleNames.get(function);
        if (ownerModuleName == null) {
            throw new IllegalStateException("Missing owner module for function: " + function.name());
        }
        return ownerModuleName + "." + function.name();
    }

    private static String signatureKey(String name, java.util.List<dev.capylang.compiler.CompiledType> parameterTypes) {
        return name + "|" + parameterTypes.stream().map(type -> String.valueOf(type)).collect(joining(","));
    }

    private static String baseMethodName(String name) {
        if (!name.startsWith(METHOD_DECL_PREFIX)) {
            return name;
        }
        var idx = name.lastIndexOf("__");
        return idx >= 0 ? name.substring(idx + 2) : name;
    }

    private static java.util.Optional<dev.capylang.compiler.CompiledFunction> canonicalNamedFunction(
            java.util.List<dev.capylang.compiler.CompiledFunction> functions
    ) {
        return functions.stream()
                .filter(function -> isNamedIdentifier(baseMethodName(function.name())))
                .findFirst();
    }

    private static boolean isNamedIdentifier(String value) {
        return value.chars().allMatch(ch -> Character.isLetterOrDigit(ch) || ch == '_');
    }

    private static String overloadSuffix(dev.capylang.compiler.CompiledFunction function) {
        var suffix = function.parameters().stream()
                .map(parameter -> sanitizeOverloadSuffix(String.valueOf(parameter.type())))
                .collect(joining("__"));
        return suffix.isBlank() ? "" : "__" + suffix;
    }

    private static String methodVariantSuffix(String rawBaseName) {
        var prefix = rawBaseName.chars().allMatch(ch -> Character.isLetterOrDigit(ch) || ch == '_') ? "name" : "op";
        return prefix + "_" + sanitizeMethodNameVariant(rawBaseName);
    }

    private static String sanitizeMethodNameVariant(String rawName) {
        var builder = new StringBuilder();
        for (var i = 0; i < rawName.length(); i++) {
            var ch = rawName.charAt(i);
            if (Character.isLetterOrDigit(ch)) {
                builder.append(Character.toLowerCase(ch));
            } else if (ch == '_') {
                builder.append('_');
            } else {
                if (!builder.isEmpty() && builder.charAt(builder.length() - 1) != '_') {
                    builder.append('_');
                }
                builder.append(symbolName(ch));
                builder.append('_');
            }
        }
        var sanitized = builder.toString().replaceAll("_+", "_");
        if (sanitized.startsWith("_")) {
            sanitized = sanitized.substring(1);
        }
        if (sanitized.endsWith("_")) {
            sanitized = sanitized.substring(0, sanitized.length() - 1);
        }
        return sanitized.isBlank() ? "generated" : sanitized;
    }

    private static String normalizeJavaMethodIdentifier(String rawName) {
        var leadingUnderscores = countLeadingUnderscores(rawName);
        var suffix = rawName.substring(leadingUnderscores);
        var normalized = normalizeJavaIdentifier(suffix, false);
        if (leadingUnderscores == 0) {
            return normalized;
        }
        return "_".repeat(leadingUnderscores) + normalized;
    }

    private static int countLeadingUnderscores(String value) {
        var count = 0;
        while (count < value.length() && value.charAt(count) == '_') {
            count++;
        }
        return count;
    }

    private static String normalizeJavaIdentifier(String name, boolean upperCamel) {
        var parts = java.util.stream.Stream.of(name.split("[^A-Za-z0-9]+"))
                .filter(part -> !part.isEmpty())
                .toList();

        var base = new StringBuilder();
        if (parts.isEmpty()) {
            base.append(encodeSymbolicIdentifier(name, upperCamel));
        } else {
            for (var i = 0; i < parts.size(); i++) {
                var part = parts.get(i);
                if (i == 0 && !upperCamel) {
                    base.append(Character.toLowerCase(part.charAt(0)));
                } else {
                    base.append(Character.toUpperCase(part.charAt(0)));
                }
                if (part.length() > 1) {
                    base.append(part.substring(1));
                }
            }
        }

        var identifier = base.toString();
        if (!Character.isJavaIdentifierStart(identifier.charAt(0))) {
            identifier = (upperCamel ? "T" : "v") + identifier;
        }
        return switch (identifier) {
            case "abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class",
                    "const", "continue", "default", "do", "double", "else", "enum", "extends", "final",
                    "finally", "float", "for", "goto", "if", "implements", "import", "instanceof", "int",
                    "interface", "long", "native", "new", "package", "private", "protected", "public",
                    "return", "short", "static", "strictfp", "super", "switch", "synchronized", "this",
                    "throw", "throws", "transient", "try", "void", "volatile", "while", "true", "false",
                    "null", "record", "sealed", "permits", "var", "yield" -> identifier + "_";
            default -> identifier;
        };
    }

    private static String encodeSymbolicIdentifier(String raw, boolean upperCamel) {
        var parts = new java.util.ArrayList<String>(raw.length());
        for (var i = 0; i < raw.length(); i++) {
            parts.add(symbolName(raw.charAt(i)));
        }
        if (parts.isEmpty()) {
            return upperCamel ? "Generated" : "generated";
        }
        var result = new StringBuilder();
        for (var i = 0; i < parts.size(); i++) {
            var part = parts.get(i);
            if (i == 0 && !upperCamel) {
                result.append(part);
            } else {
                result.append(Character.toUpperCase(part.charAt(0)));
                if (part.length() > 1) {
                    result.append(part.substring(1));
                }
            }
        }
        return result.toString();
    }

    private static String symbolName(char symbol) {
        return switch (symbol) {
            case '+' -> "plus";
            case '-' -> "minus";
            case '*' -> "star";
            case '/' -> "slash";
            case '\\' -> "backslash";
            case '^' -> "power";
            case '%' -> "mod";
            case '$' -> "dollar";
            case '#' -> "hash";
            case '@' -> "at";
            case '~' -> "tilde";
            case '!' -> "bang";
            case ':' -> "colon";
            case '<' -> "less";
            case '>' -> "greater";
            case '|' -> "pipe";
            default -> "op" + Integer.toHexString(symbol);
        };
    }

    private static String sanitizeOverloadSuffix(String typeName) {
        var sanitized = typeName.replaceAll("[^A-Za-z0-9]+", "_").replaceAll("_+", "_");
        if (sanitized.startsWith("_")) {
            sanitized = sanitized.substring(1);
        }
        if (sanitized.endsWith("_")) {
            sanitized = sanitized.substring(0, sanitized.length() - 1);
        }
        return sanitized.toLowerCase();
    }

    private static String erasedJavaType(dev.capylang.compiler.CompiledType type) {
        return switch (type) {
            case dev.capylang.compiler.CollectionLinkedType.CompiledList ignored -> "java.util.List";
            case dev.capylang.compiler.CollectionLinkedType.CompiledSet ignored -> "java.util.Set";
            case dev.capylang.compiler.CollectionLinkedType.CompiledDict ignored -> "java.util.Map";
            case dev.capylang.compiler.CompiledTupleType ignored -> "java.util.List";
            case dev.capylang.compiler.CompiledFunctionType functionType -> functionType.argumentType() == dev.capylang.compiler.PrimitiveLinkedType.NOTHING
                    ? "java.util.function.Supplier"
                    : "java.util.function.Function";
            case dev.capylang.compiler.PrimitiveLinkedType primitive -> primitive.name();
            case dev.capylang.compiler.CompiledGenericTypeParameter ignored -> "java.lang.Object";
            case dev.capylang.compiler.GenericDataType genericDataType -> {
                var name = genericDataType.name();
                var idx = name.indexOf('[');
                yield idx > 0 ? name.substring(0, idx) : name;
            }
        };
    }

    private <T> T time(java.util.function.LongConsumer recorder, Supplier<T> action) {
        var startedAt = System.nanoTime();
        var result = action.get();
        recorder.accept(System.nanoTime() - startedAt);
        return result;
    }

    private static final class GenerationTimings {
        private long astBuildNanos;
        private long sourceRenderNanos;

        private void addAstBuildNanos(long nanos) {
            astBuildNanos += nanos;
        }

        private void addSourceRenderNanos(long nanos) {
            sourceRenderNanos += nanos;
        }

        private long astBuildNanos() {
            return astBuildNanos;
        }

        private long sourceRenderNanos() {
            return sourceRenderNanos;
        }
    }

    private Path relativePath(JavaClass javaClass, String simpleName) {
        var packageName = javaClass.javaPackage().toString();
        if (packageName.isBlank()) {
            return Path.of(simpleName + ".java");
        }
        return Path.of(packageName.replace('.', '/'), simpleName + ".java");
    }

    private String code(JavaClass javaClass, String helperCallOwnerName, boolean allowPrivateStaticMethods) {
        var code = new StringBuilder();

        // package
        code.append("package ").append(javaClass.javaPackage()).append(";\n\n");

        // imports
        appendImports(code, javaClass.staticImports());
        if (!javaClass.staticImports().isEmpty()) {
            code.append('\n');
        }

        // generated annotation
        javaClass.annotations().forEach(annotation -> code.append(annotation).append("\n"));

        // class declaration
        code.append("public class ")
                .append(javaClass.name())
                .append("{");

        // interfaces
        code.append('\n');
        javaClass.interfaces()
                .stream()
                .map(javaInterface -> mapJavaInterface(javaInterface, helperCallOwnerName))
                .forEach(code::append);

        // records
        code.append('\n');
        javaClass.records()
                .stream()
                .map(record -> mapJavaRecord(record, helperCallOwnerName, false))
                .forEach(code::append);

        // enums
        code.append('\n');
        javaClass.enums()
                .stream()
                .map(this::mapJavaEnum)
                .forEach(code::append);

        // static members
        code.append('\n');
        javaClass.staticConsts().stream()
                .map(javaConst -> mapJavaConst(javaConst, allowPrivateStaticMethods, false, javaClass.name().toString()))
                .forEach(code::append);
        javaClass.staticMethods()
                .stream()
                .map(method -> mapJavaMethod(method, allowPrivateStaticMethods, javaClass.javaPackage().toString(), javaClass.name().toString()))
                .forEach(code::append);
        if (requiresUnsupportedHelper(code)) {
            code.append('\n').append(unsupportedHelperMethod());
        }

        // close object declaration
        code.append("}");

        return code.toString();
    }

    private String codeNestedInOwnerInterface(JavaClass javaClass, JavaInterface ownerInterface, String helperCallOwnerName) {
        var code = new StringBuilder();
        code.append("package ").append(javaClass.javaPackage()).append(";\n\n");
        appendImports(code, javaClass.staticImports());
        if (!javaClass.staticImports().isEmpty()) {
            code.append('\n');
        }
        javaClass.annotations().forEach(annotation -> code.append(annotation).append("\n"));

        code.append(mapJavaOwnerInterfaceHeader(ownerInterface)).append("{\n");
        if (ownerInterface instanceof JavaNormalInterface normalInterface) {
            normalInterface.methods().stream()
                    .map(this::mapJavaInterfaceMethod)
                    .forEach(method -> code.append(method).append('\n'));
            normalInterface.defaultMethods().stream()
                    .map(method -> mapJavaInterfaceDefaultMethod(method, helperCallOwnerName))
                    .forEach(code::append);
        }
        if (ownerInterface instanceof JavaSealedInterface sealedInterface) {
            sealedInterface.methods().stream()
                    .map(this::mapJavaInterfaceMethod)
                    .forEach(method -> code.append(method).append('\n'));
            sealedInterface.defaultMethods().stream()
                    .map(method -> mapJavaInterfaceDefaultMethod(method, helperCallOwnerName))
                    .forEach(code::append);
        }

        javaClass.interfaces().stream()
                .filter(javaInterface -> javaInterface != ownerInterface)
                .map(javaInterface -> mapJavaInterface(javaInterface, helperCallOwnerName))
                .map(this::removeVisibilityModifier)
                .forEach(code::append);
        javaClass.records().stream()
                .map(record -> mapJavaRecord(record, helperCallOwnerName, false))
                .map(this::removeVisibilityModifier)
                .forEach(code::append);
        javaClass.enums().stream()
                .map(this::mapJavaEnum)
                .map(this::removeVisibilityModifier)
                .forEach(code::append);
        javaClass.staticConsts().stream()
                .map(javaConst -> mapJavaConst(javaConst, false, true, javaClass.name().toString()))
                .forEach(code::append);
        javaClass.staticMethods().stream()
                .map(method -> mapJavaMethod(method, true, javaClass.javaPackage().toString(), javaClass.name().toString()))
                .forEach(code::append);
        if (requiresUnsupportedHelper(code)) {
            code.append('\n').append(unsupportedHelperMethod());
        }

        code.append("}\n");
        return code.toString();
    }

    private String removeVisibilityModifier(String declaration) {
        if (declaration.startsWith("public ")) {
            return declaration.substring("public ".length());
        }
        if (declaration.startsWith("private ")) {
            return declaration.substring("private ".length());
        }
        return declaration;
    }

    private String mapJavaInterfaceHeader(JavaInterface javaInterface) {
        return switch (javaInterface) {
            case JavaNormalInterface javaNormalInterface -> "public interface " + javaNormalInterface.name() + " ";
            case JavaSealedInterface javaSealedInterface -> {
                var permits = join(", ", javaSealedInterface.permits());
                var typeParameters = javaSealedInterface.typeParameters().isEmpty()
                        ? ""
                        : javaSealedInterface.typeParameters().stream().collect(joining(", ", "<", ">"));
                yield "public sealed interface " + javaSealedInterface.name() + typeParameters + " permits " + permits + " ";
            }
        };
    }

    private String mapJavaOwnerInterfaceHeader(JavaInterface javaInterface) {
        return switch (javaInterface) {
            case JavaNormalInterface javaNormalInterface -> "public interface " + javaNormalInterface.name() + " ";
            case JavaSealedInterface javaSealedInterface -> {
                var ownerName = javaSealedInterface.name().toString();
                var permits = javaSealedInterface.permits().stream()
                        .map(permit -> ownerName + "." + permit)
                        .collect(joining(", "));
                var typeParameters = javaSealedInterface.typeParameters().isEmpty()
                        ? ""
                        : javaSealedInterface.typeParameters().stream().collect(joining(", ", "<", ">"));
                yield "public sealed interface " + ownerName + typeParameters + " permits " + permits + " ";
            }
        };
    }

    private String codeTopLevelDeclaration(JavaClass javaClass, String declarationCode) {
        var code = new StringBuilder();
        code.append("package ").append(javaClass.javaPackage()).append(";\n\n");
        javaClass.annotations().forEach(annotation -> code.append(annotation).append("\n"));
        code.append(declarationCode);
        return code.toString();
    }

    private void appendImports(StringBuilder code, Set<String> staticImports) {
        var classImports = new TreeSet<String>();
        staticImports.stream()
                .map(this::classImportForStaticImport)
                .filter(className -> !className.isBlank())
                .forEach(classImport -> {
                    classImports.add(classImport);
                    var companionImport = extractCompanionOwnerImport(classImport);
                    if (!companionImport.isBlank()) {
                        classImports.add(companionImport);
                    }
                });
        classImports.forEach(classImport -> code.append("import ").append(classImport).append(";\n"));
        staticImports.stream()
                .sorted()
                .filter(staticImport -> !isTypeImport(staticImport))
                .forEach(staticImport -> code.append("import static ").append(staticImport).append(";\n"));
    }

    private String classImportForStaticImport(String staticImport) {
        if (!isTypeImport(staticImport)) {
            return extractClassNameFromStaticImport(staticImport);
        }
        var ownerImport = extractClassNameFromStaticImport(staticImport);
        if (ownerImport.isBlank()) {
            return "";
        }
        var member = staticImport.substring(staticImport.lastIndexOf('.') + 1);
        var ownerSimpleName = ownerImport.substring(ownerImport.lastIndexOf('.') + 1);
        return ownerSimpleName.equals(member) ? ownerImport : staticImport;
    }

    private boolean isTypeImport(String staticImport) {
        if (staticImport.endsWith(".*")) {
            return false;
        }
        var lastDot = staticImport.lastIndexOf('.');
        if (lastDot <= 0 || lastDot == staticImport.length() - 1) {
            return false;
        }
        var member = staticImport.substring(lastDot + 1);
        return Character.isUpperCase(member.charAt(0));
    }

    private String extractCompanionOwnerImport(String classImport) {
        if (!classImport.endsWith("Module")) {
            return "";
        }
        return classImport.substring(0, classImport.length() - "Module".length());
    }

    private String extractClassNameFromStaticImport(String staticImport) {
        if (staticImport.endsWith(".*")) {
            return staticImport.substring(0, staticImport.length() - 2);
        }
        var lastDot = staticImport.lastIndexOf('.');
        if (lastDot <= 0) {
            return "";
        }
        return staticImport.substring(0, lastDot);
    }

    private boolean hasTypeOrDataNameConflictWithFile(JavaClass javaClass) {
        var fileName = javaClass.name().toString();
        return javaClass.interfaces().stream().anyMatch(javaInterface -> javaInterface.name().toString().equals(fileName))
               || javaClass.records().stream().anyMatch(record -> record.name().toString().equals(fileName))
               || javaClass.enums().stream().anyMatch(javaEnum -> javaEnum.name().toString().equals(fileName));
    }

    private List<TopLevelDeclaration> topLevelDeclarations(JavaClass javaClass, String helperCallOwnerName) {
        var declarations = new ArrayList<TopLevelDeclaration>();
        javaClass.interfaces().forEach(javaInterface -> declarations.add(
                new TopLevelDeclaration(javaInterface.name().toString(), mapJavaInterface(javaInterface, helperCallOwnerName))
        ));
        javaClass.records().forEach(javaRecord -> declarations.add(
                new TopLevelDeclaration(javaRecord.name().toString(), mapJavaRecord(javaRecord, helperCallOwnerName, true))
        ));
        javaClass.enums().forEach(javaEnum -> declarations.add(
                new TopLevelDeclaration(javaEnum.name().toString(), mapJavaEnum(javaEnum))
        ));
        return List.copyOf(declarations);
    }

    private record TopLevelDeclaration(String name, String code) {
    }

    private String mapJavaRecord(JavaRecord record, String helperCallOwnerName, boolean topLevel) {
        var fields = record.fields().stream().map(this::mapJavaRecordField).collect(joining(", "));
        var typeParameters = record.typeParameters().isEmpty()
                ? ""
                : record.typeParameters().stream().collect(joining(", ", "<", ">"));
        var implementInterfaces = record.implementInterfaces().size() > 0
                ? record.implementInterfaces().stream()
                .map(Objects::toString)
                .collect(joining(", ", " implements ", " "))
                : "";
        var staticMethods = record.staticMethods().stream()
                .map(method -> mapJavaMethod(method, true, "", record.name().toString()))
                .collect(joining("\n"));
        var methods = record.methods().stream()
                .map(method -> mapJavaRecordMethod(method, helperCallOwnerName))
                .collect(joining("\n"));
        var withMethods = mapJavaRecordWithMethods(record);
        var toStringMethod = mapJavaRecordToString(record);
        var visibility = record.isPrivate() ? (topLevel ? "" : "private ") : "public ";
        return mapJavaDoc(record.comments())
               + visibility + " record " + record.name() + typeParameters + "(" + fields + ")" + implementInterfaces + "{"
               + staticMethods + methods + withMethods + toStringMethod + "}\n";
    }

    private String mapJavaRecordField(JavaRecord.JavaRecordField field) {
        return field.type() + " " + field.name();
    }


    private String mapJavaRecordWithMethods(JavaRecord record) {
        if (record.methods().stream().anyMatch(method -> "with".equals(method.name()))) {
            return "";
        }
        var parameters = record.fields().stream().map(this::mapJavaRecordField).collect(joining(", "));
        var arguments = record.fields().stream().map(JavaRecord.JavaRecordField::name).collect(joining(", "));
        if (record.fields().isEmpty()) {
            return "public " + record.name() + " with() { return this; }\n";
        }
        return "public " + record.name() + " with(" + parameters + ") {\n"
               + "return new " + record.name() + "(" + arguments + ");\n"
               + "}\n";
    }

    private String mapJavaRecordToString(JavaRecord record) {
        if (record.methods().stream().anyMatch(method -> "toString".equals(method.name()))) {
            return "";
        }
        if (record.fields().isEmpty()) {
            return "@Override public java.lang.String toString() { return \"" + record.name() + " { }\"; }\n";
        }
        var body = new StringBuilder();
        body.append("@Override public java.lang.String toString() { return \"")
                .append(record.name())
                .append(" { \"");
        for (int i = 0; i < record.fields().size(); i++) {
            var field = record.fields().get(i);
            if (i > 0) {
                body.append(" + \", \"");
            }
            var displayFieldName = mapRecordFieldDisplayName(record, field);
            body.append(" + \"\\\"")
                    .append(displayFieldName)
                    .append("\\\": \" + ")
                    .append(mapRecordFieldToStringValue(field));
        }
        body.append(" + \" }\"; }\n");
        return body.toString();
    }

    private String mapRecordFieldDisplayName(JavaRecord record, JavaRecord.JavaRecordField field) {
        if ("Error".equals(record.name().toString())
            && "ex".equals(field.name())
            && "dev.capylang.CapybaraException".equals(field.type().toString())) {
            return "message";
        }
        return field.name();
    }

    private String mapRecordFieldToStringValue(JavaRecord.JavaRecordField field) {
        return "dev.capylang.CapybaraToStringUtil.toStringValue(" + field.name() + ")";
    }

    private String mapJavaEnum(JavaEnum javaEnum) {
        var implementInterfaces = javaEnum.implementInterfaces().isEmpty()
                ? ""
                : javaEnum.implementInterfaces().stream().map(Objects::toString).collect(joining(", ", " implements ", " "));
        var values = javaEnum.values().isEmpty() ? List.of("INSTANCE") : javaEnum.values();
        var body = String.join(", ", values);
        if (values.size() == 1 && "INSTANCE".equals(values.getFirst())) {
            return "public enum " + javaEnum.name() + implementInterfaces + "{" + body + "}\n";
        }
        return "public enum " + javaEnum.name() + implementInterfaces + "{"
               + body + ";\n"
               + "public static java.util.Set<" + javaEnum.name() + "> valuesSet() {\n"
               + "return java.util.EnumSet.allOf(" + javaEnum.name() + ".class);\n"
               + "}\n"
               + "public static capy.lang.Result<" + javaEnum.name() + "> parse(java.lang.String name) {\n"
               + "try {\n"
               + "return new capy.lang.Result.Success<>(" + javaEnum.name() + ".valueOf(name));\n"
               + "} catch (java.lang.IllegalArgumentException ex) {\n"
               + "return new capy.lang.Result.Error(new dev.capylang.CapybaraException(\"Cannot parse enum `"
               + javaEnum.name()
               + "` from string: \" + name));\n"
               + "}\n"
               + "}\n"
               + "public static capy.lang.Result<" + javaEnum.name() + "> parse(int order) {\n"
               + "var all = " + javaEnum.name() + ".values();\n"
               + "if (order < 0 || order >= all.length) {\n"
               + "return new capy.lang.Result.Error(new dev.capylang.CapybaraException(\"Cannot parse enum `"
               + javaEnum.name()
               + "` from order: \" + order));\n"
               + "}\n"
               + "return new capy.lang.Result.Success<>(all[order]);\n"
               + "}\n"
               + "}\n";
    }


    private String mapJavaInterface(JavaInterface javaInterface, String helperCallOwnerName) {
        return switch (javaInterface) {
            case JavaNormalInterface javaNormalInterface -> mapJavaNormalInterface(javaNormalInterface, helperCallOwnerName);
            case JavaSealedInterface javaSealedInterface -> mapJavaSealedInterface(javaSealedInterface, helperCallOwnerName);
        };
    }

    private String mapJavaNormalInterface(JavaNormalInterface javaInterface, String helperCallOwnerName) {
        var methods = javaInterface.methods().size() > 0
                ? javaInterface.methods().stream()
                .map(this::mapJavaInterfaceMethod)
                .collect(joining("\n", "\n", ""))
                : "";
        var defaultMethods = javaInterface.defaultMethods().stream()
                .map(method -> mapJavaInterfaceDefaultMethod(method, helperCallOwnerName))
                .collect(joining());
        return mapJavaDoc(javaInterface.comments())
               + "public interface " + javaInterface.name() + " {" + methods + defaultMethods + "}\n";
    }

    private String mapJavaSealedInterface(JavaSealedInterface javaInterface, String helperCallOwnerName) {
        var permits = join(", ", javaInterface.permits());
        var typeParameters = javaInterface.typeParameters().isEmpty()
                ? ""
                : javaInterface.typeParameters().stream().collect(joining(", ", "<", ">"));
        var methods = javaInterface.methods().size() > 0
                ? javaInterface.methods().stream()
                .map(this::mapJavaInterfaceMethod)
                .collect(joining("\n", "\n", ""))
                : "";
        var defaultMethods = javaInterface.defaultMethods().stream()
                .map(method -> mapJavaInterfaceDefaultMethod(method, helperCallOwnerName))
                .collect(joining());
        return mapJavaDoc(javaInterface.comments())
               + "public sealed interface " + javaInterface.name() + typeParameters + " permits " + permits + " {" + methods + defaultMethods + "}\n";
    }

    private String mapJavaInterfaceMethod(JavaInterface.JavaInterfaceMethod method) {
        return method.returnType() + " " + method.name() + "();";
    }

    private String mapJavaInterfaceDefaultMethod(JavaMethod method, String helperCallOwnerName) {
        var prefix = method.isPrivate() ? "private" : "default";
        var methodTypeParameters = method.typeParameters().isEmpty()
                ? ""
                : method.typeParameters().stream().collect(joining(", ", "<", ">")) + " ";
        return mapJavaDoc(method.comments())
               + prefix + " " + methodTypeParameters + method.returnType() + " " + mapMethodName(method.name()) + "(" + mapFunctionParameters(method.parameters()) + ") {\n"
               + evaluateExpression(method.expression(), method.parameters(), helperCallOwnerName)
               + "\n}\n";
    }

    private String mapJavaMethod(JavaMethod method, boolean allowPrivateStaticMethods, String ownerPackage, String ownerName) {
        if (method.programMain()) {
            return mapJavaProgramMainMethod(method);
        }
        var methodTypeParameters = method.typeParameters().isEmpty()
                ? ""
                : method.typeParameters().stream().collect(joining(", ", "<", ">")) + " ";
        var visibility = method.isPrivate()
                ? (allowPrivateStaticMethods ? "private " : "")
                : "public ";
        if (isCapyDateTimeClockNowMethod(ownerPackage, ownerName, method)) {
            return mapCapyDateTimeClockNowMethod(method, visibility, methodTypeParameters);
        }
        if (isCapyIoConsoleMethod(ownerPackage, ownerName, method)) {
            return mapCapyIoConsoleMethod(method, visibility, methodTypeParameters);
        }
        if (isCapyTestTimedMethod(ownerPackage, ownerName, method)) {
            var nameParameter = method.parameters().get(0).generatedName();
            var assertParameter = method.parameters().get(1).generatedName();
            return mapJavaDoc(method.comments())
                   + visibility + "static " + methodTypeParameters + method.returnType() + " " + mapMethodName(method.name()) + "(" + mapFunctionParameters(method.parameters()) + ") {\n"
                   + "var start = System.currentTimeMillis();\n"
                   + "var result = _execute((" + assertParameter + ").assertions());\n"
                   + "var delta = System.currentTimeMillis() - start;\n"
                   + "return new TestCase(" + nameParameter + ", result, ((" + assertParameter + ").assertions()).size(), (delta/1000));\n"
                   + "}\n";
        }
        return mapJavaDoc(method.comments())
               + visibility + "static " + methodTypeParameters + method.returnType() + " " + mapMethodName(method.name()) + "(" + mapFunctionParameters(method.parameters()) + ") {\n"
               + evaluateExpression(method.expression(), method.parameters())
               + "\n}\n";
    }

    private boolean isCapyDateTimeClockNowMethod(String ownerPackage, String ownerName, JavaMethod method) {
        return "capy.dateTime".equals(ownerPackage)
               && "Clock".equals(ownerName)
               && "now".equals(mapMethodName(method.name()))
               && method.parameters().isEmpty()
               && isEffectTypeReference(method.returnType().toString());
    }

    private String mapCapyDateTimeClockNowMethod(JavaMethod method, String visibility, String methodTypeParameters) {
        return mapJavaDoc(method.comments())
               + visibility + "static " + methodTypeParameters + method.returnType() + " " + mapMethodName(method.name()) + "() {\n"
               + "return capy.lang.Effect.delay(() -> dev.capylang.DateTimeUtil.fromJavaOffsetDateTime(java.time.OffsetDateTime.now(java.time.ZoneOffset.UTC)));\n"
               + "}\n";
    }

    private boolean isCapyIoConsoleMethod(String ownerPackage, String ownerName, JavaMethod method) {
        if (!"capy.io".equals(ownerPackage) || !"Console".equals(ownerName) || !isEffectTypeReference(method.returnType().toString())) {
            return false;
        }
        var methodName = mapMethodName(method.name());
        return switch (methodName) {
            case "print", "println", "printError", "printlnError" ->
                    method.parameters().size() == 1;
            case "readLine" -> method.parameters().isEmpty();
            default -> false;
        };
    }

    private String mapCapyIoConsoleMethod(JavaMethod method, String visibility, String methodTypeParameters) {
        var methodName = mapMethodName(method.name());
        var runtimeCall = switch (methodName) {
            case "print", "println", "printError", "printlnError" ->
                    "dev.capylang.ConsoleUtil." + methodName + "(" + method.parameters().getFirst().generatedName() + ")";
            case "readLine" -> "dev.capylang.ConsoleUtil.readLine()";
            default -> throw new IllegalStateException("Unsupported Console method: " + methodName);
        };
        return mapJavaDoc(method.comments())
               + visibility + "static " + methodTypeParameters + method.returnType() + " " + methodName + "(" + mapFunctionParameters(method.parameters()) + ") {\n"
               + "return capy.lang.Effect.delay(() -> " + runtimeCall + ");\n"
               + "}\n";
    }

    private boolean isCapyTestTimedMethod(String ownerPackage, String ownerName, JavaMethod method) {
        if (!"capy.test".equals(ownerPackage) || !"CapyTest".equals(ownerName)) {
            return false;
        }
        if (!"test".equals(mapMethodName(method.name()))) {
            return false;
        }
        if (!"TestCase".equals(method.returnType().toString())) {
            return false;
        }
        if (method.parameters().size() != 2) {
            return false;
        }
        var first = method.parameters().get(0);
        var second = method.parameters().get(1);
        return "java.lang.String".equals(first.type().toString()) && "Assert".equals(second.type().toString());
    }

    private String mapJavaRecordMethod(JavaMethod method, String helperCallOwnerName) {
        var methodTypeParameters = method.typeParameters().isEmpty()
                ? ""
                : method.typeParameters().stream().collect(joining(", ", "<", ">")) + " ";
        return mapJavaDoc(method.comments())
               + (method.isPrivate() ? "private" : "public") + " " + methodTypeParameters + method.returnType() + " " + mapMethodName(method.name()) + "(" + mapFunctionParameters(method.parameters()) + ") {\n"
               + evaluateExpression(method.expression(), method.parameters(), helperCallOwnerName)
               + "\n}\n";
    }

    private String mapJavaProgramMainMethod(JavaMethod method) {
        var rewrittenParameters = method.parameters().isEmpty()
                ? method.parameters()
                : List.of(new JavaMethod.JavaFunctionParameter(
                        method.parameters().getFirst().type(),
                        method.parameters().getFirst().sourceName(),
                        "__capybaraArgsList"
                ));
        var evaluated = evaluateExpression(method.expression(), rewrittenParameters);
        var returnsEffectProgram = isEffectProgramType(method.returnType().toString());
        var programType = returnsEffectProgram
                ? normalizeProgramTypeReference(effectPayloadTypeReference(method.returnType().toString()))
                : normalizeProgramTypeReference(method.returnType().toString());
        var programComputation = returnsEffectProgram
                ? evaluated.replaceFirst("(?m)^\\s*return\\s+", "var __capybaraProgramEffect = ")
                  + "\n" + programType + " program = __capybaraProgramEffect.unsafeRun();"
                : evaluated.replaceFirst("(?m)^\\s*return\\s+", programType + " program = ");
        var successType = programType + ".Success";
        var failedType = programType + ".Failed";
        return mapJavaDoc(method.comments())
               + "public static void main(java.lang.String[] args) {\n"
               + "var __capybaraArgsList = java.util.List.of(args);\n"
               + programComputation + "\n"
               + "switch (program) {\n"
               + "case " + successType + "(var __capybaraResults) -> {\n"
               + "__capybaraResults.forEach(System.out::println);\n"
               + "}\n"
               + "case " + failedType + "(var __capybaraExitCode, var __capybaraErrors) -> {\n"
               + "__capybaraErrors.forEach(System.err::println);\n"
               + "System.exit(__capybaraExitCode);\n"
               + "}\n"
               + "default -> throw new java.lang.IllegalStateException(\"Unexpected value: \" + program);\n"
               + "}\n"
               + "}\n";
    }

    private String normalizeProgramTypeReference(String typeName) {
        var parts = typeName.split("\\.");
        if (parts.length >= 2 && parts[parts.length - 1].equals(parts[parts.length - 2])) {
            return String.join(".", java.util.Arrays.copyOf(parts, parts.length - 1));
        }
        return typeName;
    }

    private boolean isEffectProgramType(String typeName) {
        if (!isEffectTypeReference(typeName)) {
            return false;
        }
        return isProgramTypeReference(effectPayloadTypeReference(typeName));
    }

    private boolean isEffectTypeReference(String typeName) {
        var normalized = typeName.replace(" ", "");
        var genericStart = normalized.indexOf('<');
        var rawType = genericStart >= 0 ? normalized.substring(0, genericStart) : normalized;
        return "Effect".equals(rawType) || rawType.endsWith(".Effect");
    }

    private String effectPayloadTypeReference(String typeName) {
        var normalized = typeName.replace(" ", "");
        var genericStart = normalized.indexOf('<');
        if (genericStart < 0 || !normalized.endsWith(">")) {
            return "capy.lang.Program";
        }
        return normalized.substring(genericStart + 1, normalized.length() - 1);
    }

    private boolean isProgramTypeReference(String typeName) {
        var normalized = normalizeProgramTypeReference(typeName.replace(" ", ""));
        return "Program".equals(normalized) || "capy.lang.Program".equals(normalized) || normalized.endsWith(".Program");
    }

    private String mapJavaDoc(List<String> comments) {
        if (comments == null || comments.isEmpty()) {
            return "";
        }
        return comments.stream()
                .map(line -> line.isEmpty() ? "///" : "/// " + line)
                .collect(joining("\n", "", "\n"));
    }

    private String mapJavaConst(
            JavaConst javaConst,
            boolean allowPrivateStaticMembers,
            boolean ownerInterfaceMember,
        String ownerTypeName
    ) {
        var visibility = constVisibility(javaConst, allowPrivateStaticMembers, ownerInterfaceMember);
        return mapJavaDoc(javaConst.comments())
               + visibility + "static final " + javaConst.type() + " " + javaConst.name() + " = "
               + extractInitializerExpression(evaluateExpression(javaConst.expression(), List.of(), ownerTypeName))
               + ";\n";
    }

    private String constVisibility(JavaConst javaConst, boolean allowPrivateStaticMembers, boolean ownerInterfaceMember) {
        if (ownerInterfaceMember) {
            return "public ";
        }
        if (javaConst.isPrivate()) {
            return allowPrivateStaticMembers ? "private " : "";
        }
        return "public ";
    }

    private String extractInitializerExpression(String methodBody) {
        var trimmed = methodBody.trim();
        if (trimmed.startsWith("return ") && trimmed.endsWith(";")) {
            return trimmed.substring("return ".length(), trimmed.length() - 1).trim();
        }
        throw new IllegalStateException("Const initializer should be a simple expression: " + methodBody);
    }

    private String mapMethodName(String name) {
        if (name.startsWith(METHOD_DECL_PREFIX)) {
            var idx = name.lastIndexOf("__");
            if (idx >= 0 && idx + 2 < name.length()) {
                return name.substring(idx + 2);
            }
        }
        return name;
    }

    private String mapFunctionParameters(List<JavaMethod.JavaFunctionParameter> parameters) {
        return parameters.stream()
                .map(this::mapFunctionParameter)
                .collect(joining(", "));
    }

    private String mapFunctionParameter(JavaMethod.JavaFunctionParameter parameter) {
        return parameter.type() + " " + parameter.generatedName();
    }

    private boolean requiresUnsupportedHelper(StringBuilder code) {
        var marker = "__capybaraUnsupported(\"";
        var generatedCode = code.toString();
        var index = generatedCode.indexOf(marker);
        while (index >= 0) {
            if (index == 0) {
                return true;
            }
            var previousChar = generatedCode.charAt(index - 1);
            if (previousChar != '\\' && previousChar != '"') {
                return true;
            }
            index = generatedCode.indexOf(marker, index + 1);
        }
        return false;
    }

    private String unsupportedHelperMethod() {
        return "private static <T> T __capybaraUnsupported(String message) {\n"
               + "throw new java.lang.UnsupportedOperationException(message);\n"
               + "}\n";
    }

}
