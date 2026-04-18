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
                .toList();
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
        for (var module : program.modules()) {
            for (var function : module.functions()) {
                var ownerKey = function.name().startsWith(METHOD_DECL_PREFIX)
                        ? function.name().substring(0, Math.max(function.name().lastIndexOf("__"), METHOD_DECL_PREFIX.length()))
                        : module.name();
                var baseName = baseMethodName(function.name());
                var erasedSignature = function.parameters().stream()
                        .map(parameter -> erasedJavaType(parameter.type()))
                        .collect(joining(","));
                collisions.computeIfAbsent(ownerKey + "|" + baseName + "|" + erasedSignature, ignored -> new java.util.ArrayList<>()).add(function);
            }
        }
        for (var entry : collisions.entrySet()) {
            var functions = entry.getValue();
            if (functions.size() < 2) {
                continue;
            }
            for (var function : functions) {
                var emittedName = baseMethodName(function.name()) + "__" + function.parameters().stream()
                        .map(parameter -> sanitizeOverloadSuffix(String.valueOf(parameter.type())))
                        .collect(joining("__"));
                var parameterTypes = function.parameters().stream().map(dev.capylang.compiler.CompiledFunction.CompiledFunctionParameter::type).toList();
                overrides.put(signatureKey(function.name(), parameterTypes), emittedName);
                if (!function.name().startsWith(METHOD_DECL_PREFIX)) {
                    overrides.put(signatureKey(baseMethodName(function.name()), parameterTypes), emittedName);
                    overrides.put(signatureKey(moduleQualifiedName(program, function), parameterTypes), emittedName);
                }
            }
        }
        return java.util.Map.copyOf(overrides);
    }

    private String moduleQualifiedName(CompiledProgram program, dev.capylang.compiler.CompiledFunction function) {
        var ownerModule = program.modules().stream()
                .filter(module -> module.functions().contains(function))
                .findFirst()
                .orElseThrow();
        return ownerModule.name() + "." + function.name();
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
        var programComputation = evaluateExpression(method.expression(), rewrittenParameters)
                .replaceFirst("(?m)^\\s*return\\s+", "var program = ");
        var programType = normalizeProgramTypeReference(method.returnType().toString());
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


