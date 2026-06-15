package dev.capylang.generator.java;

import dev.capylang.compiler.CollectionLinkedType;
import dev.capylang.compiler.CompiledAnnotation;
import dev.capylang.compiler.CompiledAnnotationArgument;
import dev.capylang.compiler.CompiledAnnotationValue;
import dev.capylang.compiler.CompiledDataParentType;
import dev.capylang.compiler.CompiledDataType;
import dev.capylang.compiler.CompiledFunctionType;
import dev.capylang.compiler.CompiledGenericTypeParameter;
import dev.capylang.compiler.CompiledObjectType;
import dev.capylang.compiler.CompiledPrimitiveBackedType;
import dev.capylang.compiler.CompiledTupleType;
import dev.capylang.compiler.PrimitiveLinkedType;

import java.util.ArrayList;
import java.util.List;

public final class ReflectionValueInfoJava {
    private ReflectionValueInfoJava() {
    }

    public static JavaDataValueInfo dataValueInfo(
            String typeName,
            String fallbackPackagePath,
            List<JavaDataValueInfo.Field> fields
    ) {
        return dataValueInfo(typeName, fallbackPackagePath, fields, List.of());
    }

    public static JavaDataValueInfo dataValueInfo(
            String typeName,
            String fallbackPackagePath,
            List<JavaDataValueInfo.Field> fields,
            List<CompiledAnnotation> annotations
    ) {
        var path = reflectionPackagePath(typeName, fallbackPackagePath);
        var packageName = path.isBlank() ? "" : simpleReflectionTypeName(path);
        return new JavaDataValueInfo(
                simpleReflectionTypeName(typeName),
                packageName,
                path,
                fields,
                annotations
        );
    }

    public static String dataValueInfoExpression(JavaDataValueInfo dataValueInfo) {
        var fieldValueInfos = new ArrayList<String>(dataValueInfo.fields().size());
        for (var field : dataValueInfo.fields()) {
            var typeInfo = reflectionTypeInfo(field.type(), dataValueInfo.packagePath());
            fieldValueInfos.add("new capy.metaProg.Reflection.FieldValueInfo("
                                + javaString(field.name()) + ", "
                                + typeInfo + ", "
                                + field.valueExpression() + ", "
                                + reflectionAnnotations(field.annotations()) + ")");
        }

        return "new capy.metaProg.Reflection.DataValueInfo("
               + javaString(dataValueInfo.name()) + ", "
               + "new capy.metaProg.Reflection.PackageInfo("
               + javaString(dataValueInfo.packageName()) + ", "
               + javaString(dataValueInfo.packagePath()) + "), "
               + reflectionList("capy.metaProg.Reflection.FieldValueInfo", fieldValueInfos)
               + ", "
               + reflectionAnnotations(dataValueInfo.annotations())
               + ")";
    }

    public static String reflectionTypeInfo(dev.capylang.compiler.CompiledType type, String fallbackPackagePath) {
        return switch (type) {
            case PrimitiveLinkedType primitive ->
                    "new capy.metaProg.Reflection.DataInfo("
                    + javaString(primitive == PrimitiveLinkedType.STRING
                            ? "String"
                            : primitive.name().toLowerCase(java.util.Locale.ROOT)) + ", "
                    + reflectionEmptyPackageInfo() + ", "
                    + reflectionAnnotations(List.of()) + ")";
            case CollectionLinkedType.CompiledList listType ->
                    "new capy.metaProg.Reflection.ListInfo("
                    + javaString("List") + ", "
                    + reflectionEmptyPackageInfo() + ", "
                    + reflectionTypeInfo(listType.elementType(), fallbackPackagePath) + ")";
            case CollectionLinkedType.CompiledSet setType ->
                    "new capy.metaProg.Reflection.SetInfo("
                    + javaString("Set") + ", "
                    + reflectionEmptyPackageInfo() + ", "
                    + reflectionTypeInfo(setType.elementType(), fallbackPackagePath) + ")";
            case CollectionLinkedType.CompiledDict dictType ->
                    "new capy.metaProg.Reflection.DictInfo("
                    + javaString("Dict") + ", "
                    + reflectionEmptyPackageInfo() + ", "
                    + reflectionTypeInfo(dictType.valueType(), fallbackPackagePath) + ")";
            case CompiledTupleType tupleType ->
                    "new capy.metaProg.Reflection.TupleInfo("
                    + javaString("Tuple") + ", "
                    + reflectionEmptyPackageInfo() + ", "
                    + reflectionList(
                            "capy.metaProg.Reflection.AnyInfo",
                            tupleType.elementTypes().stream()
                                    .map(elementType -> reflectionTypeInfo(elementType, fallbackPackagePath))
                                    .toList()
                    ) + ")";
            case CompiledFunctionType functionType -> {
                var shape = flattenReflectionFunctionType(functionType);
                yield "new capy.metaProg.Reflection.FunctionTypeInfo("
                      + javaString("function") + ", "
                      + reflectionEmptyPackageInfo() + ", "
                      + reflectionList(
                              "capy.metaProg.Reflection.AnyInfo",
                              shape.parameterTypes().stream()
                                      .map(parameterType -> reflectionTypeInfo(parameterType, fallbackPackagePath))
                                      .toList()
                      ) + ", "
                      + reflectionTypeInfo(shape.returnType(), fallbackPackagePath) + ")";
            }
            case CompiledGenericTypeParameter genericTypeParameter ->
                    "new capy.metaProg.Reflection.DataInfo("
                    + javaString(genericTypeParameter.name()) + ", "
                    + reflectionEmptyPackageInfo() + ", "
                    + reflectionAnnotations(List.of()) + ")";
            case CompiledPrimitiveBackedType primitiveBackedType ->
                    reflectionTypeInfo(primitiveBackedType.backingType(), fallbackPackagePath);
            case CompiledDataParentType parentType ->
                    "new capy.metaProg.Reflection.DataInfo("
                    + javaString(simpleReflectionTypeName(parentType.name())) + ", "
                    + reflectionPackageInfo(parentType.name(), fallbackPackagePath) + ", "
                    + reflectionAnnotations(parentType.annotations()) + ")";
            case CompiledDataType dataType ->
                    "new capy.metaProg.Reflection.DataInfo("
                    + javaString(simpleReflectionTypeName(dataType.name())) + ", "
                    + reflectionPackageInfo(dataType.name(), fallbackPackagePath) + ", "
                    + reflectionAnnotations(dataType.annotations()) + ")";
            case CompiledObjectType objectType ->
                    "new capy.metaProg.Reflection.DataInfo("
                    + javaString(simpleReflectionTypeName(objectType.name())) + ", "
                    + reflectionPackageInfo(objectType.name(), fallbackPackagePath) + ", "
                    + reflectionAnnotations(objectType.annotations()) + ")";
        };
    }

    public static String reflectionAnnotations(List<CompiledAnnotation> annotations) {
        return reflectionList(
                "capy.metaProg.Reflection.AnnotationInfo",
                annotations.stream()
                        .map(ReflectionValueInfoJava::reflectionAnnotation)
                        .toList()
        );
    }

    private static String reflectionAnnotation(CompiledAnnotation annotation) {
        return "new capy.metaProg.Reflection.AnnotationInfo("
               + javaString(annotation.name()) + ", "
               + reflectionPackageInfoValues(annotation.packageName(), annotation.packagePath()) + ", "
               + reflectionList(
                       "capy.metaProg.Reflection.AnnotationArgumentInfo",
                       annotation.arguments().stream()
                               .map(ReflectionValueInfoJava::reflectionAnnotationArgument)
                               .toList()
               )
               + ")";
    }

    private static String reflectionAnnotationArgument(CompiledAnnotationArgument argument) {
        return "new capy.metaProg.Reflection.AnnotationArgumentInfo("
               + javaString(argument.name()) + ", "
               + reflectionAnnotationValue(argument.value())
               + ")";
    }

    private static String reflectionAnnotationValue(CompiledAnnotationValue value) {
        return switch (value) {
            case CompiledAnnotationValue.StringValue stringValue ->
                    "new capy.metaProg.Reflection.AnnotationString(" + normalizeStringLiteral(stringValue.value()) + ")";
            case CompiledAnnotationValue.IntValue intValue ->
                    "new capy.metaProg.Reflection.AnnotationInt(" + intValue.value() + ")";
            case CompiledAnnotationValue.LongValue longValue ->
                    "new capy.metaProg.Reflection.AnnotationLong(" + longValue.value() + ")";
            case CompiledAnnotationValue.DoubleValue doubleValue ->
                    "new capy.metaProg.Reflection.AnnotationDouble(" + doubleValue.value() + ")";
            case CompiledAnnotationValue.FloatValue floatValue ->
                    "new capy.metaProg.Reflection.AnnotationFloat(" + floatValue.value() + ")";
            case CompiledAnnotationValue.BoolValue boolValue ->
                    "new capy.metaProg.Reflection.AnnotationBool(" + boolValue.value() + ")";
            case CompiledAnnotationValue.TypeNameValue typeNameValue ->
                    "new capy.metaProg.Reflection.AnnotationTypeName(" + javaString(typeNameValue.name()) + ")";
            case CompiledAnnotationValue.NothingValue ignored ->
                    "new capy.metaProg.Reflection.AnnotationNothing()";
        };
    }

    public static String reflectionPackagePath(String symbolName, String fallbackPackagePath) {
        var normalized = symbolName.replace('\\', '/');
        var dot = normalized.lastIndexOf('.');
        var slash = normalized.lastIndexOf('/');
        if (slash >= 0) {
            if (dot > slash) {
                return normalized.substring(0, dot).replaceFirst("^/", "");
            }
            return normalized.substring(0, slash).replaceFirst("^/", "");
        }
        if (dot > 0) {
            return normalized.substring(0, dot);
        }
        return fallbackPackagePath == null ? "" : fallbackPackagePath;
    }

    private static String reflectionList(String javaElementType, List<String> values) {
        if (values.isEmpty()) {
            return "java.util.List.<" + javaElementType + ">of()";
        }
        return "java.util.List.of(" + String.join(", ", values) + ")";
    }

    private static ReflectionFunctionShape flattenReflectionFunctionType(CompiledFunctionType functionType) {
        var parameterTypes = new ArrayList<dev.capylang.compiler.CompiledType>();
        dev.capylang.compiler.CompiledType current = functionType;
        while (current instanceof CompiledFunctionType currentFunctionType) {
            parameterTypes.add(currentFunctionType.argumentType());
            current = currentFunctionType.returnType();
        }
        return new ReflectionFunctionShape(List.copyOf(parameterTypes), current);
    }

    private static String reflectionPackageInfo(String symbolName, String fallbackPackagePath) {
        var path = reflectionPackagePath(symbolName, fallbackPackagePath);
        var name = path.isBlank() ? "" : simpleReflectionTypeName(path);
        return "new capy.metaProg.Reflection.PackageInfo(" + javaString(name) + ", " + javaString(path) + ")";
    }

    private static String reflectionPackageInfoValues(String packageName, String packagePath) {
        return "new capy.metaProg.Reflection.PackageInfo("
               + javaString(packageName == null ? "" : packageName)
               + ", "
               + javaString(packagePath == null ? "" : packagePath.replaceFirst("^/", ""))
               + ")";
    }

    private static String reflectionEmptyPackageInfo() {
        return "new capy.metaProg.Reflection.PackageInfo(\"\", \"\")";
    }

    private static String simpleReflectionTypeName(String typeName) {
        var normalized = stripGenericSuffix(typeName);
        var slash = normalized.lastIndexOf('/');
        var dot = normalized.lastIndexOf('.');
        var index = Math.max(slash, dot);
        return index >= 0 ? normalized.substring(index + 1) : normalized;
    }

    private static String stripGenericSuffix(String typeName) {
        var idx = typeName.indexOf('[');
        return idx >= 0 ? typeName.substring(0, idx) : typeName;
    }

    private static String escapeJavaString(String value) {
        return value
                .replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\n", "\\n")
                .replace("\r", "\\r")
                .replace("\t", "\\t");
    }

    private static String javaString(String value) {
        return "\"" + escapeJavaString(value) + "\"";
    }

    private static String normalizeStringLiteral(String raw) {
        if (raw.length() < 2) {
            return javaString(raw);
        }
        if (raw.charAt(0) == '"' && raw.charAt(raw.length() - 1) == '"') {
            var content = normalizeDoubleQuotedContent(raw.substring(1, raw.length() - 1));
            return javaString(content);
        }
        if (raw.charAt(0) == '\'' && raw.charAt(raw.length() - 1) == '\'') {
            return javaString(raw.substring(1, raw.length() - 1));
        }
        return javaString(raw);
    }

    private static String normalizeDoubleQuotedContent(String content) {
        var normalized = new StringBuilder(content.length());
        for (var i = 0; i < content.length(); i++) {
            var ch = content.charAt(i);
            if (ch == '\\' && i + 1 < content.length()) {
                var next = content.charAt(i + 1);
                if (next == '"' || next == '\\') {
                    normalized.append(next);
                    i++;
                    continue;
                }
            }
            normalized.append(ch);
        }
        return normalized.toString();
    }

    private record ReflectionFunctionShape(
            List<dev.capylang.compiler.CompiledType> parameterTypes,
            dev.capylang.compiler.CompiledType returnType
    ) {
    }
}
