package dev.capylang.generator.java;

import dev.capylang.compiler.CollectionLinkedType;
import dev.capylang.compiler.CompiledDataParentType;
import dev.capylang.compiler.CompiledDataType;
import dev.capylang.compiler.CompiledFunctionType;
import dev.capylang.compiler.CompiledGenericTypeParameter;
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
        var path = reflectionPackagePath(typeName, fallbackPackagePath);
        var packageName = path.isBlank() ? "" : simpleReflectionTypeName(path);
        return new JavaDataValueInfo(
                simpleReflectionTypeName(typeName),
                packageName,
                path,
                fields
        );
    }

    public static String dataValueInfoExpression(JavaDataValueInfo dataValueInfo) {
        var fieldValueInfos = new ArrayList<String>(dataValueInfo.fields().size());
        for (var field : dataValueInfo.fields()) {
            var typeInfo = reflectionTypeInfo(field.type(), dataValueInfo.packagePath());
            fieldValueInfos.add("new capy.metaProg.Reflection.FieldValueInfo("
                                + javaString(field.name()) + ", "
                                + typeInfo + ", "
                                + field.valueExpression() + ")");
        }

        return "new capy.metaProg.Reflection.DataValueInfo("
               + javaString(dataValueInfo.name()) + ", "
               + "new capy.metaProg.Reflection.PackageInfo("
               + javaString(dataValueInfo.packageName()) + ", "
               + javaString(dataValueInfo.packagePath()) + "), "
               + reflectionList("capy.metaProg.Reflection.FieldValueInfo", fieldValueInfos)
               + ")";
    }

    public static String reflectionTypeInfo(dev.capylang.compiler.CompiledType type, String fallbackPackagePath) {
        return switch (type) {
            case PrimitiveLinkedType primitive ->
                    "new capy.metaProg.Reflection.DataInfo("
                    + javaString(primitive.name().toLowerCase(java.util.Locale.ROOT)) + ", "
                    + reflectionEmptyPackageInfo() + ")";
            case CollectionLinkedType.CompiledList listType ->
                    "new capy.metaProg.Reflection.ListInfo("
                    + javaString("list") + ", "
                    + reflectionEmptyPackageInfo() + ", "
                    + reflectionTypeInfo(listType.elementType(), fallbackPackagePath) + ")";
            case CollectionLinkedType.CompiledSet setType ->
                    "new capy.metaProg.Reflection.SetInfo("
                    + javaString("set") + ", "
                    + reflectionEmptyPackageInfo() + ", "
                    + reflectionTypeInfo(setType.elementType(), fallbackPackagePath) + ")";
            case CollectionLinkedType.CompiledDict dictType ->
                    "new capy.metaProg.Reflection.DictInfo("
                    + javaString("dict") + ", "
                    + reflectionEmptyPackageInfo() + ", "
                    + reflectionTypeInfo(dictType.valueType(), fallbackPackagePath) + ")";
            case CompiledTupleType tupleType ->
                    "new capy.metaProg.Reflection.TupleInfo("
                    + javaString("tuple") + ", "
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
                    + reflectionEmptyPackageInfo() + ")";
            case CompiledDataParentType parentType ->
                    "new capy.metaProg.Reflection.DataInfo("
                    + javaString(simpleReflectionTypeName(parentType.name())) + ", "
                    + reflectionPackageInfo(parentType.name(), fallbackPackagePath) + ")";
            case CompiledDataType dataType ->
                    "new capy.metaProg.Reflection.DataInfo("
                    + javaString(simpleReflectionTypeName(dataType.name())) + ", "
                    + reflectionPackageInfo(dataType.name(), fallbackPackagePath) + ")";
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

    private record ReflectionFunctionShape(
            List<dev.capylang.compiler.CompiledType> parameterTypes,
            dev.capylang.compiler.CompiledType returnType
    ) {
    }
}
