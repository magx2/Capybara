package dev.capylang.compiler;

import dev.capylang.compiler.CollectionLinkedType.CompiledDict;
import dev.capylang.compiler.CollectionLinkedType.CompiledList;
import dev.capylang.compiler.CollectionLinkedType.CompiledSet;
import dev.capylang.compiler.parser.CollectionType;
import dev.capylang.compiler.parser.CollectionType.DictType;
import dev.capylang.compiler.parser.CollectionType.ListType;
import dev.capylang.compiler.parser.CollectionType.SetType;
import dev.capylang.compiler.parser.DataType;
import dev.capylang.compiler.parser.FunctionType;
import dev.capylang.compiler.parser.PrimitiveType;
import dev.capylang.compiler.parser.Type;
import dev.capylang.compiler.parser.TupleType;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;


public class CapybaraTypeCompiler {
    public static final class LinkCache {
        private final Map<String, Result<CompiledType>> linkedTypesByKey = new HashMap<>();
        private final Map<String, ParsedDataTypeName> parsedDataTypeNames = new HashMap<>();
        private final Map<String, Type> parsedTypeArguments = new HashMap<>();
        private final Map<String, List<String>> splitTopLevelTypeArguments = new HashMap<>();
    }

    public static Result<CompiledType> linkType(Type type, Map<String, GenericDataType> dataTypes) {
        return linkType(type, dataTypes, new LinkCache());
    }

    public static Result<CompiledType> linkType(Type type, Map<String, GenericDataType> dataTypes, LinkCache linkCache) {
        var cacheKey = typeCacheKey(type);
        var cached = linkCache.linkedTypesByKey.get(cacheKey);
        if (cached != null) {
            return cached;
        }
        var linked = switch (type) {
            case PrimitiveType primitiveType -> Result.success(linkPrimitiveType(primitiveType));
            case CollectionType collectionType -> linkCollectionType(collectionType, dataTypes, linkCache);
            case DataType dataType -> linkDataType(dataType, dataTypes, linkCache);
            case FunctionType functionType -> linkFunctionType(functionType, dataTypes, linkCache);
            case TupleType tupleType -> linkTupleType(tupleType, dataTypes, linkCache);
        };
        linkCache.linkedTypesByKey.put(cacheKey, linked);
        return linked;
    }

    private static String typeCacheKey(Type type) {
        return switch (type) {
            case PrimitiveType primitiveType -> primitiveType.name();
            case CollectionType.ListType listType -> "list[" + typeCacheKey(listType.elementType()) + "]";
            case CollectionType.SetType setType -> "set[" + typeCacheKey(setType.elementType()) + "]";
            case CollectionType.DictType dictType -> "dict[" + typeCacheKey(dictType.valueType()) + "]";
            case DataType dataType -> dataType.name();
            case FunctionType functionType ->
                    "(" + typeCacheKey(functionType.argumentType()) + " => " + typeCacheKey(functionType.returnType()) + ")";
            case TupleType tupleType -> "tuple[" + tupleType.elementTypes().stream()
                    .map(CapybaraTypeCompiler::typeCacheKey)
                    .collect(java.util.stream.Collectors.joining(", ")) + "]";
        };
    }

    @Deprecated
    public static Result<CompiledType> linkType(Type type) {
        // TODO proper mapping of types
        return linkType(type, Map.of());
    }

    private static Result<CompiledType> linkDataType(DataType dataType, Map<String, GenericDataType> dataTypes, LinkCache linkCache) {
        var parsedName = parseDataTypeName(dataType.name(), linkCache);
        var baseName = parsedName.baseName();
        if (dataTypes.containsKey(baseName)) {
            return instantiateTypeArgumentsIfNeeded(withQualifiedNameIfNeeded(dataTypes.get(baseName), baseName), parsedName.typeArguments(), dataTypes, linkCache);
        }
        var matchedBySimpleName = matchBySimpleTypeName(baseName, dataTypes);
        if (matchedBySimpleName != null) {
            return instantiateTypeArgumentsIfNeeded(withQualifiedNameIfNeeded(dataTypes.get(matchedBySimpleName), baseName), parsedName.typeArguments(), dataTypes, linkCache);
        }
        var matchedQualified = matchQualifiedByModuleTail(baseName, dataTypes);
        if (matchedQualified != null) {
            return instantiateTypeArgumentsIfNeeded(withQualifiedNameIfNeeded(dataTypes.get(matchedQualified), matchedQualified), parsedName.typeArguments(), dataTypes, linkCache);
        }
        var normalized = normalizeQualifiedName(baseName);
        if (normalized != null && dataTypes.containsKey(normalized)) {
            return instantiateTypeArgumentsIfNeeded(withQualifiedNameIfNeeded(dataTypes.get(normalized), normalized), parsedName.typeArguments(), dataTypes, linkCache);
        }

        return Result.error("Data type \"" + baseName + "\" not found");
    }

    private static Result<CompiledType> instantiateTypeArgumentsIfNeeded(
            CompiledType linkedType,
            List<String> typeArguments,
            Map<String, GenericDataType> dataTypes,
            LinkCache linkCache
    ) {
        if (typeArguments.isEmpty()) {
            return Result.success(linkedType);
        }
        return typeArguments.stream()
                .map(typeArgument -> parseTypeArgument(typeArgument, linkCache))
                .map(type -> linkType(type, dataTypes, linkCache))
                .collect(new ResultCollectionCollector<>())
                .map(linkedTypeArguments -> instantiateTypeArguments(linkedType, linkedTypeArguments));
    }

    private static Type parseTypeArgument(String raw) {
        return parseTypeArgument(raw, new LinkCache());
    }

    private static Type parseTypeArgument(String raw, LinkCache linkCache) {
        var cached = linkCache.parsedTypeArguments.get(raw);
        if (cached != null) {
            return cached;
        }
        var trimmed = raw.trim();
        var parsed = PrimitiveType.find(trimmed)
                .map(Type.class::cast)
                .orElseGet(() -> {
                    if (trimmed.startsWith("list[") && trimmed.endsWith("]")) {
                        return new ListType(parseTypeArgument(trimmed.substring(5, trimmed.length() - 1), linkCache));
                    }
                    if (trimmed.startsWith("set[") && trimmed.endsWith("]")) {
                        return new SetType(parseTypeArgument(trimmed.substring(4, trimmed.length() - 1), linkCache));
                    }
                    if (trimmed.startsWith("dict[") && trimmed.endsWith("]")) {
                        return new DictType(parseTypeArgument(trimmed.substring(5, trimmed.length() - 1), linkCache));
                    }
                    if (trimmed.startsWith("tuple[") && trimmed.endsWith("]")) {
                        var inner = trimmed.substring(6, trimmed.length() - 1);
                        var elements = splitTopLevelTypeArguments(inner, linkCache).stream()
                                .map(argument -> CapybaraTypeCompiler.parseTypeArgument(argument, linkCache))
                                .toList();
                        return new TupleType(elements);
                    }
                    var fatArrowIndex = indexOfTopLevelArrow(trimmed, "=>");
                    if (fatArrowIndex > 0) {
                        var left = trimmed.substring(0, fatArrowIndex).trim();
                        var right = trimmed.substring(fatArrowIndex + 2).trim();
                        return new FunctionType(parseTypeArgument(stripOptionalParentheses(left), linkCache), parseTypeArgument(right, linkCache));
                    }
                    var slimArrowIndex = indexOfTopLevelArrow(trimmed, "->");
                    if (slimArrowIndex > 0) {
                        var left = trimmed.substring(0, slimArrowIndex).trim();
                        var right = trimmed.substring(slimArrowIndex + 2).trim();
                        return new FunctionType(parseTypeArgument(stripOptionalParentheses(left), linkCache), parseTypeArgument(right, linkCache));
                    }
                    return new DataType(trimmed);
                });
        linkCache.parsedTypeArguments.put(raw, parsed);
        return parsed;
    }

    private static int indexOfTopLevelArrow(String value, String arrow) {
        var square = 0;
        var paren = 0;
        for (int i = 0; i < value.length() - 1; i++) {
            var ch = value.charAt(i);
            if (ch == '[') {
                square++;
                continue;
            }
            if (ch == ']') {
                square = Math.max(0, square - 1);
                continue;
            }
            if (ch == '(') {
                paren++;
                continue;
            }
            if (ch == ')') {
                paren = Math.max(0, paren - 1);
                continue;
            }
            if (square == 0 && paren == 0 && value.startsWith(arrow, i)) {
                return i;
            }
        }
        return -1;
    }

    private static String stripOptionalParentheses(String value) {
        var trimmed = value.trim();
        if (!trimmed.startsWith("(") || !trimmed.endsWith(")")) {
            return trimmed;
        }
        var inner = trimmed.substring(1, trimmed.length() - 1);
        var square = 0;
        var paren = 0;
        for (int i = 0; i < inner.length(); i++) {
            var ch = inner.charAt(i);
            if (ch == '[') {
                square++;
            } else if (ch == ']') {
                square = Math.max(0, square - 1);
            } else if (ch == '(') {
                paren++;
            } else if (ch == ')') {
                paren = Math.max(0, paren - 1);
            }
            if (square == 0 && paren == 0 && i < inner.length() - 1 && inner.charAt(i) == ',' ) {
                return trimmed;
            }
        }
        return inner.trim();
    }

    private static CompiledType instantiateTypeArguments(CompiledType linkedType, List<CompiledType> typeArguments) {
        var mappedTypeArguments = typeArguments.stream().map(CapybaraTypeCompiler::typeDescriptor).toList();
        return switch (linkedType) {
            case CompiledDataParentType parentType -> {
                var substitutions = substitutionsFor(parentType.typeParameters(), typeArguments);
                yield new CompiledDataParentType(
                        parentType.name(),
                        parentType.fields().stream()
                                .map(field -> new CompiledDataType.CompiledField(
                                        field.name(),
                                        substituteTypeParameters(field.type(), substitutions)
                                ))
                                .toList(),
                        parentType.subTypes().stream()
                                .map(subType -> (CompiledDataType) substituteTypeParameters(subType, substitutions))
                                .toList(),
                        mappedTypeArguments,
                        parentType.comments(),
                        parentType.visibility(),
                        parentType.enumType()
                );
            }
            case CompiledDataType dataType -> {
                if (dataType.typeParameters().isEmpty()) {
                    yield new CompiledDataType(
                            dataType.name(),
                            dataType.fields(),
                            mappedTypeArguments,
                            dataType.extendedTypes(),
                            dataType.comments(),
                            dataType.visibility(),
                            dataType.singleton()
                    );
                }
                var substitutions = substitutionsFor(dataType.typeParameters(), typeArguments);
                var substitutedFields = dataType.fields().stream()
                        .map(field -> new CompiledDataType.CompiledField(field.name(), substituteTypeParameters(field.type(), substitutions)))
                        .toList();
                yield new CompiledDataType(
                        dataType.name(),
                        substitutedFields,
                        dataType.typeParameters().stream()
                                .map(typeDescriptor -> substituteTypeDescriptor(typeDescriptor, substitutions))
                                .toList(),
                        dataType.extendedTypes().stream()
                                .map(typeDescriptor -> substituteTypeDescriptor(typeDescriptor, substitutions))
                                .toList(),
                        dataType.comments(),
                        dataType.visibility(),
                        dataType.singleton()
                );
            }
            default -> linkedType;
        };
    }

    private static Map<String, CompiledType> substitutionsFor(List<String> typeParameters, List<CompiledType> typeArguments) {
        var substitutions = new LinkedHashMap<String, CompiledType>();
        var max = Math.min(typeParameters.size(), typeArguments.size());
        for (int i = 0; i < max; i++) {
            substitutions.put(typeParameters.get(i), typeArguments.get(i));
        }
        return substitutions;
    }

    private static String typeDescriptor(CompiledType type) {
        return switch (type) {
            case PrimitiveLinkedType primitive -> primitive.name().toLowerCase();
            case CompiledList linkedList -> "list[" + typeDescriptor(linkedList.elementType()) + "]";
            case CompiledSet linkedSet -> "set[" + typeDescriptor(linkedSet.elementType()) + "]";
            case CompiledDict linkedDict -> "dict[" + typeDescriptor(linkedDict.valueType()) + "]";
            case CompiledTupleType linkedTupleType -> "tuple[" + linkedTupleType.elementTypes().stream()
                    .map(CapybaraTypeCompiler::typeDescriptor)
                    .collect(java.util.stream.Collectors.joining(", ")) + "]";
            case CompiledFunctionType linkedFunctionType ->
                    "(" + typeDescriptor(linkedFunctionType.argumentType()) + " => " + typeDescriptor(linkedFunctionType.returnType()) + ")";
            case CompiledDataType linkedDataType -> linkedDataType.typeParameters().isEmpty()
                    ? linkedDataType.name()
                    : linkedDataType.name() + "[" + String.join(", ", linkedDataType.typeParameters()) + "]";
            case CompiledDataParentType linkedDataParentType -> linkedDataParentType.typeParameters().isEmpty()
                    ? linkedDataParentType.name()
                    : linkedDataParentType.name() + "[" + String.join(", ", linkedDataParentType.typeParameters()) + "]";
            case CompiledGenericTypeParameter linkedGenericTypeParameter -> linkedGenericTypeParameter.name();
        };
    }

    private static CompiledType substituteTypeParameters(CompiledType type, Map<String, CompiledType> substitutions) {
        if (type instanceof CompiledGenericTypeParameter genericTypeParameter) {
            return substitutions.getOrDefault(genericTypeParameter.name(), type);
        }
        return switch (type) {
            case CompiledList linkedList -> new CompiledList(substituteTypeParameters(linkedList.elementType(), substitutions));
            case CompiledSet linkedSet -> new CompiledSet(substituteTypeParameters(linkedSet.elementType(), substitutions));
            case CompiledDict linkedDict -> new CompiledDict(substituteTypeParameters(linkedDict.valueType(), substitutions));
            case CompiledFunctionType functionType -> new CompiledFunctionType(
                    substituteTypeParameters(functionType.argumentType(), substitutions),
                    substituteTypeParameters(functionType.returnType(), substitutions)
            );
            case CompiledTupleType linkedTupleType -> new CompiledTupleType(
                    linkedTupleType.elementTypes().stream()
                            .map(elementType -> substituteTypeParameters(elementType, substitutions))
                            .toList()
            );
            case CompiledDataType linkedDataType -> new CompiledDataType(
                    linkedDataType.name(),
                    linkedDataType.fields().stream()
                            .map(field -> new CompiledDataType.CompiledField(
                                    field.name(),
                                    substituteTypeParameters(field.type(), substitutions)
                            ))
                            .toList(),
                    linkedDataType.typeParameters().stream()
                            .map(typeDescriptor -> substituteTypeDescriptor(typeDescriptor, substitutions))
                            .toList(),
                    linkedDataType.extendedTypes().stream()
                            .map(typeDescriptor -> substituteTypeDescriptor(typeDescriptor, substitutions))
                            .toList(),
                    linkedDataType.comments(),
                    linkedDataType.visibility(),
                    linkedDataType.singleton()
            );
            case CompiledDataParentType linkedDataParentType -> new CompiledDataParentType(
                    linkedDataParentType.name(),
                    linkedDataParentType.fields().stream()
                            .map(field -> new CompiledDataType.CompiledField(
                                    field.name(),
                                    substituteTypeParameters(field.type(), substitutions)
                            ))
                            .toList(),
                    linkedDataParentType.subTypes().stream()
                            .map(subType -> (CompiledDataType) substituteTypeParameters(subType, substitutions))
                            .toList(),
                    linkedDataParentType.typeParameters().stream()
                            .map(typeDescriptor -> substituteTypeDescriptor(typeDescriptor, substitutions))
                            .toList(),
                    linkedDataParentType.comments(),
                    linkedDataParentType.visibility(),
                    linkedDataParentType.enumType()
            );
            default -> type;
        };
    }

    private static String substituteTypeDescriptor(String descriptor, Map<String, CompiledType> substitutions) {
        if (descriptor == null || descriptor.isBlank()) {
            return descriptor;
        }
        var trimmed = descriptor.trim();
        var direct = substitutions.get(trimmed);
        if (direct != null) {
            return typeDescriptor(direct);
        }
        if (trimmed.startsWith("list[") && trimmed.endsWith("]")) {
            return "list[" + substituteTypeDescriptor(trimmed.substring(5, trimmed.length() - 1), substitutions) + "]";
        }
        if (trimmed.startsWith("set[") && trimmed.endsWith("]")) {
            return "set[" + substituteTypeDescriptor(trimmed.substring(4, trimmed.length() - 1), substitutions) + "]";
        }
        if (trimmed.startsWith("dict[") && trimmed.endsWith("]")) {
            return "dict[" + substituteTypeDescriptor(trimmed.substring(5, trimmed.length() - 1), substitutions) + "]";
        }
        if (trimmed.startsWith("tuple[") && trimmed.endsWith("]")) {
            var inner = trimmed.substring(6, trimmed.length() - 1);
            return "tuple[" + splitTopLevelTypeArguments(inner, new LinkCache()).stream()
                    .map(arg -> substituteTypeDescriptor(arg, substitutions))
                    .collect(java.util.stream.Collectors.joining(", ")) + "]";
        }
        var arrowIndex = indexOfTopLevelArrow(trimmed, "=>");
        if (trimmed.startsWith("(") && trimmed.endsWith(")") && arrowIndex > 0) {
            var inner = trimmed.substring(1, trimmed.length() - 1).trim();
            var innerArrow = indexOfTopLevelArrow(inner, "=>");
            if (innerArrow > 0) {
                return "(" + substituteTypeDescriptor(inner.substring(0, innerArrow).trim(), substitutions)
                        + " => " + substituteTypeDescriptor(inner.substring(innerArrow + 2).trim(), substitutions) + ")";
            }
        }
        var parsed = parseDataTypeName(trimmed, new LinkCache());
        if (!parsed.typeArguments().isEmpty()) {
            return parsed.baseName() + "[" + parsed.typeArguments().stream()
                    .map(arg -> substituteTypeDescriptor(arg, substitutions))
                    .collect(java.util.stream.Collectors.joining(", ")) + "]";
        }
        return trimmed;
    }

    private static ParsedDataTypeName parseDataTypeName(String rawName, LinkCache linkCache) {
        var cached = linkCache.parsedDataTypeNames.get(rawName);
        if (cached != null) {
            return cached;
        }
        var idx = rawName.indexOf('[');
        if (idx <= 0 || !rawName.endsWith("]")) {
            var parsed = new ParsedDataTypeName(rawName, List.of());
            linkCache.parsedDataTypeNames.put(rawName, parsed);
            return parsed;
        }
        var baseName = rawName.substring(0, idx);
        var argsContent = rawName.substring(idx + 1, rawName.length() - 1);
        var parsed = new ParsedDataTypeName(baseName, splitTopLevelTypeArguments(argsContent, linkCache));
        linkCache.parsedDataTypeNames.put(rawName, parsed);
        return parsed;
    }

    private static List<String> splitTopLevelTypeArguments(String content, LinkCache linkCache) {
        var cached = linkCache.splitTopLevelTypeArguments.get(content);
        if (cached != null) {
            return cached;
        }
        var result = new ArrayList<String>();
        var depth = 0;
        var current = new StringBuilder();
        for (var i = 0; i < content.length(); i++) {
            var ch = content.charAt(i);
            if (ch == '[') {
                depth++;
                current.append(ch);
                continue;
            }
            if (ch == ']') {
                depth = Math.max(0, depth - 1);
                current.append(ch);
                continue;
            }
            if (ch == ',' && depth == 0) {
                var token = current.toString().trim();
                if (!token.isEmpty()) {
                    result.add(token);
                }
                current.setLength(0);
                continue;
            }
            current.append(ch);
        }
        var token = current.toString().trim();
        if (!token.isEmpty()) {
            result.add(token);
        }
        var args = List.copyOf(result);
        linkCache.splitTopLevelTypeArguments.put(content, args);
        return args;
    }

    private static String matchBySimpleTypeName(String rawName, Map<String, GenericDataType> dataTypes) {
        if (rawName.startsWith("/")) {
            return null;
        }
        if (dataTypes.containsKey(rawName)) {
            return rawName;
        }
        var qualifiedPathCandidates = dataTypes.keySet().stream()
                .filter(key -> key.endsWith("/" + rawName + "." + rawName))
                .distinct()
                .sorted()
                .toList();
        if (!qualifiedPathCandidates.isEmpty()) {
            return qualifiedPathCandidates.getFirst();
        }
        var moduleQualifiedCandidates = dataTypes.keySet().stream()
                .filter(key -> key.endsWith("." + rawName))
                .distinct()
                .sorted()
                .toList();
        var pathQualifiedCandidates = moduleQualifiedCandidates.stream()
                .filter(key -> key.startsWith("/"))
                .toList();
        if (!pathQualifiedCandidates.isEmpty()) {
            return pathQualifiedCandidates.getFirst();
        }
        if (moduleQualifiedCandidates.size() == 1) {
            return moduleQualifiedCandidates.getFirst();
        }
        return null;
    }

    private static String matchQualifiedByModuleTail(String rawName, Map<String, GenericDataType> dataTypes) {
        if (!rawName.startsWith("/")) {
            return null;
        }
        var slashIdx = rawName.lastIndexOf('/');
        if (slashIdx < 0 || slashIdx >= rawName.length() - 1) {
            return null;
        }
        var moduleAndTypeTail = rawName.substring(slashIdx + 1);
        return dataTypes.keySet().stream()
                .filter(key -> key.endsWith("/" + moduleAndTypeTail))
                .findFirst()
                .orElse(null);
    }

    private static String normalizeQualifiedName(String rawName) {
        if (!rawName.startsWith("/")) {
            return null;
        }
        var dotIdx = rawName.lastIndexOf('.');
        var slashIdx = rawName.lastIndexOf('/');
        if (dotIdx < 0 || slashIdx < 0 || slashIdx >= dotIdx) {
            return null;
        }
        return rawName.substring(slashIdx + 1);
    }

    private static CompiledType withQualifiedNameIfNeeded(GenericDataType type, String requestedName) {
        if (!requestedName.contains(".") && !requestedName.startsWith("/")) {
            return type;
        }
        return switch (type) {
            case CompiledDataType linkedDataType -> new CompiledDataType(
                    requestedName,
                    linkedDataType.fields(),
                    linkedDataType.typeParameters(),
                    linkedDataType.extendedTypes(),
                    linkedDataType.comments(),
                    linkedDataType.visibility(),
                    linkedDataType.singleton()
            );
            case CompiledDataParentType linkedDataParentType -> new CompiledDataParentType(
                    requestedName,
                    linkedDataParentType.fields(),
                    linkedDataParentType.subTypes(),
                    linkedDataParentType.typeParameters(),
                    linkedDataParentType.comments(),
                    linkedDataParentType.visibility(),
                    linkedDataParentType.enumType()
            );
        };
    }

    private record ParsedDataTypeName(String baseName, List<String> typeArguments) {
    }

    private static CompiledType linkPrimitiveType(PrimitiveType primitiveType) {
        return switch (primitiveType) {
            case BYTE -> PrimitiveLinkedType.BYTE;
            case INT -> PrimitiveLinkedType.INT;
            case LONG -> PrimitiveLinkedType.LONG;
            case DOUBLE -> PrimitiveLinkedType.DOUBLE;
            case STRING -> PrimitiveLinkedType.STRING;
            case BOOL -> PrimitiveLinkedType.BOOL;
            case FLOAT -> PrimitiveLinkedType.FLOAT;
            case ANY -> PrimitiveLinkedType.ANY;
            case DATA -> PrimitiveLinkedType.DATA;
            case ENUM -> PrimitiveLinkedType.ENUM;
            case NOTHING -> PrimitiveLinkedType.NOTHING;
        };
    }

    private static Result<CompiledType> linkCollectionType(CollectionType type, Map<String, GenericDataType> dataTypes, LinkCache linkCache) {
        return switch (type) {
            case ListType list -> linkType(list.elementType(), dataTypes, linkCache).map(CompiledList::new);
            case DictType dict -> linkType(dict.valueType(), dataTypes, linkCache).map(CompiledDict::new);
            case SetType set -> linkType(set.elementType(), dataTypes, linkCache).map(CompiledSet::new);
        };
    }

    private static Result<CompiledType> linkFunctionType(FunctionType type, Map<String, GenericDataType> dataTypes, LinkCache linkCache) {
        return Result.join(
                (CompiledType argumentType, CompiledType returnType) -> new CompiledFunctionType(argumentType, returnType),
                linkType(type.argumentType(), dataTypes, linkCache),
                linkType(type.returnType(), dataTypes, linkCache)
        );
    }

    private static Result<CompiledType> linkTupleType(TupleType type, Map<String, GenericDataType> dataTypes, LinkCache linkCache) {
        return type.elementTypes().stream()
                .map(elementType -> linkType(elementType, dataTypes, linkCache))
                .collect(new ResultCollectionCollector<>())
                .map(CompiledTupleType::new);
    }
}




