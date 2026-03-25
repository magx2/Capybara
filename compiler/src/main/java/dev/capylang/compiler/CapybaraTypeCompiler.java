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
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;


public class CapybaraTypeCompiler {

    public static Result<CompiledType> linkType(Type type, Map<String, GenericDataType> dataTypes) {
        return switch (type) {
            case PrimitiveType primitiveType -> Result.success(linkPrimitiveType(primitiveType));
            case CollectionType collectionType -> linkCollectionType(collectionType, dataTypes);
            case DataType dataType -> linkDataType(dataType, dataTypes);
            case FunctionType functionType -> linkFunctionType(functionType, dataTypes);
            case TupleType tupleType -> linkTupleType(tupleType, dataTypes);
        };
    }

    @Deprecated
    public static Result<CompiledType> linkType(Type type) {
        // TODO proper mapping of types
        return linkType(type, Map.of());
    }

    private static Result<CompiledType> linkDataType(DataType dataType, Map<String, GenericDataType> dataTypes) {
        var parsedName = parseDataTypeName(dataType.name());
        var baseName = parsedName.baseName();
        if (dataTypes.containsKey(baseName)) {
            return instantiateTypeArgumentsIfNeeded(withQualifiedNameIfNeeded(dataTypes.get(baseName), baseName), parsedName.typeArguments(), dataTypes);
        }
        var matchedBySimpleName = matchBySimpleTypeName(baseName, dataTypes);
        if (matchedBySimpleName != null) {
            return instantiateTypeArgumentsIfNeeded(withQualifiedNameIfNeeded(dataTypes.get(matchedBySimpleName), baseName), parsedName.typeArguments(), dataTypes);
        }
        var matchedQualified = matchQualifiedByModuleTail(baseName, dataTypes);
        if (matchedQualified != null) {
            return instantiateTypeArgumentsIfNeeded(withQualifiedNameIfNeeded(dataTypes.get(matchedQualified), matchedQualified), parsedName.typeArguments(), dataTypes);
        }
        var normalized = normalizeQualifiedName(baseName);
        if (normalized != null && dataTypes.containsKey(normalized)) {
            return instantiateTypeArgumentsIfNeeded(withQualifiedNameIfNeeded(dataTypes.get(normalized), normalized), parsedName.typeArguments(), dataTypes);
        }

        return Result.error("Data type \"" + baseName + "\" not found");
    }

    private static Result<CompiledType> instantiateTypeArgumentsIfNeeded(
            CompiledType linkedType,
            List<String> typeArguments,
            Map<String, GenericDataType> dataTypes
    ) {
        if (typeArguments.isEmpty()) {
            return Result.success(linkedType);
        }
        return typeArguments.stream()
                .map(CapybaraTypeCompiler::parseTypeArgument)
                .map(type -> linkType(type, dataTypes))
                .collect(new ResultCollectionCollector<>())
                .map(linkedTypeArguments -> instantiateTypeArguments(linkedType, linkedTypeArguments));
    }

    private static Type parseTypeArgument(String raw) {
        var trimmed = raw.trim();
        return PrimitiveType.find(trimmed)
                .map(Type.class::cast)
                .orElseGet(() -> {
                    if (trimmed.startsWith("list[") && trimmed.endsWith("]")) {
                        return new ListType(parseTypeArgument(trimmed.substring(5, trimmed.length() - 1)));
                    }
                    if (trimmed.startsWith("set[") && trimmed.endsWith("]")) {
                        return new SetType(parseTypeArgument(trimmed.substring(4, trimmed.length() - 1)));
                    }
                    if (trimmed.startsWith("dict[") && trimmed.endsWith("]")) {
                        return new DictType(parseTypeArgument(trimmed.substring(5, trimmed.length() - 1)));
                    }
                    if (trimmed.startsWith("tuple[") && trimmed.endsWith("]")) {
                        var inner = trimmed.substring(6, trimmed.length() - 1);
                        var elements = splitTopLevelTypeArguments(inner).stream()
                                .map(CapybaraTypeCompiler::parseTypeArgument)
                                .toList();
                        return new TupleType(elements);
                    }
                    var fatArrowIndex = indexOfTopLevelArrow(trimmed, "=>");
                    if (fatArrowIndex > 0) {
                        var left = trimmed.substring(0, fatArrowIndex).trim();
                        var right = trimmed.substring(fatArrowIndex + 2).trim();
                        return new FunctionType(parseTypeArgument(stripOptionalParentheses(left)), parseTypeArgument(right));
                    }
                    var slimArrowIndex = indexOfTopLevelArrow(trimmed, "->");
                    if (slimArrowIndex > 0) {
                        var left = trimmed.substring(0, slimArrowIndex).trim();
                        var right = trimmed.substring(slimArrowIndex + 2).trim();
                        return new FunctionType(parseTypeArgument(stripOptionalParentheses(left)), parseTypeArgument(right));
                    }
                    return new DataType(trimmed);
                });
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
            case CompiledDataParentType parentType -> new CompiledDataParentType(
                    parentType.name(),
                    parentType.fields(),
                    parentType.subTypes(),
                    mappedTypeArguments,
                    parentType.visibility(),
                    parentType.enumType()
            );
            case CompiledDataType dataType -> {
                if (dataType.typeParameters().isEmpty()) {
                    yield new CompiledDataType(
                            dataType.name(),
                            dataType.fields(),
                            mappedTypeArguments,
                            dataType.extendedTypes(),
                            dataType.visibility(),
                            dataType.singleton()
                    );
                }
                var substitutions = new LinkedHashMap<String, CompiledType>();
                var max = Math.min(dataType.typeParameters().size(), typeArguments.size());
                for (int i = 0; i < max; i++) {
                    substitutions.put(dataType.typeParameters().get(i), typeArguments.get(i));
                }
                var substitutedFields = dataType.fields().stream()
                        .map(field -> new CompiledDataType.CompiledField(field.name(), substituteTypeParameters(field.type(), substitutions)))
                        .toList();
                yield new CompiledDataType(
                        dataType.name(),
                        substitutedFields,
                        mappedTypeArguments,
                        dataType.extendedTypes(),
                        dataType.visibility(),
                        dataType.singleton()
                );
            }
            default -> linkedType;
        };
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
            default -> type;
        };
    }

    private static ParsedDataTypeName parseDataTypeName(String rawName) {
        var idx = rawName.indexOf('[');
        if (idx <= 0 || !rawName.endsWith("]")) {
            return new ParsedDataTypeName(rawName, List.of());
        }
        var baseName = rawName.substring(0, idx);
        var argsContent = rawName.substring(idx + 1, rawName.length() - 1);
        return new ParsedDataTypeName(baseName, splitTopLevelTypeArguments(argsContent));
    }

    private static List<String> splitTopLevelTypeArguments(String content) {
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
        return List.copyOf(result);
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
                    linkedDataType.visibility(),
                    linkedDataType.singleton()
            );
            case CompiledDataParentType linkedDataParentType -> new CompiledDataParentType(
                    requestedName,
                    linkedDataParentType.fields(),
                    linkedDataParentType.subTypes(),
                    linkedDataParentType.typeParameters(),
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
            case NOTHING -> PrimitiveLinkedType.NOTHING;
        };
    }

    private static Result<CompiledType> linkCollectionType(CollectionType type, Map<String, GenericDataType> dataTypes) {
        return switch (type) {
            case ListType list -> linkType(list.elementType(), dataTypes).map(CompiledList::new);
            case DictType dict -> linkType(dict.valueType(), dataTypes).map(CompiledDict::new);
            case SetType set -> linkType(set.elementType(), dataTypes).map(CompiledSet::new);
        };
    }

    private static Result<CompiledType> linkFunctionType(FunctionType type, Map<String, GenericDataType> dataTypes) {
        return Result.join(
                (CompiledType argumentType, CompiledType returnType) -> new CompiledFunctionType(argumentType, returnType),
                linkType(type.argumentType(), dataTypes),
                linkType(type.returnType(), dataTypes)
        );
    }

    private static Result<CompiledType> linkTupleType(TupleType type, Map<String, GenericDataType> dataTypes) {
        return type.elementTypes().stream()
                .map(elementType -> linkType(elementType, dataTypes))
                .collect(new ResultCollectionCollector<>())
                .map(CompiledTupleType::new);
    }
}


