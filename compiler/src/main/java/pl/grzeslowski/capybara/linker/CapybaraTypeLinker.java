package pl.grzeslowski.capybara.linker;

import pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedDict;
import pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedList;
import pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedSet;
import pl.grzeslowski.capybara.parser.CollectionType;
import pl.grzeslowski.capybara.parser.CollectionType.DictType;
import pl.grzeslowski.capybara.parser.CollectionType.ListType;
import pl.grzeslowski.capybara.parser.CollectionType.SetType;
import pl.grzeslowski.capybara.parser.DataType;
import pl.grzeslowski.capybara.parser.FunctionType;
import pl.grzeslowski.capybara.parser.PrimitiveType;
import pl.grzeslowski.capybara.parser.Type;
import pl.grzeslowski.capybara.parser.TupleType;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;


public class CapybaraTypeLinker {

    public static ValueOrError<LinkedType> linkType(Type type, Map<String, GenericDataType> dataTypes) {
        return switch (type) {
            case PrimitiveType primitiveType -> ValueOrError.success(linkPrimitiveType(primitiveType));
            case CollectionType collectionType -> linkCollectionType(collectionType, dataTypes);
            case DataType dataType -> linkDataType(dataType, dataTypes);
            case FunctionType functionType -> linkFunctionType(functionType, dataTypes);
            case TupleType tupleType -> linkTupleType(tupleType, dataTypes);
        };
    }

    @Deprecated
    public static ValueOrError<LinkedType> linkType(Type type) {
        // TODO proper mapping of types
        return linkType(type, Map.of());
    }

    private static ValueOrError<LinkedType> linkDataType(DataType dataType, Map<String, GenericDataType> dataTypes) {
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

        return ValueOrError.error("Data type \"" + baseName + "\" not found");
    }

    private static ValueOrError<LinkedType> instantiateTypeArgumentsIfNeeded(
            LinkedType linkedType,
            List<String> typeArguments,
            Map<String, GenericDataType> dataTypes
    ) {
        if (typeArguments.isEmpty()) {
            return ValueOrError.success(linkedType);
        }
        return typeArguments.stream()
                .map(CapybaraTypeLinker::parseTypeArgument)
                .map(type -> linkType(type, dataTypes))
                .collect(new ValueOrErrorCollectionCollector<>())
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
                                .map(CapybaraTypeLinker::parseTypeArgument)
                                .toList();
                        return new TupleType(elements);
                    }
                    return new DataType(trimmed);
                });
    }

    private static LinkedType instantiateTypeArguments(LinkedType linkedType, List<LinkedType> typeArguments) {
        var mappedTypeArguments = typeArguments.stream().map(CapybaraTypeLinker::typeDescriptor).toList();
        return switch (linkedType) {
            case LinkedDataParentType parentType -> new LinkedDataParentType(
                    parentType.name(),
                    parentType.fields(),
                    parentType.subTypes(),
                    mappedTypeArguments
            );
            case LinkedDataType dataType -> {
                if (dataType.typeParameters().isEmpty()) {
                    yield new LinkedDataType(
                            dataType.name(),
                            dataType.fields(),
                            mappedTypeArguments,
                            dataType.extendedTypes(),
                            dataType.singleton()
                    );
                }
                var substitutions = new LinkedHashMap<String, LinkedType>();
                var max = Math.min(dataType.typeParameters().size(), typeArguments.size());
                for (int i = 0; i < max; i++) {
                    substitutions.put(dataType.typeParameters().get(i), typeArguments.get(i));
                }
                var substitutedFields = dataType.fields().stream()
                        .map(field -> new LinkedDataType.LinkedField(field.name(), substituteTypeParameters(field.type(), substitutions)))
                        .toList();
                yield new LinkedDataType(
                        dataType.name(),
                        substitutedFields,
                        mappedTypeArguments,
                        dataType.extendedTypes(),
                        dataType.singleton()
                );
            }
            default -> linkedType;
        };
    }

    private static String typeDescriptor(LinkedType type) {
        return switch (type) {
            case PrimitiveLinkedType primitive -> primitive.name().toLowerCase();
            case LinkedList linkedList -> "list[" + typeDescriptor(linkedList.elementType()) + "]";
            case LinkedSet linkedSet -> "set[" + typeDescriptor(linkedSet.elementType()) + "]";
            case LinkedDict linkedDict -> "dict[" + typeDescriptor(linkedDict.valueType()) + "]";
            case LinkedTupleType linkedTupleType -> "tuple[" + linkedTupleType.elementTypes().stream()
                    .map(CapybaraTypeLinker::typeDescriptor)
                    .collect(java.util.stream.Collectors.joining(", ")) + "]";
            case LinkedFunctionType linkedFunctionType ->
                    "(" + typeDescriptor(linkedFunctionType.argumentType()) + " -> " + typeDescriptor(linkedFunctionType.returnType()) + ")";
            case LinkedDataType linkedDataType -> linkedDataType.typeParameters().isEmpty()
                    ? linkedDataType.name()
                    : linkedDataType.name() + "[" + String.join(", ", linkedDataType.typeParameters()) + "]";
            case LinkedDataParentType linkedDataParentType -> linkedDataParentType.typeParameters().isEmpty()
                    ? linkedDataParentType.name()
                    : linkedDataParentType.name() + "[" + String.join(", ", linkedDataParentType.typeParameters()) + "]";
            case LinkedGenericTypeParameter linkedGenericTypeParameter -> linkedGenericTypeParameter.name();
        };
    }

    private static LinkedType substituteTypeParameters(LinkedType type, Map<String, LinkedType> substitutions) {
        if (type instanceof LinkedGenericTypeParameter genericTypeParameter) {
            return substitutions.getOrDefault(genericTypeParameter.name(), type);
        }
        return switch (type) {
            case LinkedList linkedList -> new LinkedList(substituteTypeParameters(linkedList.elementType(), substitutions));
            case LinkedSet linkedSet -> new LinkedSet(substituteTypeParameters(linkedSet.elementType(), substitutions));
            case LinkedDict linkedDict -> new LinkedDict(substituteTypeParameters(linkedDict.valueType(), substitutions));
            case LinkedFunctionType functionType -> new LinkedFunctionType(
                    substituteTypeParameters(functionType.argumentType(), substitutions),
                    substituteTypeParameters(functionType.returnType(), substitutions)
            );
            case LinkedTupleType linkedTupleType -> new LinkedTupleType(
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

    private static LinkedType withQualifiedNameIfNeeded(GenericDataType type, String requestedName) {
        if (!requestedName.contains(".")) {
            return type;
        }
        return switch (type) {
            case LinkedDataType linkedDataType -> new LinkedDataType(
                    requestedName,
                    linkedDataType.fields(),
                    linkedDataType.typeParameters(),
                    linkedDataType.extendedTypes(),
                    linkedDataType.singleton()
            );
            case LinkedDataParentType linkedDataParentType -> new LinkedDataParentType(
                    requestedName,
                    linkedDataParentType.fields(),
                    linkedDataParentType.subTypes(),
                    linkedDataParentType.typeParameters()
            );
        };
    }

    private record ParsedDataTypeName(String baseName, List<String> typeArguments) {
    }

    private static LinkedType linkPrimitiveType(PrimitiveType primitiveType) {
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

    private static ValueOrError<LinkedType> linkCollectionType(CollectionType type, Map<String, GenericDataType> dataTypes) {
        return switch (type) {
            case ListType list -> linkType(list.elementType(), dataTypes).map(LinkedList::new);
            case DictType dict -> linkType(dict.valueType(), dataTypes).map(LinkedDict::new);
            case SetType set -> linkType(set.elementType(), dataTypes).map(LinkedSet::new);
        };
    }

    private static ValueOrError<LinkedType> linkFunctionType(FunctionType type, Map<String, GenericDataType> dataTypes) {
        return ValueOrError.join(
                (LinkedType argumentType, LinkedType returnType) -> new LinkedFunctionType(argumentType, returnType),
                linkType(type.argumentType(), dataTypes),
                linkType(type.returnType(), dataTypes)
        );
    }

    private static ValueOrError<LinkedType> linkTupleType(TupleType type, Map<String, GenericDataType> dataTypes) {
        return type.elementTypes().stream()
                .map(elementType -> linkType(elementType, dataTypes))
                .collect(new ValueOrErrorCollectionCollector<>())
                .map(LinkedTupleType::new);
    }
}

