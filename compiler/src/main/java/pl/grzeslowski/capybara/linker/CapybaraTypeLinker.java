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
        if (dataTypes.containsKey(dataType.name())) {
            return ValueOrError.success(withQualifiedNameIfNeeded(dataTypes.get(dataType.name()), dataType.name()));
        }
        var matchedBySimpleName = matchBySimpleTypeName(dataType.name(), dataTypes);
        if (matchedBySimpleName != null) {
            return ValueOrError.success(withQualifiedNameIfNeeded(dataTypes.get(matchedBySimpleName), dataType.name()));
        }
        var matchedQualified = matchQualifiedByModuleTail(dataType.name(), dataTypes);
        if (matchedQualified != null) {
            return ValueOrError.success(withQualifiedNameIfNeeded(dataTypes.get(matchedQualified), matchedQualified));
        }
        var normalized = normalizeQualifiedName(dataType.name());
        if (normalized != null && dataTypes.containsKey(normalized)) {
            return ValueOrError.success(withQualifiedNameIfNeeded(dataTypes.get(normalized), normalized));
        }

        return ValueOrError.error("Data type \"" + dataType.name() + "\" not found");
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
