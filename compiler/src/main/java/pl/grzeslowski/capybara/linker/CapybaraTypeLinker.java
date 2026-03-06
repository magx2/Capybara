package pl.grzeslowski.capybara.linker;

import pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedDict;
import pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedList;
import pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedSet;
import pl.grzeslowski.capybara.parser.CollectionType;
import pl.grzeslowski.capybara.parser.CollectionType.DictType;
import pl.grzeslowski.capybara.parser.CollectionType.ListType;
import pl.grzeslowski.capybara.parser.CollectionType.SetType;
import pl.grzeslowski.capybara.parser.DataType;
import pl.grzeslowski.capybara.parser.PrimitiveType;
import pl.grzeslowski.capybara.parser.Type;

import java.util.Map;


public class CapybaraTypeLinker {

    public static ValueOrError<LinkedType> linkType(Type type, Map<String, GenericDataType> dataTypes) {
        return switch (type) {
            case PrimitiveType primitiveType -> ValueOrError.success(linkPrimitiveType(primitiveType));
            case CollectionType collectionType -> linkCollectionType(collectionType);
            case DataType dataType -> linkDataType(dataType, dataTypes);
        };
    }

    @Deprecated
    public static ValueOrError<LinkedType> linkType(Type type) {
        // TODO proper mapping of types
        return linkType(type, Map.of());
    }

    private static ValueOrError<LinkedType> linkDataType(DataType dataType, Map<String, GenericDataType> dataTypes) {
        if (dataTypes.containsKey(dataType.name())) {
            return ValueOrError.success(dataTypes.get(dataType.name()));
        }

        return ValueOrError.error("Data type \"" + dataType.name() + "\" not found");
    }

    private static LinkedType linkPrimitiveType(PrimitiveType primitiveType) {
        return switch (primitiveType) {
            case INT -> PrimitiveLinkedType.INT;
            case STRING -> PrimitiveLinkedType.STRING;
            case BOOL -> PrimitiveLinkedType.BOOL;
            case FLOAT -> PrimitiveLinkedType.FLOAT;
        };
    }

    private static ValueOrError<LinkedType> linkCollectionType(CollectionType type) {
        return switch (type) {
            case ListType list -> linkType(list.elementType()).map(LinkedList::new);
            case DictType dict -> linkType(dict.valueType()).map(LinkedDict::new);
            case SetType set -> linkType(set.elementType()).map(LinkedSet::new);
        };
    }
}
