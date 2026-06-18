package dev.capylang.compiler;

import java.util.List;
import java.util.stream.Collectors;

final class TypeStrings {
    private TypeStrings() {
    }

    static String describe(CompiledType type) {
        if (type == null) {
            return "<null>";
        }
        if (type instanceof CollectionLinkedType.CompiledList list) {
            return "List[" + describe(list.elementType()) + "]";
        }
        if (type instanceof CollectionLinkedType.CompiledSet set) {
            return "Set[" + describe(set.elementType()) + "]";
        }
        if (type instanceof CollectionLinkedType.CompiledDict dict) {
            return "Dict[" + describe(dict.valueType()) + "]";
        }
        if (type instanceof CompiledFunctionType function) {
            return describe(function.argumentType()) + "=>" + describe(function.returnType());
        }
        if (type instanceof CompiledTupleType tuple) {
            return "Tuple[" + describeAll(tuple.elementTypes()) + "]";
        }
        return type.name();
    }

    static String describeAll(List<? extends CompiledType> types) {
        return types.stream()
                .map(TypeStrings::describe)
                .collect(Collectors.joining(", "));
    }
}
