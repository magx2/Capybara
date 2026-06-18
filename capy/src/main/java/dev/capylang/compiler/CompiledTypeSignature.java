package dev.capylang.compiler;

import dev.capylang.compiler.CollectionLinkedType.CompiledDict;
import dev.capylang.compiler.CollectionLinkedType.CompiledList;
import dev.capylang.compiler.CollectionLinkedType.CompiledSet;

import java.util.List;

import static java.util.stream.Collectors.joining;

public final class CompiledTypeSignature {
    private CompiledTypeSignature() {
    }

    public static String signatureKey(String name, List<CompiledType> parameterTypes) {
        return name + "|" + parameterSignature(parameterTypes);
    }

    public static String parameterSignature(List<CompiledType> parameterTypes) {
        return parameterTypes.stream()
                .map(CompiledTypeSignature::typeKey)
                .collect(joining(","));
    }

    public static String typeKey(CompiledType type) {
        return switch (type) {
            case PrimitiveLinkedType primitive -> primitive.name();
            case CompiledList list -> "CompiledList[elementType=" + typeKey(list.elementType()) + "]";
            case CompiledSet set -> "CompiledSet[elementType=" + typeKey(set.elementType()) + "]";
            case CompiledDict dict -> "CompiledDict[valueType=" + typeKey(dict.valueType()) + "]";
            case CompiledTupleType tuple -> "CompiledTupleType[elementTypes=[" + tuple.elementTypes().stream()
                    .map(CompiledTypeSignature::typeKey)
                    .collect(joining(", ")) + "]]";
            case CompiledFunctionType function -> "CompiledFunctionType[argumentType="
                                                  + typeKey(function.argumentType())
                                                  + ", returnType="
                                                  + typeKey(function.returnType())
                                                  + "]";
            case CompiledGenericTypeParameter genericTypeParameter -> genericTypeParameter.name();
            case CompiledDataType dataType -> namedTypeKey(dataType.name(), dataType.typeParameters());
            case CompiledDataParentType parentType -> namedTypeKey(parentType.name(), parentType.typeParameters());
            case CompiledObjectType objectType -> objectType.name();
            case CompiledPrimitiveBackedType primitiveBackedType -> primitiveBackedType.name();
        };
    }

    private static String namedTypeKey(String name, List<String> typeParameters) {
        if (typeParameters.isEmpty()) {
            return name;
        }
        return name + "[" + String.join(", ", typeParameters) + "]";
    }
}
