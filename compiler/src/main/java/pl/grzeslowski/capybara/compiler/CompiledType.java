package pl.grzeslowski.capybara.compiler;

public sealed interface CompiledType permits CollectionLinkedType, GenericDataType, PrimitiveLinkedType, CompiledGenericTypeParameter, CompiledFunctionType, CompiledTupleType {
    String name();
}
