package pl.grzeslowski.capybara.linker;

public sealed interface LinkedType permits CollectionLinkedType, GenericDataType, PrimitiveLinkedType, LinkedGenericTypeParameter, LinkedFunctionType, LinkedTupleType {
    String name();
}
