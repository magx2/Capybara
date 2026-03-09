package pl.grzeslowski.capybara.parser;

public sealed interface Type permits DataType, PrimitiveType, CollectionType, FunctionType, TupleType {
    String name();
}

