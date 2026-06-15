package dev.capylang.compiler.parser;

public sealed interface Type permits DataType, PrimitiveType, CollectionType, FunctionType, TupleType {
    String name();
}

