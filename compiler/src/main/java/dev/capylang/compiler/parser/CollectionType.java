package dev.capylang.compiler.parser;

public sealed interface CollectionType extends Type {
    public record ListType(Type elementType) implements CollectionType {
        @Override
        public String name() {
            return "List";
        }
    }

    public record SetType(Type elementType) implements CollectionType {

        @Override
        public String name() {
            return "Set";
        }
    }

    public record DictType(Type valueType) implements CollectionType {
        @Override
        public String name() {
            return "Dict";
        }
    }
}
