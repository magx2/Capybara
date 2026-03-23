package pl.grzeslowski.capybara.compiler.parser;

public sealed interface CollectionType extends Type {
    public record ListType(Type elementType) implements CollectionType {
        @Override
        public String name() {
            return "list";
        }
    }

    public record SetType(Type elementType) implements CollectionType {

        @Override
        public String name() {
            return "set";
        }
    }

    public record DictType(Type valueType) implements CollectionType {
        @Override
        public String name() {
            return "dict";
        }
    }
}
