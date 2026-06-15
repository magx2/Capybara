package dev.capylang.compiler;

public sealed interface CollectionLinkedType extends CompiledType {
    public record CompiledList(CompiledType elementType) implements CollectionLinkedType {
        @Override
        public String name() {
            return "List";
        }
    }

    public record CompiledSet(CompiledType elementType) implements CollectionLinkedType {

        @Override
        public String name() {
            return "Set";
        }
    }

    public record CompiledDict(CompiledType valueType) implements CollectionLinkedType {
        @Override
        public String name() {
            return "Dict";
        }
    }
}
