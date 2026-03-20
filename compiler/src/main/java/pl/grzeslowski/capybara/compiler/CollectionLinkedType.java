package pl.grzeslowski.capybara.compiler;

public sealed interface CollectionLinkedType extends CompiledType {
    public record CompiledList(CompiledType elementType) implements CollectionLinkedType {
        @Override
        public String name() {
            return "list";
        }
    }

    public record CompiledSet(CompiledType elementType) implements CollectionLinkedType {

        @Override
        public String name() {
            return "set";
        }
    }

    public record CompiledDict(CompiledType valueType) implements CollectionLinkedType {
        @Override
        public String name() {
            return "dict";
        }
    }
}
