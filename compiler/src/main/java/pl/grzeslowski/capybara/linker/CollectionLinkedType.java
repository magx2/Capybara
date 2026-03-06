package pl.grzeslowski.capybara.linker;

public sealed interface CollectionLinkedType extends LinkedType {
    public record LinkedList(LinkedType elementType) implements CollectionLinkedType {
        @Override
        public String name() {
            return "list";
        }
    }

    public record LinkedSet(LinkedType elementType) implements CollectionLinkedType {

        @Override
        public String name() {
            return "set";
        }
    }

    public record LinkedDict(LinkedType valueType) implements CollectionLinkedType {
        @Override
        public String name() {
            return "dict";
        }
    }
}
