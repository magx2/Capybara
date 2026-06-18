package dev.capylang.compiler;

public sealed interface CollectionLinkedType extends CompiledType
        permits CollectionLinkedType.CompiledList, CollectionLinkedType.CompiledSet, CollectionLinkedType.CompiledDict {
    record CompiledList(CompiledType elementType) implements CollectionLinkedType {
        @Override
        public String name() {
            return "List";
        }

        @Override
        public String toString() {
            return "CompiledList[elementType=" + TypeStrings.describe(elementType) + "]";
        }
    }

    record CompiledSet(CompiledType elementType) implements CollectionLinkedType {
        @Override
        public String name() {
            return "Set";
        }

        @Override
        public String toString() {
            return "CompiledSet[elementType=" + TypeStrings.describe(elementType) + "]";
        }
    }

    record CompiledDict(CompiledType valueType) implements CollectionLinkedType {
        @Override
        public String name() {
            return "Dict";
        }

        @Override
        public String toString() {
            return "CompiledDict[valueType=" + TypeStrings.describe(valueType) + "]";
        }
    }
}
