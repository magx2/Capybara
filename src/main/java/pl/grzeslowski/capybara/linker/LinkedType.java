package pl.grzeslowski.capybara.linker;

public sealed interface LinkedType permits GenericDataType, PrimitiveLinkedType {
    String name();

}
