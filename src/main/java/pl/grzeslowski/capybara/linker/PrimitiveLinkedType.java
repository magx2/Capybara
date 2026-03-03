package pl.grzeslowski.capybara.linker;

import pl.grzeslowski.capybara.parser.Type;

public record PrimitiveLinkedType(String name) implements LinkedType {
    public static final PrimitiveLinkedType INT = new PrimitiveLinkedType("int");
    public static final PrimitiveLinkedType STRING = new PrimitiveLinkedType("string");
    public static final PrimitiveLinkedType BOOL = new PrimitiveLinkedType("bool");
    public static final PrimitiveLinkedType FLOAT = new PrimitiveLinkedType("float");
}
