package pl.grzeslowski.capybara.parser;

public record Type(String name) {
    public static final Type INT = new Type("int");
    public static final Type STRING = new Type("string");
    public static final Type BOOL = new Type("bool");

}
