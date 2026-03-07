package pl.grzeslowski.capybara.parser;

import java.util.Arrays;
import java.util.Optional;

public enum PrimitiveType implements Type {
    BYTE("byte"),
    INT("int"),
    LONG("long"),
    DOUBLE("double"),
    STRING("string"),
    BOOL("bool"),
    FLOAT("float"),
    NOTHING("nothing");

    private final String name;

    PrimitiveType(String name) {
        this.name = name;
    }

    public static Optional<PrimitiveType> find(String name) {
        return Arrays.stream(values()).filter(x -> x.name.equals(name)).findAny();
    }
}

