package pl.grzeslowski.capybara.parser;

import java.util.Arrays;
import java.util.Optional;

public enum PrimitiveType implements Type {
    INT("int"),
    STRING("string"),
    BOOL("bool"),
    FLOAT("float");

    private final String name;

    PrimitiveType(String name) {
        this.name = name;
    }

    public static Optional<PrimitiveType> find(String name) {
        return Arrays.stream(values()).filter(x -> x.name.equals(name)).findAny();
    }
}

