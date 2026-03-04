package pl.grzeslowski.capybara.linker;

import java.util.Arrays;
import java.util.Optional;

public enum PrimitiveLinkedType implements LinkedType {
    INT,
    FLOAT,
    STRING,
    BOOL,
    /**
     * Top type for all types
     */
    ANY;

    public static Optional<PrimitiveLinkedType> find(String name) {
        return Arrays.stream(values()).filter(x -> x.name().equals(name)).findAny();
    }


}
