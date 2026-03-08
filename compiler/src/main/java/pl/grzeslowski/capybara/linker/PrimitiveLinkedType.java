package pl.grzeslowski.capybara.linker;

import java.util.Arrays;
import java.util.Optional;

public enum PrimitiveLinkedType implements LinkedType {
    BYTE,
    INT,
    LONG,
    FLOAT,
    DOUBLE,
    STRING,
    BOOL,
    /**
     * Bottom-like type that can be assigned to any other type.
     */
    NOTHING,
    /**
     * Top type for all types
     */
    ANY,
    /**
     * Super type for all user-defined data/type declarations.
     */
    DATA;

    public static Optional<PrimitiveLinkedType> find(String name) {
        return Arrays.stream(values()).filter(x -> x.name().equals(name)).findAny();
    }


}
