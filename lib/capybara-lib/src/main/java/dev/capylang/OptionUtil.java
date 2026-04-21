package dev.capylang;

import capy.lang.Option;

import java.util.Optional;

public final class OptionUtil {
    private OptionUtil() {
    }

    public static <T> Option<T> toCapyOption(Optional<T> optional) {
        return optional.<Option<T>>map(Option.Some::new).orElseGet(OptionUtil::none);
    }

    public static <T> Optional<T> toJavaOptional(Option<T> option) {
        return switch (option) {
            case Option.Some<T> some -> Optional.ofNullable(some.value());
            case Option.None ignored -> Optional.empty();
        };
    }

    @SuppressWarnings("unchecked")
    private static <T> Option<T> none() {
        return (Option<T>) Option.None.INSTANCE;
    }
}
