package dev.capylang.compiler;

import java.util.Optional;

/** Carries the selected compile-generate target through compiler validation. */
public final class BackendCompilationContext {
    private static final ThreadLocal<String> OUTPUT_TYPE = new ThreadLocal<>();

    private BackendCompilationContext() {
    }

    public static Optional<String> outputType() {
        return Optional.ofNullable(OUTPUT_TYPE.get());
    }

    public static void withOutputType(String outputType, Runnable action) {
        var previous = OUTPUT_TYPE.get();
        try {
            OUTPUT_TYPE.set(outputType);
            action.run();
        } finally {
            if (previous == null) {
                OUTPUT_TYPE.remove();
            } else {
                OUTPUT_TYPE.set(previous);
            }
        }
    }
}
