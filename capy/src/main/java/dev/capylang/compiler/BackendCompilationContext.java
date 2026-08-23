package dev.capylang.compiler;

import java.util.Optional;

/** Carries the selected compile-generate target through compiler validation. */
public final class BackendCompilationContext {
    private static final ThreadLocal<String> OUTPUT_TYPE = new ThreadLocal<>();
    private static final ThreadLocal<Object> GENERATION_INVOCATION = new ThreadLocal<>();

    private BackendCompilationContext() {
    }

    public static Optional<String> outputType() {
        return Optional.ofNullable(OUTPUT_TYPE.get());
    }

    public static Optional<Object> generationInvocation() {
        return Optional.ofNullable(GENERATION_INVOCATION.get());
    }

    public static void withOutputType(String outputType, Runnable action) {
        var previous = OUTPUT_TYPE.get();
        var previousInvocation = GENERATION_INVOCATION.get();
        try {
            OUTPUT_TYPE.set(outputType);
            GENERATION_INVOCATION.set(new Object());
            action.run();
        } finally {
            if (previous == null) {
                OUTPUT_TYPE.remove();
            } else {
                OUTPUT_TYPE.set(previous);
            }
            if (previousInvocation == null) {
                GENERATION_INVOCATION.remove();
            } else {
                GENERATION_INVOCATION.set(previousInvocation);
            }
        }
    }
}
