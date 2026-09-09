package dev.capylang;

import org.junit.jupiter.api.Test;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.junit.jupiter.api.Assertions.assertTrue;

class GeneratedCapyCompilerTest {
    @Test
    void shouldPreserveUnexpectedCompilerFailureContext() {
        var output = new ByteArrayOutputStream();
        var failure = new IllegalStateException(
                "compilation failed",
                new IndexOutOfBoundsException("fromIndex(1) > toIndex(0)")
        );

        GeneratedCapyCompiler.writeUnexpectedFailure(failure, new PrintStream(output));

        var message = output.toString(UTF_8);
        assertTrue(message.contains("Capybara compiler failed unexpectedly. This is an internal compiler error."));
        assertTrue(message.contains("java.lang.IllegalStateException: compilation failed"));
        assertTrue(message.contains("Caused by: java.lang.IndexOutOfBoundsException: fromIndex(1) > toIndex(0)"));
        assertTrue(message.contains("at dev.capylang.GeneratedCapyCompilerTest"));
    }
}
