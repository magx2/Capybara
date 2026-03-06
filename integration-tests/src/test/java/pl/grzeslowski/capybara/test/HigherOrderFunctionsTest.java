package pl.grzeslowski.capybara.test;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class HigherOrderFunctionsTest {
    @Test
    void mapWithFunctionReference() {
        assertThat(HigherOrderFunctions.map(List.of(1, 2, 3)))
                .isEqualTo(List.of(1, 4, 9));
    }

    @Test
    void invokesHigherOrderFunction() {
        assertThat(HigherOrderFunctions.hof(x -> "n=" + x))
                .isEqualTo("n=5");
    }

    @Test
    void invokesHigherOrderFunctionWithLambdaCapture() {
        assertThat(HigherOrderFunctions.invokeHof("Hello"))
                .isEqualTo("Hello 5");
    }
}
