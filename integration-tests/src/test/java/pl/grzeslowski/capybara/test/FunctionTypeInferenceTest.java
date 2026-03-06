package pl.grzeslowski.capybara.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class FunctionTypeInferenceTest {
    @Test
    void simpleNoParamIn() {
        assertThat(FunctionTypeInference.simpleNoParamInt()).isExactlyInstanceOf(Integer.class);
    }

    @Test
    void simple() {
        assertThat(FunctionTypeInference.simple(1)).isExactlyInstanceOf(Integer.class);
    }

    @Test
    void infixOperator() {
        assertThat(FunctionTypeInference.infixOperator(1, 2)).isExactlyInstanceOf(Integer.class);
    }

    @Test
    void ifOperator() {
        assertThat(FunctionTypeInference.ifOperator(1, 2)).isExactlyInstanceOf(String.class);
    }

    @Test
    void typeSum() {
        assertThat(FunctionTypeInference.typeSum(1)).isExactlyInstanceOf(String.class);
    }
}
