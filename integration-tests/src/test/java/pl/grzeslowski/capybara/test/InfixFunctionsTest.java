package pl.grzeslowski.capybara.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class InfixFunctionsTest {
    @Test
    void methodStyleFunctionWithThis() {
        var sum = new InfixFunctions.Sum(10);
        assertThat(InfixFunctions.add(sum, 5).value()).isEqualTo(15);
    }
}
