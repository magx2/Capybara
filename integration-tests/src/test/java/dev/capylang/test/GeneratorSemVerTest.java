package dev.capylang.test;

import capy.lang.Result;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

@SuppressWarnings("unchecked")
class GeneratorSemVerTest {
    @Test
    void customToStringUsesCapybaraImplementation() {
        assertThat(GeneratorSemVer.customToString("capy")).isEqualTo("pretty:capy");
    }

    @Test
    void localGenericOptionTypeCompilesAndRuns() {
        var result = GeneratorSemVer.parseLocalOption(7);

        assertThat(result).isInstanceOf(Result.Success.class);
        assertThat(((Result.Success<Integer>) result).value()).isEqualTo(7);
    }
}
