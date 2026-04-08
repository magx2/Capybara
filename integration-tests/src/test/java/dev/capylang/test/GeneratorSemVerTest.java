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

    @Test
    void localGenericOptionNoneBranchPreservesConcreteErrorType() {
        var result = GeneratorSemVer.parseLocalOptionNone();

        assertThat(result).isInstanceOf(Result.Error.class);
        assertThat(((Result.Error<Integer>) result).ex().getMessage()).isEqualTo("missing value");
    }

    @Test
    void localGenericPipeShouldPreserveConcreteTypeForWithReceiver() {
        var result = GeneratorSemVer.parseLocalSemverBuild("001");

        assertThat(result).isInstanceOf(Result.Success.class);
        assertThat(((Result.Success<String>) result).value()).isEqualTo("001");
    }

    @Test
    void chainedWithOnLocalGenericPipeShouldKeepConcreteReceiverType() {
        var result = GeneratorSemVer.parseLocalSemverBuildAndIncrement("001");

        assertThat(result).isInstanceOf(Result.Success.class);
        assertThat(((Result.Success<Integer>) result).value()).isEqualTo(2);
    }
}
