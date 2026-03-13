package pl.grzeslowski.capybara.test;

import capy.lang.Result;
import org.junit.jupiter.api.Test;
import pl.grzeslowski.capybara.CapybaraException;

import static org.assertj.core.api.Assertions.assertThat;

class ResultErrorTest {
    @Test
    void successResult() {
        var result = ResultError.successResult();
        assertThat(result).isInstanceOf(Result.Success.class);
        assertThat(((Result.Success<Integer>) result).value()).isEqualTo(7);
    }

    @Test
    void failResult() {
        var result = ResultError.failResult();
        assertThat(result).isInstanceOf(Result.Error.class);
        assertThat(((Result.Error) result).ex())
                .isInstanceOf(CapybaraException.class)
                .hasMessage("boom");
    }

    @Test
    void parseAndAddRestSizeSuccess() {
        var result = ResultError.parseAndAddRestSize("10");
        assertThat(result).isInstanceOf(Result.Success.class);
        assertThat(((Result.Success<Long>) result).value()).isEqualTo(13L);
    }

    @Test
    void parseAndAddRestSizeFail() {
        var result = ResultError.parseAndAddRestSize("abc");
        assertThat(result).isInstanceOf(Result.Error.class);
        assertThat(((Result.Error) result).ex())
                .isInstanceOf(CapybaraException.class)
                .hasMessageContaining("Cannot parse string to long");
    }
}
