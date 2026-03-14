package pl.grzeslowski.capybara.test;

import capy.lang.Result;
import org.junit.jupiter.api.Test;
import pl.grzeslowski.capybara.CapybaraException;

import java.util.LinkedHashMap;

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

    @Test
    void mapUntypedResult() {
        var result = ResultError.mapUntypedResult();
        assertThat(result).isInstanceOf(Result.Success.class);
        assertThat(((Result.Success<String>) result).value()).isEqualTo("mapped_success_value");
    }

    @Test
    void flatmapUntypedResult() {
        var result = ResultError.flatmapUntypedResult();
        assertThat(result).isInstanceOf(Result.Success.class);
        assertThat(((Result.Success<String>) result).value()).isEqualTo("mapped_success_value");
    }

    @Test
    void dictReduceThenMap() {
        var dict = new LinkedHashMap<String, Integer>();
        dict.put("a", 1);
        dict.put("b", 2);

        var result = ResultError.dictReduceThenMap(dict);

        assertThat(result).isInstanceOf(Result.Success.class);
        var value = ((Result.Success<String>) result).value();
        assertThat(value).contains("a:1,").contains("b:2,").endsWith("done");
    }
}
