package dev.capylang.test;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.assertj.core.api.Assertions.assertThat;

class LocalFunctionDocCommentsTest {
    @ParameterizedTest
    @CsvSource({
            "0, 0",
            "1, 1",
            "5, 15"
    })
    void localFunctionDocCommentDoesNotBreakCompilationOrExecution(int input, int expected) {
        assertThat(LocalFunctionDocComments.accumulate(input)).isEqualTo(expected);
    }
}
