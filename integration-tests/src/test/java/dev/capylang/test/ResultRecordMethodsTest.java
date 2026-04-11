package dev.capylang.test;

import capy.lang.Result;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class ResultRecordMethodsTest {
    @Test
    void recordMethodCanReturnResultOfSameRecordType() {
        var date = new ResultRecordMethods.Date(15, 4, 2026);

        var result = date.firstDayOfMonth();

        assertThat(result).isInstanceOf(Result.Success.class);
        var value = ((Result.Success<ResultRecordMethods.Date>) result).value();
        assertThat(value.day()).isEqualTo(1);
        assertThat(value.month()).isEqualTo(4);
        assertThat(value.year()).isEqualTo(2026);
    }

    @Test
    void recordMethodWithParametersCanReturnResultOfSameRecordType() {
        var date = new ResultRecordMethods.Date(15, 4, 2026);

        var result = date.withDay(20);

        assertThat(result).isInstanceOf(Result.Success.class);
        var value = ((Result.Success<ResultRecordMethods.Date>) result).value();
        assertThat(value.day()).isEqualTo(20);
        assertThat(value.month()).isEqualTo(4);
        assertThat(value.year()).isEqualTo(2026);
    }
}
