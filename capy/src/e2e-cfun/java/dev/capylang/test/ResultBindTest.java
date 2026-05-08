package dev.capylang.test;

import capy.lang.Result;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class ResultBindTest {
    @Test
    void usersReturnsTupleWhenAllConstructorsSucceed() {
        var result = ResultBind.users("Ann", "Bob", "Cid");

        assertThat(result).isInstanceOf(Result.Success.class);
        var value = ((Result.Success<List<Object>>) result).value();
        assertThat(value).hasSize(3);
        assertThat(((ResultBind.User) value.get(0)).name()).isEqualTo("Ann");
        assertThat(((ResultBind.User) value.get(1)).name()).isEqualTo("Bob");
        assertThat(((ResultBind.User) value.get(2)).name()).isEqualTo("Cid");
    }

    @Test
    void usersWithRegularLetKeepsFirstError() {
        assertThat(ResultBind.firstErrorMessage("", "Bob")).isEqualTo("You need to pass name");
        assertThat(ResultBind.firstErrorMessage("Ann", "")).isEqualTo("You need to pass name");
    }
}
