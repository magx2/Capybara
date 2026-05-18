package dev.capylang.test;

import capy.lang.Result;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class PrimitiveBackedTypesTest {
    @Test
    void primitiveBackedTypesRunAsPrimitiveInts() {
        assertThat(PrimitiveBackedTypes.rawUserId(7)).isEqualTo(7);
        assertThat(PrimitiveBackedTypes.unwrapUserId(7)).isEqualTo(7);
        assertThat(PrimitiveBackedTypes.unwrapRawUserId(7)).isEqualTo(7);
        assertThat(PrimitiveBackedTypes.plusUserIds(2, 3)).isEqualTo(5);
        assertThat(PrimitiveBackedTypes.addUserIds(4, 5)).isEqualTo(9);
        assertThat(PrimitiveBackedTypes.unwrapScore(PrimitiveBackedTypes.scoreOf(11))).isEqualTo(11);
    }

    @Test
    void resultReturningConstructorRetagsPayloadType() {
        var result = PrimitiveBackedTypes.newUserId(7);

        assertThat(result).isInstanceOf(Result.Success.class);
        assertThat(((Result.Success<Integer>) result).value()).isEqualTo(7);
    }

    @Test
    void resultReturningConstructorRejectsInvalidValues() {
        var result = PrimitiveBackedTypes.newUserId(0);

        assertThat(result).isInstanceOf(Result.Error.class);
        assertThat(((Result.Error) result).ex()).hasMessage("bad user id");
    }
}
