package dev.capylang.test;

import capy.lang.Result;
import dev.capylang.PrimitiveType;
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
        assertThat(PrimitiveBackedTypes.incrementUserId(6)).isEqualTo(7);
        assertThat(PrimitiveBackedTypes.stringAt("abc", 1)).isEqualTo("b");
        assertThat(PrimitiveBackedTypes.stringGet("abc", 2)).isEqualTo("c");
        assertThat(PrimitiveBackedTypes.unwrapScore(PrimitiveBackedTypes.scoreOf(11))).isEqualTo(11);
        assertThat(PrimitiveBackedTypes.rawToken("abc")).isEqualTo("abc");
        assertThat(PrimitiveBackedTypes.unwrapToken("abc")).isEqualTo("abc");
        assertThat(PrimitiveBackedTypes.passTokenToString("abc")).isEqualTo("abc_suffix");
    }

    @Test
    void primitiveBackedTypesPreserveSourceTypeMetadataOnErasedJavaSignatures() throws Exception {
        var rawUserId = PrimitiveBackedTypes.class.getMethod("rawUserId", int.class);
        assertThat(rawUserId.getAnnotatedReturnType().getAnnotation(PrimitiveType.class).cfunType())
                .isEqualTo("/dev/capylang/test/PrimitiveBackedTypes.user_id");

        var unwrapUserId = PrimitiveBackedTypes.class.getMethod("unwrapUserId", int.class);
        assertThat(unwrapUserId.getAnnotatedParameterTypes()[0].getAnnotation(PrimitiveType.class).cfunType())
                .isEqualTo("/dev/capylang/test/PrimitiveBackedTypes.user_id");

        var scoreOf = PrimitiveBackedTypes.class.getMethod("scoreOf", int.class);
        assertThat(scoreOf.getAnnotatedReturnType().getAnnotation(PrimitiveType.class).cfunType())
                .isEqualTo("/dev/capylang/test/PrimitiveBackedTypes.score");

        var rawToken = PrimitiveBackedTypes.class.getMethod("rawToken", String.class);
        assertThat(rawToken.getAnnotatedReturnType().getAnnotation(PrimitiveType.class).cfunType())
                .isEqualTo("/dev/capylang/test/PrimitiveBackedTypes.token");

        var unwrapToken = PrimitiveBackedTypes.class.getMethod("unwrapToken", String.class);
        assertThat(unwrapToken.getAnnotatedParameterTypes()[0].getAnnotation(PrimitiveType.class).cfunType())
                .isEqualTo("/dev/capylang/test/PrimitiveBackedTypes.token");
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
