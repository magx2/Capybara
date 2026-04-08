package dev.capylang.test;

import capy.lang.Result;
import org.junit.jupiter.api.Test;
import dev.capylang.CapybaraException;

import static org.assertj.core.api.Assertions.assertThat;

@SuppressWarnings("unchecked")
class StringCollectionTest {
    @Test
    void contains() {
        assertThat(StringCollection.contains("capybara", "pyb")).isTrue();
        assertThat(StringCollection.contains("capybara", "dog")).isFalse();
    }

    @Test
    void containsMethod() {
        assertThat(StringCollection.containsMethod("capybara", "pyb")).isTrue();
        assertThat(StringCollection.containsMethod("capybara", "dog")).isFalse();
    }

    @Test
    void containsEmptySubstring() {
        assertThat(StringCollection.contains("capybara", "")).isTrue();
        assertThat(StringCollection.contains("", "")).isTrue();
    }

    @Test
    void startsWithMethod() {
        assertThat(StringCollection.startsWithMethod("capybara", "cap")).isTrue();
        assertThat(StringCollection.startsWithMethod("capybara", "bara")).isFalse();
    }

    @Test
    void endWithMethod() {
        assertThat(StringCollection.endWithMethod("capybara", "bara")).isTrue();
        assertThat(StringCollection.endWithMethod("capybara", "cap")).isFalse();
    }

    @Test
    void trimMethod() {
        assertThat(StringCollection.trimMethod("  capybara  ")).isEqualTo("capybara");
        assertThat(StringCollection.trimMethod("capybara")).isEqualTo("capybara");
    }

    @Test
    void replaceMethod() {
        assertThat(StringCollection.replaceMethod("capybara", "bara", "lang")).isEqualTo("capylang");
        assertThat(StringCollection.replaceMethod("aaaa", "a", "b")).isEqualTo("bbbb");
        assertThat(StringCollection.replaceMethod("capybara", "dog", "cat")).isEqualTo("capybara");
    }

    @Test
    void isEmptyMethod() {
        assertThat(StringCollection.isEmptyMethod("")).isTrue();
        assertThat(StringCollection.isEmptyMethod("capybara")).isFalse();
    }

    @Test
    void notIsEmptyMethod() {
        assertThat(StringCollection.notIsEmptyMethod("")).isFalse();
        assertThat(StringCollection.notIsEmptyMethod("capybara")).isTrue();
    }

    @Test
    void notNestedIsEmpty() {
        assertThat(StringCollection.notNestedIsEmpty(new StringCollection.BufferHolder(""))).isFalse();
        assertThat(StringCollection.notNestedIsEmpty(new StringCollection.BufferHolder("capybara"))).isTrue();
    }

    @Test
    void notTrimmedIsEmpty() {
        assertThat(StringCollection.notTrimmedIsEmpty("   ")).isFalse();
        assertThat(StringCollection.notTrimmedIsEmpty(" capybara ")).isTrue();
    }

    @Test
    void size() {
        assertThat(StringCollection.size("capybara")).isEqualTo(8);
        assertThat(StringCollection.size("")).isZero();
    }

    @Test
    void toIntMethod() {
        var success = StringCollection.toIntMethod("123");
        assertThat(success).isInstanceOf(Result.Success.class);
        assertThat(((Result.Success<Integer>) success).value()).isEqualTo(123);

        var error = StringCollection.toIntMethod("abc");
        assertThat(error).isInstanceOf(Result.Error.class);
        assertThat(((Result.Error) error).ex())
                .isInstanceOf(CapybaraException.class)
                .hasMessage("Cannot parse string to int: abc");
    }

    @Test
    void toLongMethod() {
        var success = StringCollection.toLongMethod("12345678901");
        assertThat(success).isInstanceOf(Result.Success.class);
        assertThat(((Result.Success<Long>) success).value()).isEqualTo(12_345_678_901L);

        var error = StringCollection.toLongMethod("abc");
        assertThat(error).isInstanceOf(Result.Error.class);
        assertThat(((Result.Error) error).ex())
                .isInstanceOf(CapybaraException.class)
                .hasMessage("Cannot parse string to long: abc");
    }

    @Test
    void toDoubleMethod() {
        var success = StringCollection.toDoubleMethod("12.5");
        assertThat(success).isInstanceOf(Result.Success.class);
        assertThat(((Result.Success<Double>) success).value()).isEqualTo(12.5);

        var error = StringCollection.toDoubleMethod("abc");
        assertThat(error).isInstanceOf(Result.Error.class);
        assertThat(((Result.Error) error).ex())
                .isInstanceOf(CapybaraException.class)
                .hasMessage("Cannot parse string to double: abc");
    }

    @Test
    void toFloatMethod() {
        var success = StringCollection.toFloatMethod("1.25");
        assertThat(success).isInstanceOf(Result.Success.class);
        assertThat(((Result.Success<Float>) success).value()).isEqualTo(1.25f);

        var error = StringCollection.toFloatMethod("abc");
        assertThat(error).isInstanceOf(Result.Error.class);
        assertThat(((Result.Error) error).ex())
                .isInstanceOf(CapybaraException.class)
                .hasMessage("Cannot parse string to float: abc");
    }

    @Test
    void toBoolMethod() {
        var successTrue = StringCollection.toBoolMethod("true");
        assertThat(successTrue).isInstanceOf(Result.Success.class);
        assertThat(((Result.Success<Boolean>) successTrue).value()).isTrue();

        var successFalse = StringCollection.toBoolMethod("false");
        assertThat(successFalse).isInstanceOf(Result.Success.class);
        assertThat(((Result.Success<Boolean>) successFalse).value()).isFalse();

        var error = StringCollection.toBoolMethod("abc");
        assertThat(error).isInstanceOf(Result.Error.class);
        assertThat(((Result.Error) error).ex())
                .isInstanceOf(CapybaraException.class)
                .hasMessage("Cannot parse string to bool: abc");
    }
}

