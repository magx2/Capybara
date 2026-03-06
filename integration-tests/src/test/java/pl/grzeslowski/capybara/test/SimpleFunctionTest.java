package pl.grzeslowski.capybara.test;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class SimpleFunctionTest {
    @ParameterizedTest(name = "{index}: should classify {0} as \"{1}\"")
    @CsvSource({
            "1,positive",
            "0,non-positive",
            "-1,non-positive"})
    void classify(int x, String out) {
        assertThat(SimpleFunction.classify(x)).isEqualTo(out);
    }

    @Test
    @DisplayName("should always return true")
    void alwaysTrue() {
        assertThat(SimpleFunction.alwaysTrue()).isTrue();
    }

    @Test
    @DisplayName("should add 2 numbers")
    void add() {
        assertThat(SimpleFunction.add(1, 2)).isEqualTo(3);
    }

    @Test
    @DisplayName("should subtract 2 numbers")
    void subtract() {
        assertThat(SimpleFunction.subtract(1, 2)).isEqualTo(-1);
    }

    @Test
    @DisplayName("should multiply 2 numbers")
    void multiply() {
        assertThat(SimpleFunction.multiply(5, 3)).isEqualTo(15);
    }

    @Test
    @DisplayName("should divide 2 numbers")
    void divide() {
        assertThat(SimpleFunction.divide(10, 2)).isEqualTo(5);
        assertThat(SimpleFunction.divide(9, 2)).isEqualTo(4);
    }

    @ParameterizedTest(name = "{index}: isPositive({0}) = \"{1}\"")
    @CsvSource({
            "1,true",
            "0,false",
            "-1,false",})
    void isPositive(int x, boolean out) {
        assertThat(SimpleFunction.isPositive(x)).isEqualTo(out);
    }

    @Test
    @DisplayName("should greet World")
    void greet() {
        assertThat(SimpleFunction.greet("World")).isEqualTo("Hello, World");
    }

    @ParameterizedTest(name = "{index}: should classify {0} as \"{1}\"")
    @CsvSource({
            "1,positive",
            "0,non-positive",
            "-1,non-positive"})
    void doubleClassify(int x, String out) {
        assertThat(SimpleFunction.doubleThenClassify(x)).isEqualTo(out);
    }

    @Test
    @DisplayName("orderOfExpression(3, 5) == 77")
    void orderOfExpression() {
        assertThat(SimpleFunction.orderOfExpression(3, 5)).isEqualTo(77);
    }

    @ParameterizedTest(name = "{0} ** {1} == {2}")
    @CsvSource({
            "2,3,8",
            "1,100,1",
            "0,100,0",
            "0,-100,1",
            "0,0,1",
            "-4,2,16",
            "-4,3,-64",
    })
    void power(int x, int y, int out) {
        assertThat(SimpleFunction.power(x, y)).isEqualTo(out);
    }

    @Test
    @DisplayName("should return static list")
    void staticList() {
        assertThat(SimpleFunction.staticList()).isEqualTo(List.of(1, 2, 3));
    }
}
