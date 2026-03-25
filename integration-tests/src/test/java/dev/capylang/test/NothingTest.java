package dev.capylang.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThatThrownBy;

class NothingTest {
    @Test
    void explicitReturnTypeFunctionThrowsForNothingLiteral() {
        assertThatThrownBy(() -> Nothing.nothing1(1))
                .isInstanceOf(UnsupportedOperationException.class)
                .hasMessage("line 1, column 28, file /dev/capylang/test/Nothing.cfun: the function `nothing1` is not yet implemented");
    }

    @Test
    void inferredReturnTypeFunctionThrowsForNothingLiteral() {
        assertThatThrownBy(() -> Nothing.nothing2(1))
                .isInstanceOf(UnsupportedOperationException.class)
                .hasMessage("line 3, column 23, file /dev/capylang/test/Nothing.cfun: the function `nothing2` is not yet implemented");
    }
}
