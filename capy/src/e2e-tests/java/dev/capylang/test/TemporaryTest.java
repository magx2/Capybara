package dev.capylang.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class TemporaryTest {
    @Test
    void generatesTopLevelTypeAndData() {
        var foo = new dev.capylang.test.Temporary.Foo("foo", 5);
        var boo = new dev.capylang.test.Temporary.Boo("boo", 7);

        assertThat(foo).isInstanceOf(dev.capylang.test.Temporary.class);
        assertThat(boo).isInstanceOf(dev.capylang.test.Temporary.class);
        assertThat(foo.fooishnes()).isEqualTo(5);
        assertThat(boo.booishnes()).isEqualTo(7);
    }
}
