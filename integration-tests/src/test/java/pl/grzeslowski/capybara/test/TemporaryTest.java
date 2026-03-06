package pl.grzeslowski.capybara.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class TemporaryTest {
    @Test
    void generatesTopLevelTypeAndData() {
        var foo = new pl.grzeslowski.capybara.test.Temporary.Foo("foo", 5);
        var boo = new pl.grzeslowski.capybara.test.Temporary.Boo("boo", 7);

        assertThat(foo).isInstanceOf(pl.grzeslowski.capybara.test.Temporary.class);
        assertThat(boo).isInstanceOf(pl.grzeslowski.capybara.test.Temporary.class);
        assertThat(foo.fooishnes()).isEqualTo(5);
        assertThat(boo.booishnes()).isEqualTo(7);
    }
}
