package pl.grzeslowski.capybara.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class BoolLogicTest {
    @Test
    void andLogic() {
        assertThat(BoolLogic.andLogic(true, true)).isTrue();
        assertThat(BoolLogic.andLogic(true, false)).isFalse();
        assertThat(BoolLogic.andLogic(false, true)).isFalse();
        assertThat(BoolLogic.andLogic(false, false)).isFalse();
    }

    @Test
    void orLogic() {
        assertThat(BoolLogic.orLogic(true, true)).isTrue();
        assertThat(BoolLogic.orLogic(true, false)).isTrue();
        assertThat(BoolLogic.orLogic(false, true)).isTrue();
        assertThat(BoolLogic.orLogic(false, false)).isFalse();
    }

    @Test
    void equalLogic() {
        assertThat(BoolLogic.equalLogic(true, true)).isTrue();
        assertThat(BoolLogic.equalLogic(false, false)).isTrue();
        assertThat(BoolLogic.equalLogic(true, false)).isFalse();
        assertThat(BoolLogic.equalLogic(false, true)).isFalse();
    }

    @Test
    void notEqualLogic() {
        assertThat(BoolLogic.notEqualLogic(true, true)).isFalse();
        assertThat(BoolLogic.notEqualLogic(false, false)).isFalse();
        assertThat(BoolLogic.notEqualLogic(true, false)).isTrue();
        assertThat(BoolLogic.notEqualLogic(false, true)).isTrue();
    }

    @Test
    void notLogic() {
        assertThat(BoolLogic.notLogic(true)).isFalse();
        assertThat(BoolLogic.notLogic(false)).isTrue();
    }

    @Test
    void complexLogic() {
        assertThat(BoolLogic.complexLogic(true, true)).isTrue();
        assertThat(BoolLogic.complexLogic(true, false)).isFalse();
        assertThat(BoolLogic.complexLogic(false, true)).isTrue();
        assertThat(BoolLogic.complexLogic(false, false)).isTrue();
    }
}
