package dev.capylang.test;

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
    void notFieldAccess() {
        assertThat(BoolLogic.notFieldAccess(new BoolLogic.BoolBox(true))).isFalse();
        assertThat(BoolLogic.notFieldAccess(new BoolLogic.BoolBox(false))).isTrue();
    }

    @Test
    void notMethodCall() {
        assertThat(BoolLogic.notMethodCall(new BoolLogic.BoolBox(true))).isFalse();
        assertThat(BoolLogic.notMethodCall(new BoolLogic.BoolBox(false))).isTrue();
    }

    @Test
    void complexLogic() {
        assertThat(BoolLogic.complexLogic(true, true)).isTrue();
        assertThat(BoolLogic.complexLogic(true, false)).isFalse();
        assertThat(BoolLogic.complexLogic(false, true)).isTrue();
        assertThat(BoolLogic.complexLogic(false, false)).isTrue();
    }

    @Test
    void numberToBool() {
        assertThat(BoolLogic.byteToBool((byte) 0x00)).isFalse();
        assertThat(BoolLogic.byteToBool((byte) 0x01)).isTrue();
        assertThat(BoolLogic.intToBool(0)).isFalse();
        assertThat(BoolLogic.intToBool(2)).isTrue();
        assertThat(BoolLogic.longToBool(0L)).isFalse();
        assertThat(BoolLogic.longToBool(2L)).isTrue();
        assertThat(BoolLogic.floatToBool(0f)).isFalse();
        assertThat(BoolLogic.floatToBool(0.5f)).isTrue();
        assertThat(BoolLogic.doubleToBool(0d)).isFalse();
        assertThat(BoolLogic.doubleToBool(0.5d)).isTrue();
    }

    @Test
    void stringAndCollectionsToBool() {
        assertThat(BoolLogic.stringToBool("")).isFalse();
        assertThat(BoolLogic.stringToBool("x")).isTrue();
        assertThat(BoolLogic.listToBool(java.util.List.of())).isFalse();
        assertThat(BoolLogic.listToBool(java.util.List.of(1))).isTrue();
        assertThat(BoolLogic.setToBool(java.util.Set.of())).isFalse();
        assertThat(BoolLogic.setToBool(java.util.Set.of(1))).isTrue();
        assertThat(BoolLogic.dictToBool(java.util.Map.of())).isFalse();
        assertThat(BoolLogic.dictToBool(java.util.Map.of("a", 1))).isTrue();
    }

    @Test
    void logicalOperatorsWithAutoBoolCoercion() {
        assertThat(BoolLogic.andInt(0, 1)).isFalse();
        assertThat(BoolLogic.andInt(2, 1)).isTrue();
        assertThat(BoolLogic.orString("", "")).isFalse();
        assertThat(BoolLogic.orString("x", "")).isTrue();
    }

    @Test
    void notWithAutoBoolCoercion() {
        assertThat(BoolLogic.notInt(0)).isTrue();
        assertThat(BoolLogic.notInt(3)).isFalse();
        assertThat(BoolLogic.notList(java.util.List.of())).isTrue();
        assertThat(BoolLogic.notList(java.util.List.of(1))).isFalse();
    }
}
