package dev.capylang.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.within;

class ConstsTest {
    @Test
    void piPlusE() {
        assertThat(Consts.piPlusE()).isCloseTo(5.14d, within(0.001d));
    }

    @Test
    void usesPrivateConst() {
        assertThat(Consts.usesPrivateConst()).isEqualTo(12);
    }

    @Test
    void localConsts() {
        assertThat(Consts.localConsts(3)).isEqualTo(18);
    }

    @Test
    void localPrivateNamedConst() {
        assertThat(Consts.localPrivateNamedConst(" ")).isTrue();
        assertThat(Consts.localPrivateNamedConst("x")).isFalse();
    }

    @Test
    void primitiveConsts() {
        assertThat(Consts.primitivesSummary())
                .isEqualTo("26:42:97387717187:1.5:2.5:true:const");
    }

    @Test
    void collectionConsts() {
        assertThat(Consts.listConstSum()).isEqualTo(6);
        assertThat(Consts.setConstHasA()).isTrue();
        assertThat(Consts.dictConstSize()).isEqualTo(2);
    }

    @Test
    void dataAndTypeConsts() {
        assertThat(Consts.dogDataName()).isEqualTo("Rex");
        assertThat(Consts.animalTypeText()).isEqualTo("cat:7");
    }
}
