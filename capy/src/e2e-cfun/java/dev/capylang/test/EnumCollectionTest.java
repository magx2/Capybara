package dev.capylang.test;

import capy.lang.Result;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class EnumCollectionTest {
    @Test
    void matchCanCoverAllEnumValues() {
        assertThat(EnumCollection.colorMatch(EnumCollection.Color.RED)).isEqualTo(1);
        assertThat(EnumCollection.colorMatch(EnumCollection.Color.BLUE)).isEqualTo(2);
        assertThat(EnumCollection.colorMatch(EnumCollection.Color.GREEN_BLUE)).isEqualTo(3);
        assertThat(EnumCollection.colorMatch(EnumCollection.Color.YELLOW)).isEqualTo(4);
        assertThat(EnumCollection.colorMatch(EnumCollection.Color.BLACK)).isEqualTo(5);
        assertThat(EnumCollection.colorMatch(EnumCollection.Color.WHITE)).isEqualTo(6);
    }

    @Test
    void matchCanUseWildcardForEnums() {
        assertThat(EnumCollection.colorMatchWildcard(EnumCollection.Color.RED)).isEqualTo(1);
        assertThat(EnumCollection.colorMatchWildcard(EnumCollection.Color.BLUE)).isEqualTo(2);
        assertThat(EnumCollection.colorMatchWildcard(EnumCollection.Color.WHITE)).isEqualTo(0);
    }

    @Test
    void enumProvidesOrderAndName() {
        assertThat(EnumCollection.colorOrder(EnumCollection.Color.RED)).isEqualTo(0);
        assertThat(EnumCollection.colorOrder(EnumCollection.Color.GREEN_BLUE)).isEqualTo(2);
        assertThat(EnumCollection.colorName(EnumCollection.Color.GREEN_BLUE)).isEqualTo("GREEN_BLUE");
    }

    @Test
    void valuesReturnsEnumSet() {
        assertThat(EnumCollection.colorValues())
                .containsExactlyInAnyOrder(EnumCollection.Color.values());
    }

    @Test
    void parseFromStringAndOrderReturnsResult() {
        assertThat(EnumCollection.parseColorName("BLUE"))
                .isInstanceOf(Result.Success.class);
        assertThat(((Result.Success<EnumCollection.Color>) EnumCollection.parseColorName("BLUE")).value())
                .isEqualTo(EnumCollection.Color.BLUE);

        assertThat(EnumCollection.parseColorOrder(3))
                .isInstanceOf(Result.Success.class);
        assertThat(((Result.Success<EnumCollection.Color>) EnumCollection.parseColorOrder(3)).value())
                .isEqualTo(EnumCollection.Color.YELLOW);

        assertThat(EnumCollection.parseColorName("UNKNOWN"))
                .isInstanceOf(Result.Error.class);
        assertThat(EnumCollection.parseColorOrder(99))
                .isInstanceOf(Result.Error.class);
    }

    @Test
    void enumValuesCanBeUsedWithoutBracesInExpressions() {
        assertThat(EnumCollection.qualifiedColorWithoutBraces())
                .isEqualTo(EnumCollection.Color.BLUE);
        assertThat(EnumCollection.unqualifiedColorWithoutBraces())
                .isEqualTo(EnumCollection.Color.GREEN_BLUE);
        assertThat(EnumCollection.inferredColorWithoutBraces())
                .isEqualTo(EnumCollection.Color.RED);
        assertThat(EnumCollection.colorNameWithoutBraces())
                .isEqualTo("YELLOW");
    }

    @Test
    void enumValuesCanBeUsedWithoutBracesInCompoundExpressions() {
        assertThat(EnumCollection.colorListWithoutBraces())
                .containsExactly(
                        EnumCollection.Color.RED,
                        EnumCollection.Color.BLUE,
                        EnumCollection.Color.GREEN_BLUE);

        var palette = EnumCollection.paletteWithoutBraces();
        assertThat(palette.primary()).isEqualTo(EnumCollection.Color.WHITE);
        assertThat(palette.fallback()).isEqualTo(EnumCollection.Color.BLACK);
    }
}
