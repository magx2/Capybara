package dev.capylang.test;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class EnumDataFieldTest {
    @Test
    void enumCanBeUsedAsDataFieldType() {
        var root = new EnumDataField.Path(EnumDataField.PathRoot.ABSOLUTE, List.of());
        var relative = new EnumDataField.Path(EnumDataField.PathRoot.RELATIVE, List.of("home"));

        assertThat(root.root()).isEqualTo(EnumDataField.PathRoot.ABSOLUTE);
        assertThat(relative.root()).isEqualTo(EnumDataField.PathRoot.RELATIVE);
        assertThat(root.segments()).isEmpty();
    }

    @Test
    void enumFieldCanBeMatchedAndRead() {
        var root = new EnumDataField.Path(EnumDataField.PathRoot.ABSOLUTE, List.of());
        var relative = new EnumDataField.Path(EnumDataField.PathRoot.RELATIVE, List.of());

        assertThat(EnumDataField.rootName(root)).isEqualTo("ABSOLUTE");
        assertThat(EnumDataField.isAbsolute(root)).isTrue();
        assertThat(EnumDataField.isAbsolute(relative)).isFalse();
    }
}
