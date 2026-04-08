package dev.capylang.test;

import org.junit.jupiter.api.Test;

import java.lang.reflect.Modifier;
import java.util.Arrays;

import static org.assertj.core.api.Assertions.assertThat;

class PrivateDataTest {
    @Test
    void underscorePrefixedDataCanBeUsedInsideModule() {
        assertThat(PrivateData.parseInt(7)).isEqualTo("ok:7");
        assertThat(PrivateData.parseConflict("x")).isEqualTo("internal:x");
    }

    @Test
    void underscorePrefixedDataIsGeneratedAsPrivateRecord() {
        var privateParseClass = Arrays.stream(PrivateData.class.getDeclaredClasses())
                .filter(clazz -> clazz.getSimpleName().equals("_Parse"))
                .findFirst()
                .orElseThrow();
        var parseClass = Arrays.stream(PrivateData.class.getDeclaredClasses())
                .filter(clazz -> clazz.getSimpleName().equals("Parse"))
                .findFirst()
                .orElseThrow();

        assertThat(Modifier.isPrivate(privateParseClass.getModifiers())).isTrue();
        assertThat(Modifier.isPrivate(parseClass.getModifiers())).isFalse();
    }
}
