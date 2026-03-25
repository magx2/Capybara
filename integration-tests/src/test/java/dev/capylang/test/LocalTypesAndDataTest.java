package dev.capylang.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class LocalTypesAndDataTest {
    @Test
    void localTypesAndDataInsideFunctionShouldWork() {
        assertThat(LocalTypesAndData.fooMe("foo")).isEqualTo("xyz");
        assertThat(LocalTypesAndData.fooMe("boo")).isEqualTo("zyx");
        assertThat(LocalTypesAndData.fooMe("john")).isEqualTo("john");
    }

    @Test
    void localDataShouldResolveImportedResultTypeWithoutFullyQualifiedName() {
        assertThat(LocalTypesAndData.localDataWithImportedResult(7)).isEqualTo("ok:7");
    }
}
