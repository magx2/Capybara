package dev.capylang.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class DeriveFeatureTest {
    @Test
    void derivesShowForDataFields() {
        assertThat(DeriveFeature.showUser()).isEqualTo("DerivedUser { name, age }");
    }

    @Test
    void derivesShowForEmptyData() {
        assertThat(DeriveFeature.showEmpty()).isEqualTo("Empty {  }");
    }

    @Test
    void derivesShowFromLinkedInheritedFields() {
        assertThat(DeriveFeature.showEmployee()).isEqualTo("Employee { id, department }");
    }

    @Test
    void derivesShowForTypeFields() {
        assertThat(DeriveFeature.showNamed()).isEqualTo("Named { id }");
    }

    @Test
    void derivesMethodWithRuntimeParameter() {
        assertThat(DeriveFeature.userBiggerThan(40)).isTrue();
        assertThat(DeriveFeature.userBiggerThan(50)).isFalse();
    }

    @Test
    void derivesMethodWithMetadataPrimitiveAndDataParameters() {
        assertThat(DeriveFeature.userMixedParameterDeriveTrue()).isTrue();
        assertThat(DeriveFeature.userMixedParameterDeriveFalse()).isFalse();
    }

    @Test
    void derivesMethodWithReflectedFieldValues() {
        assertThat(DeriveFeature.userFieldValuesSummary()).isEqualTo("name=Ada,age=42");
    }
}
