package dev.capylang.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class SameNameObjectOrientedFpInteropTest {
    @Test
    void objectOrientedCodeCanUseFpSiblingModuleWithoutImportsWhenFileNamesMatch() {
        var interactor = new SharedInteractor();

        assertThat(interactor.invoke_fp_function("Capy")).isEqualTo("dog:Capy");
        assertThat(interactor.create_fp_data("Bara")).isInstanceOf(SharedInterop.SharedDog.class);
        assertThat(((SharedInterop.SharedDog) interactor.create_fp_data("Bara")).name()).isEqualTo("Bara");
        assertThat(interactor.match_fp_type("Mochi")).isEqualTo("dog:Mochi");
    }
}
