package dev.capylang.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class ObjectOrientedFpInteropTest {
    @Test
    void objectOrientedCodeCanInvokeFpFunctionsCreateFpDataAndMatchFpTypes() {
        var interactor = new PetInteractor();

        assertThat(interactor.invoke_fp_function("Capy")).isEqualTo("dog:Capy");
        assertThat(interactor.create_fp_data("Bara")).isInstanceOf(ObjectOrientedFpInterop.InteropDog.class);
        assertThat(((ObjectOrientedFpInterop.InteropDog) interactor.create_fp_data("Bara")).name()).isEqualTo("Bara");
        assertThat(interactor.match_fp_type("Mochi")).isEqualTo("dog:Mochi");
    }
}
