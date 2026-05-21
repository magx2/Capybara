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
        assertThat(interactor.create_fp_empty()).isEqualTo(ObjectOrientedFpInterop.InteropNone.INSTANCE);
        assertThat(interactor.match_fp_empty()).isEqualTo("none");
        assertThat(interactor.create_runtime_none()).isEqualTo(capy.lang.Option.None.INSTANCE);
        assertThat(interactor.invoke_rec_function(10)).isEqualTo(36);
        assertThat(interactor.echo_user_id(7)).isEqualTo(7);
        assertThat(interactor.construct_user_id(11)).isEqualTo(11);
        assertThat(interactor.make_user_id(13)).isEqualTo(13);
        assertThat(interactor.unwrap_user_id(17)).isEqualTo(17);
        assertThat(interactor.add_user_ids(19, 23)).isEqualTo(42);
        assertThat(interactor.user_id_slots(10)).hasSize(10);
        assertThat(interactor.local_user_id(29)).isEqualTo(29);
    }
}
