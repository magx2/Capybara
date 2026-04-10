package dev.capylang.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class DataConstructorTest {
    @Test
    void constructorCanReturnProtectedDataDirectly() {
        assertThat(DataConstructor.oddNumber(3)).isEqualTo(3);
        assertThat(DataConstructor.oddNumber(4)).isEqualTo(5);
    }

    @Test
    void constructorCanReturnResultOfProtectedData() {
        assertThat(DataConstructor.validatedUserAge(7)).isEqualTo("ok:7");
        assertThat(DataConstructor.validatedUserAge(0)).isEqualTo("err:Age has to be greater than 0. Was 0.");
    }
}
