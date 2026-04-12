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

    @Test
    void sameFileCanBypassResultReturningDataConstructor() {
        assertThat(DataConstructor.validatedUserAgeOrDefault(7)).isEqualTo(7);
        assertThat(DataConstructor.validatedUserAgeOrDefault(0)).isEqualTo(1);
    }

    @Test
    void typeConstructorRunsBeforeDataConstructor() {
        assertThat(DataConstructor.validatedNamedUser("Ada", "admin")).isEqualTo("ok:Ada:admin");
        assertThat(DataConstructor.validatedNamedUser("", "admin")).isEqualTo("err:Name was empty");
        assertThat(DataConstructor.validatedNamedUser("Ada", "")).isEqualTo("err:Role was empty");
    }

    @Test
    void typeConstructorAlsoWrapsDataWithoutOwnConstructor() {
        assertThat(DataConstructor.validatedNamedGuest("Ada", "guest")).isEqualTo("ok:Ada:guest");
        assertThat(DataConstructor.validatedNamedGuest("", "guest")).isEqualTo("err:Name was empty");
    }

    @Test
    void sharedSubtypeAppliesAllParentTypeConstructors() {
        assertThat(DataConstructor.validatedProfile("Ada", "admin")).isEqualTo("ok:Ada:admin");
        assertThat(DataConstructor.validatedProfile("", "admin")).isEqualTo("err:Profile name was empty");
        assertThat(DataConstructor.validatedProfile("Ada", "")).isEqualTo("err:Profile role was empty");
    }
}
