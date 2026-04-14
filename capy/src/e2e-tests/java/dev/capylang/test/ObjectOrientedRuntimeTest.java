package dev.capylang.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class ObjectOrientedRuntimeTest {
    @Test
    void classMethodsUseConstructorStateAndInheritedBehavior() {
        var person = new Person("Capy");

        assertThat(person.greet()).isEqualTo("hello Capy");
        assertThat(person.print()).isEqualTo("hello Capy!");
    }

    @Test
    void traitDefaultMethodSupportsBlockReturnBranches() {
        var person = new Person("Capy");

        assertThat(person.display(true)).isEqualTo("[Capy]");
        assertThat(person.display(false)).isEqualTo("Capy");
    }

    @Test
    void generatedTypesPreserveInterfaceAndTraitContracts() {
        Printable printable = new Person("Capy");
        BracketNaming naming = new Person("Capy");

        assertThat(printable.print()).isEqualTo("hello Capy!");
        assertThat(naming.bracket("Capy")).isEqualTo("[Capy]");
    }
}
