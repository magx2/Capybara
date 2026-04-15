package dev.capylang.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThatCode;
import static org.assertj.core.api.Assertions.assertThat;

class ObjectOrientedRuntimeTest {
    @Test
    void classMethodsUseConstructorStateAndInheritedBehavior() {
        var person = new Person("Capy");

        assertThat(person.greet()).isEqualTo("hello Capy");
        assertThat(person.mutable()).isEqualTo("2");
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

    @Test
    void nestedStatementBlockCanReturnFromMethod() {
        var person = new Person("Capy");

        assertThat(person.nested_label()).isEqualTo("hello Capy");
    }

    @Test
    void loopsExecuteWithExpectedControlFlow() {
        var person = new Person("Capy");

        assertThat(person.first_positive(java.util.List.of(-2, 0, 4))).isEqualTo(4);
        assertThat(person.first_positive(java.util.List.of(-2, 0))).isEqualTo(0);
        assertThat(person.first_large(java.util.List.of(3, 12, 20))).isEqualTo(12);
        assertThat(person.while_flag(true)).isEqualTo(1);
        assertThat(person.while_flag(false)).isEqualTo(0);
        assertThat(person.do_once(true)).isEqualTo(1);
        assertThat(person.do_once(false)).isEqualTo(2);
    }

    @Test
    void arrayTypesCompileAndUseJavaArraySemantics() {
        var person = new Person("Capy");
        var people = new Person[]{person, new Person("Bara")};

        assertThat(person.second_name(new String[]{"zero", "one", "two"})).isEqualTo("one");
        assertThat(person.first_id(new int[]{9, 8, 7})).isEqualTo(9);
        assertThat(person.copy_people(people)).isSameAs(people);
        assertThat(person.names()).containsExactly("zero", "one");
        assertThat(person.slots(4)).hasSize(4);
    }

    @Test
    void voidMethodsCompileAndExecuteForExpressionAndBlockBodies() throws Exception {
        var person = new Person("Capy");

        assertThat(Person.class.getMethod("ping").getReturnType()).isEqualTo(void.class);
        assertThat(Person.class.getMethod("warmup", boolean.class).getReturnType()).isEqualTo(void.class);
        assertThat(Person.class.getMethod("foreach_warm", java.util.List.class).getReturnType()).isEqualTo(void.class);
        assertThat(Person.class.getMethod("while_warm", boolean.class).getReturnType()).isEqualTo(void.class);
        assertThat(Person.class.getMethod("do_warm").getReturnType()).isEqualTo(void.class);

        assertThatCode(() -> person.ping()).doesNotThrowAnyException();
        assertThatCode(() -> person.warmup(true)).doesNotThrowAnyException();
        assertThatCode(() -> person.warmup(false)).doesNotThrowAnyException();
        assertThatCode(() -> person.foreach_warm(java.util.List.of(1, 2, 3))).doesNotThrowAnyException();
        assertThatCode(() -> person.while_warm(false)).doesNotThrowAnyException();
        assertThatCode(person::do_warm).doesNotThrowAnyException();
    }
}
