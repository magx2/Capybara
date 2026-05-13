package dev.capylang.generator.java;

import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.TreeSet;

import static org.assertj.core.api.Assertions.assertThat;

class JavaInterfaceTest {
    @Test
    void shouldSortNormalAndSealedInterfacesTogether() {
        var interfaces = new TreeSet<JavaInterface>();

        interfaces.add(new JavaSealedInterface(
                new JavaType("Payment"),
                List.of(),
                List.of(),
                List.of("CardPayment"),
                List.of(),
                List.of()
        ));
        interfaces.add(new JavaNormalInterface(
                new JavaType("Auditable"),
                List.of(),
                List.of(),
                List.of()
        ));

        assertThat(interfaces)
                .extracting(javaInterface -> javaInterface.name().toString())
                .containsExactly("Auditable", "Payment");
    }
}
