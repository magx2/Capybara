package dev.capylang.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class StringInterpolationTest {
    @Test
    void simpleInterpolation() {
        assertThat(StringInterpolation.simple("Capybara")).isEqualTo("Hello Capybara");
    }

    @Test
    void expressionInterpolation() {
        assertThat(StringInterpolation.expression("Capy", 7)).isEqualTo("Hello Capy7");
    }

    @Test
    void escapedInterpolationMarkersStayLiteral() {
        assertThat(StringInterpolation.escaped("ignored")).isEqualTo("Hello \\{}");
    }

    @Test
    void quoteEscapeInterpolation() {
        assertThat(StringInterpolation.quoteEscape(new StringInterpolation.Foo("Martin"))).isEqualTo("Hello \"Martin\"");
    }

    @Test
    void dataFieldInterpolation() {
        assertThat(StringInterpolation.dataField(new StringInterpolation.Foo("Martin"))).isEqualTo("Hello Martin");
    }

    @Test
    void singleQuotedStringsDoNotInterpolate() {
        assertThat(StringInterpolation.singleQuoted("Capybara")).isEqualTo("Hello {name}");
    }
}



