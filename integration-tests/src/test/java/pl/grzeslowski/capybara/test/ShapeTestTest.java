package pl.grzeslowski.capybara.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class ShapeTestTest {

    @Test
    void sealedClasses() {
        // given
        var circle = new ShapeTest.Circle(10f);
        var rectangle = new ShapeTest.Rectangle(15f, 20f);

        // then
        assertThat(circle).isInstanceOf(ShapeTest.Shape.class);
        assertThat(circle.radius()).isEqualTo(10f);

        assertThat(rectangle).isInstanceOf(ShapeTest.Shape.class);
        assertThat(rectangle.width()).isEqualTo(15f);
        assertThat(rectangle.height()).isEqualTo(20f);
    }
}
