package pl.grzeslowski.capybara.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class DataTest {
    @Test
    void algebraicTypeCanHoldSubtypes() {
        Data.Shape circle = new Data.Circle(2.5f);
        Data.Shape rectangle = new Data.Rectangle(3.0f, 4.0f);

        assertThat(circle).isInstanceOf(Data.Circle.class);
        assertThat(rectangle).isInstanceOf(Data.Rectangle.class);
    }

    @Test
    void quotedFieldNameIsSupported() {
        var rectangle = new Data.Rectangle(3.0f, 4.0f);

        assertThat(rectangle.width()).isEqualTo(3.0f);
        assertThat(rectangle.height()).isEqualTo(4.0f);
    }

    @Test
    void areaUsesAlgebraicMatch() {
        assertThat(Data.area(new Data.Circle(2.0f))).isEqualTo(12.56f);
        assertThat(Data.area(new Data.Rectangle(3.0f, 4.0f))).isEqualTo(12.0f);
    }

    @Test
    void daVinciMapsBetweenVariants() {
        var fromCircle = Data.daVinci(new Data.Circle(2.0f));
        var fromRectangle = Data.daVinci(new Data.Rectangle(6.0f, 2.0f));

        assertThat(fromCircle).isInstanceOf(Data.Rectangle.class);
        assertThat(((Data.Rectangle) fromCircle).width()).isEqualTo(4.0f);
        assertThat(((Data.Rectangle) fromCircle).height()).isEqualTo(4.0f);

        assertThat(fromRectangle).isInstanceOf(Data.Circle.class);
        assertThat(((Data.Circle) fromRectangle).radius()).isEqualTo(2.0f);
    }
}
