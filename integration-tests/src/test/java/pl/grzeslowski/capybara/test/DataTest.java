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

    @Test
    void singleCreatesEnumSingleton() {
        Data.Knight knight = Data.Tom.INSTANCE;
        assertThat(knight).isSameAs(Data.Tom.INSTANCE);
    }

    @Test
    void genericTypeCanIncludeSingleSubtype() {
        Data.King king = Data.Jerry.INSTANCE;
        assertThat(king).isSameAs(Data.Jerry.INSTANCE);
    }

    @Test
    void genericTypeCanIncludeGenericRecordSubtype() {
        Data.King king = new Data.FrenchKing<>(true, "baguette");
        assertThat(king).isInstanceOf(Data.FrenchKing.class);
    }

    @Test
    void parentTypeFieldsArePresentInSubtypes() {
        Data.Worker blueCollar = new Data.BlueCollar("Jan", "Kowalski", 10.0f);
        Data.Worker whiteCollar = new Data.WhiteCollar("Anna", "Nowak", 9.5f);

        assertThat(((Data.BlueCollar) blueCollar).name()).isEqualTo("Jan");
        assertThat(((Data.BlueCollar) blueCollar).surname()).isEqualTo("Kowalski");
        assertThat(((Data.BlueCollar) blueCollar).strength()).isEqualTo(10.0f);

        assertThat(((Data.WhiteCollar) whiteCollar).name()).isEqualTo("Anna");
        assertThat(((Data.WhiteCollar) whiteCollar).surname()).isEqualTo("Nowak");
        assertThat(((Data.WhiteCollar) whiteCollar).intelligence()).isEqualTo(9.5f);
    }

    @Test
    void dataToStringUsesJsonLikeFieldRendering() {
        assertThat(Data.printableDataToString())
                .isEqualTo("PrintableData { \"foo\": \"abc\", \"boo\": 123 }");
    }

    @Test
    void stringPlusDataWorksInBothDirections() {
        assertThat(Data.stringPlusDataLeft())
                .isEqualTo("prefix=PrintableData { \"foo\": \"abc\", \"boo\": 123 }");
        assertThat(Data.stringPlusDataRight())
                .isEqualTo("PrintableData { \"foo\": \"abc\", \"boo\": 123 }=suffix");
    }

    @Test
    void canCreateDataWithPositionalArguments() {
        var value = Data.createPositionalData("abc", 10);
        assertThat(value.text()).isEqualTo("abc");
        assertThat(value.number()).isEqualTo(10);
    }

    @Test
    void positionalArgumentsCanBeExpressions() {
        var value = Data.createPositionalDataExpr("abc", 10);
        assertThat(value.text()).isEqualTo("abc!");
        assertThat(value.number()).isEqualTo(11);
    }

    @SuppressWarnings("unchecked")
    @Test
    void genericFieldCanBeMappedWithResultPipe() {
        var success = Data.genericFieldPipe(new Data.Box<>("41"));
        assertThat(success).isInstanceOf(Data.BoxSuccess.class);
        assertThat(((Data.BoxSuccess<Long>) success).value()).isEqualTo(43L);

        var error = Data.genericFieldPipe(new Data.Box<>("x"));
        assertThat(error).isInstanceOf(Data.BoxError.class);
    }
}
