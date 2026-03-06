package pl.grzeslowski.capybara.parser;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;

class CapybaraParserTest {

    @ParameterizedTest(name = "{index}: should parse function {0}")
    @MethodSource
    void parse(String name, String code) {
        var functional = new CapybaraParser().parseFunctional(code);
        findFunction(name, functional);
        System.out.println(functional);
    }

    private static Function findFunction(String name, Functional functional) {
        return functional.definitions()
                .stream()
                .filter(Function.class::isInstance)
                .map(Function.class::cast)
                .filter(f -> f.name().equals(name))
                .findAny()
                .orElseThrow(() -> new AssertionError("Function " + name + " not found"));
    }

    static Stream<Arguments> parse() {
        return Stream.of(
                Arguments.of(
                        "test_if",
                        """
                                fun test_if(x: int): string =
                                    if x > 0 then "positive"
                                    else if x < 0 then "negative"
                                    else "zero"
                                """),
                Arguments.of("list_identity", "fun list_identity(l: list[int]): list[int] = l"),
                Arguments.of("pipe_map", "fun pipe_map(l: list[int]): list[int] = l | x => x * 2"));
    }

    @Test
    @DisplayName("should parse list type")
    void parseListType() {
        // given
        var name = "list_identity";
        var code = "fun list_identity(l: list[int]): list[int] = l";

        // when
        var functional = new CapybaraParser().parseFunctional(code);

        // then
        var function = findFunction(name, functional);
        var parameters = function.parameters();
        assertThat(parameters).hasSize(1);
        var parameter = parameters.get(0);
        assertThat(parameter.name()).isEqualTo("l");
        var listOfIntsType = new CollectionType.ListType(PrimitiveType.INT);
        assertThat(parameter.type()).isEqualTo(listOfIntsType);
        assertThat(function.returnType()).hasValue(listOfIntsType);
    }
}
