package pl.grzeslowski.capybara.generator.java;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.api.Test;
import pl.grzeslowski.capybara.compiler.Module;
import pl.grzeslowski.capybara.compiler.Program;
import pl.grzeslowski.capybara.linker.*;
import pl.grzeslowski.capybara.parser.CapybaraParser;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Stream;

import static java.util.stream.Collectors.joining;
import static org.assertj.core.api.Assertions.assertThat;
import static pl.grzeslowski.capybara.linker.LinkedExpressionPrinter.printExpression;
import static pl.grzeslowski.capybara.linker.PrimitiveLinkedType.ANY;

class JavaExpressionEvaluatorTest {
    static {
        var rootLogger = Logger.getLogger("");
        rootLogger.setLevel(Level.FINE);
        for (Handler handler : rootLogger.getHandlers()) {
            handler.setLevel(Level.FINE);
        }
    }

    @ParameterizedTest(name = "{index}: should compile method {0} to return {2}")
    @MethodSource
    void returnType(String name, String fun, LinkedType expectedReturnType) {
        // given
        var program = compileProgram(fun);

        // when
        var returnType = findFunction(name, program).map(LinkedFunction::returnType);

        // then
        assertThat(returnType).contains(expectedReturnType);
    }

    static Stream<Arguments> returnType() {
        return Stream.of(
                Arguments.of(
                        "list_of_obj",
                        "fun list_of_obj() = []",
                        new CollectionLinkedType.LinkedList(ANY)),
                Arguments.of(
                        "set_of_obj",
                        "fun set_of_obj() = {}",
                        new CollectionLinkedType.LinkedSet(ANY)),
                Arguments.of(
                        "dict_of_obj",
                        "fun dict_of_obj() = { \"one\": 1, }",
                        new CollectionLinkedType.LinkedDict(PrimitiveLinkedType.INT))
        );
    }

    @ParameterizedTest(name = "{index}: should `{0}`")
    @MethodSource
    void wild(String name, String fun, String expected) {
        var program = compileProgram(fun);
        var expression = findFunction(name, program)
                .map(LinkedFunction::expression)
                .orElseThrow();
        printExpression(expression);

        // when
        var evaluated = JavaExpressionEvaluator.evaluateExpression(expression);

        // then
        assertThat(evaluated).isEqualToNormalizingNewlines(expected);
    }

    private static LinkedProgram compileProgram(String fun) {
        var functional = CapybaraParser.INSTANCE.parseFunctional(fun);
        var programValueOrError = CapybaraLinker.INSTANCE.link(new Program(List.of(new Module("test", "/foo/boo", functional))));
        if (programValueOrError instanceof ValueOrError.Error<LinkedProgram> er) {
            throw new AssertionError(er.errors()
                    .stream()
                    .map(ValueOrError.Error.SingleError::message)
                    .collect(joining(", ")));
        }
        return ((ValueOrError.Value<LinkedProgram>) programValueOrError).value();
    }

    private static Optional<LinkedFunction> findFunction(String name, LinkedProgram program) {
        return program.modules()
                .stream()
                .map(LinkedModule::functions)
                .flatMap(Collection::stream)
                .filter(f -> f.name().equals(name))
                .findAny();
    }

    @Test
    void dictKeysMustBeStrings() {
        var functional = CapybaraParser.INSTANCE.parseFunctional("""
                fun invalid_dict() = {
                    1: 1
                }
                """);
        var programValueOrError = CapybaraLinker.INSTANCE.link(new Program(List.of(new Module("test", "/foo/boo", functional))));
        assertThat(programValueOrError).isInstanceOf(ValueOrError.Error.class);
        var error = (ValueOrError.Error<LinkedProgram>) programValueOrError;
        assertThat(error.errors().stream().map(ValueOrError.Error.SingleError::message).collect(joining(",")))
                .contains("dict keys must be of type `STRING`");
    }

    static Stream<Arguments> wild() {
        return Stream.of(
                Arguments.of(
                        "wild_if",
                        """
                                fun wild_if(x: int): int =
                                    let a = "unsued?"
                                    if ({
                                        let a = x * 2
                                        a > 2
                                    }) then {
                                        let a = x * x
                                        "I'm happy " + a
                                       } else {
                                        let a = x / 2
                                        "I'm not happy " + a
                                        }
                                """,
                        """
                                var a = "unsued?";
                                var a_j1 = (x*2);
                                var a_j2 = (x*x);
                                var a_j3 = (x/2);
                                return ((a_j1>2)) ? (("I'm happy "+a_j2)) : (("I'm not happy "+a_j3));"""
                ),
                Arguments.of(
                        "wild_infix",
                        """
                                fun wild_infix(a: int, b: int): int =
                                    {
                                        let x = a * 2
                                        x - 1
                                    } / {
                                        let x = if(b!=0) then b else 1
                                        x*2
                                    }
                                """,
                        """
                                var x = (a*2);
                                var x_j1 = ((b!=0)) ? (b) : (1);
                                return ((x-1)/(x_j1*2));"""
                ),
                Arguments.of(
                        "wild_if_nested",
                        """
                                fun wild_if_nested(a: int, b: int): int =
                                    if (a > 0) then {
                                        {
                                            let x = a * 2
                                            x + 1
                                        }
                                    } else {
                                        {
                                            let x = b * 3
                                            x - 1
                                        }
                                    }
                                """,
                        """
                                var x = (a*2);
                                var x_j1 = (b*3);
                                return ((a>0)) ? ((x+1)) : ((x_j1-1));"""
                ),
                Arguments.of(
                        "wild_infix_single_side_let",
                        """
                                fun wild_infix_single_side_let(a: int): int =
                                    a + {
                                        let x = a * 2
                                        x + 1
                                    }
                                """,
                        """
                                var x = (a*2);
                                return (a+(x+1));"""
                ),
                Arguments.of(
                        "list_of_obj",
                        "fun list_of_obj() = []",
                        "return java.util.List.of();"
                ),
                Arguments.of(
                        "dict_of_obj",
                        """
                                fun dict_of_obj() = {
                                    "one": 1,
                                    "two": 2,
                                    "three": 3,
                                }
                                """,
                        "return java.util.Map.of(\"one\", 1, \"two\", 2, \"three\", 3);"
                ),
                Arguments.of(
                        "pipe_map",
                        """
                                fun pipe_map(l: list[int]): list[int] =
                                    l | x => x * 2
                                """,
                        "return l.stream().map(x -> ((x*2))).toList();"
                ),
                Arguments.of(
                        "pipe_filter_out",
                        """
                                fun pipe_filter_out(l: list[int]): list[int] =
                                    l |- x => x > 2
                                """,
                        "return l.stream().filter(x -> !((x>2))).toList();"
                ),
                Arguments.of(
                        "pipe_reduce",
                        """
                                fun pipe_reduce(l: list[int]): int =
                                    l |> 0, (a, b) => a + b
                                """,
                        "return l.stream().reduce(0, (a, b) -> ((a+b)));"
                )
        );
    }
}
