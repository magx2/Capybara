package pl.grzeslowski.capybara.generator.java;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.api.Test;
import pl.grzeslowski.capybara.parser.Module;
import pl.grzeslowski.capybara.parser.Program;
import pl.grzeslowski.capybara.compiler.*;
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
import static pl.grzeslowski.capybara.compiler.CompiledExpressionPrinter.printExpression;
import static pl.grzeslowski.capybara.compiler.PrimitiveLinkedType.ANY;

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
    void returnType(String name, String fun, CompiledType expectedReturnType) {
        // given
        var program = compileProgram(fun);

        // when
        var returnType = findFunction(name, program).map(CompiledFunction::returnType);

        // then
        assertThat(returnType).contains(expectedReturnType);
    }

    static Stream<Arguments> returnType() {
        return Stream.of(
                Arguments.of(
                        "list_of_obj",
                        "fun list_of_obj() = []",
                        new CollectionLinkedType.CompiledList(ANY)),
                Arguments.of(
                        "set_of_obj",
                        "fun set_of_obj() = {}",
                        new CollectionLinkedType.CompiledSet(ANY)),
                Arguments.of(
                        "dict_of_obj",
                        "fun dict_of_obj() = { \"one\": 1, }",
                        new CollectionLinkedType.CompiledDict(PrimitiveLinkedType.INT))
        );
    }

    @ParameterizedTest(name = "{index}: should `{0}`")
    @MethodSource
    void wild(String name, String fun, String expected) {
        var program = compileProgram(fun);
        var expression = findFunction(name, program)
                .map(CompiledFunction::expression)
                .orElseThrow();
        printExpression(expression);

        // when
        var evaluated = JavaExpressionEvaluator.evaluateExpression(expression);

        // then
        assertThat(evaluated).isEqualToNormalizingNewlines(expected);
    }

    private static CompiledProgram compileProgram(String fun) {
        var functional = CapybaraParser.INSTANCE.parseFunctional("test", "/foo/boo", fun).functional();
        var programValueOrError = CapybaraCompiler.INSTANCE.compile(new Program(List.of(new Module("test", "/foo/boo", functional))), new java.util.TreeSet<>());
        if (programValueOrError instanceof ValueOrError.Error<CompiledProgram> er) {
            throw new AssertionError(er.errors()
                    .stream()
                    .map(ValueOrError.Error.SingleError::message)
                    .collect(joining(", ")));
        }
        return ((ValueOrError.Value<CompiledProgram>) programValueOrError).value();
    }

    private static Optional<CompiledFunction> findFunction(String name, CompiledProgram program) {
        return program.modules()
                .stream()
                .map(CompiledModule::functions)
                .flatMap(Collection::stream)
                .filter(f -> f.name().equals(name))
                .findAny();
    }

    @Test
    void dictKeysMustBeStrings() {
        var functional = CapybaraParser.INSTANCE.parseFunctional("test", "/foo/boo", """
                fun invalid_dict() = {
                    1: 1
                }
                """).functional();
        var programValueOrError = CapybaraCompiler.INSTANCE.compile(new Program(List.of(new Module("test", "/foo/boo", functional))), new java.util.TreeSet<>());
        assertThat(programValueOrError).isInstanceOf(ValueOrError.Error.class);
        var error = (ValueOrError.Error<CompiledProgram>) programValueOrError;
        assertThat(error.errors().stream().map(ValueOrError.Error.SingleError::message).collect(joining(",")))
                .contains("dict keys must be of type `STRING`");
    }

    static Stream<Arguments> wild() {
        return Stream.of(
                Arguments.of(
                        "wild_if",
                        """
                                fun wild_if(x: int): string =
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
                                return ((a_j1>2)) ? (("I'm happy "+java.lang.String.valueOf(a_j2))) : (("I'm not happy "+java.lang.String.valueOf(a_j3)));"""
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
                        "return java.util.stream.Stream.of(java.util.Map.entry(\"one\", 1), java.util.Map.entry(\"two\", 2), java.util.Map.entry(\"three\", 3)).collect(java.util.stream.Collectors.toMap(java.util.Map.Entry::getKey, java.util.Map.Entry::getValue, (oldValue, newValue) -> newValue, java.util.LinkedHashMap::new));"
                ),
                Arguments.of(
                        "single_quote_string",
                        "fun single_quote_string(): string = 'hello'",
                        "return \"hello\";"
                ),
                Arguments.of(
                        "summon_tom",
                        """
                                type Knight = EnglishKnight | Tom
                                data EnglishKnight { power: float }
                                single Tom
                                fun summon_tom(): Knight = Tom
                                """,
                        "return Tom.INSTANCE;"
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
                        "map",
                        """
                                fun map(l: list[int]) = l | :double

                                fun double(x: int) = x * x
                                """,
                        "return l.stream().map(it -> (double_(it))).toList();"
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
                        "return l.stream().reduce((a, b) -> ((a+b))).orElse(0);"
                ),
                Arguments.of(
                        "pipe_flat_map",
                        """
                                fun pipe_flat_map(l: list[int]): list[int] =
                                    l |* x => [x, x + 1]
                                """,
                        "return l.stream().flatMap(x -> (java.util.List.of(x, (x+1))).stream()).toList();"
                )
        );
    }
}



