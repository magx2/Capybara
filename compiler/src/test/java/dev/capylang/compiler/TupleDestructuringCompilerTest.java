package dev.capylang.compiler;

import dev.capylang.compiler.parser.RawModule;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.fail;

class TupleDestructuringCompilerTest {
    @Test
    void shouldCompileTupleDestructuringInPipeMapper() {
        var compiled = compileProgram(List.of(new RawModule(
                "TuplePipes",
                "/foo/boo",
                "fun map_pairs(values: List[Tuple[int, int]]): List[String] = values | (number, expected) => \"digits(\" + number + \") should return \" + expected"
        )));

        var function = compiled.modules().first().functions().stream()
                .filter(it -> it.name().equals("map_pairs"))
                .findFirst()
                .orElseThrow();

        assertThat(CompiledExpressionPrinter.printExpression(function.expression(), 0))
                .contains("| number;;expected =>")
                .contains("(number: INT)")
                .contains("(expected: INT)");
    }

    @Test
    void shouldCompileTupleDestructuringInPipeFilter() {
        var compiled = compileProgram(List.of(new RawModule(
                "TuplePipes",
                "/foo/boo",
                "fun filter_pairs(values: List[Tuple[int, int]]): List[Tuple[int, int]] = values |- (left, right) => left == right"
        )));

        var function = compiled.modules().first().functions().stream()
                .filter(it -> it.name().equals("filter_pairs"))
                .findFirst()
                .orElseThrow();

        assertThat(CompiledExpressionPrinter.printExpression(function.expression(), 0))
                .contains("|- left;;right =>")
                .contains("(left: INT) == (right: INT)");
    }

    @Test
    void shouldFailWhenTupleDestructuringIsUsedForNonTupleElements() {
        var error = compileFailure("fun foo(values: List[int]) = values | (a, b) => a + b");

        assertThat(error.message()).contains("Right side lambda of `|` can use tuple destructuring only for tuple elements");
    }

    @Test
    void shouldFailWhenTupleDestructuringArityDoesNotMatchTupleSize() {
        var error = compileFailure("fun foo(values: List[Tuple[int, int]]) = values |* (a, b, c) => [a, b, c]");

        assertThat(error.message()).contains("Tuple destructuring in `|*` expects 2 arguments, got 3");
    }

    private static CompiledProgram compileProgram(List<RawModule> rawModules) {
        var result = CapybaraCompiler.INSTANCE.compile(rawModules, new java.util.TreeSet<>());
        if (result instanceof Result.Error<CompiledProgram> error) {
            fail(error.errors().toString());
        }
        return ((Result.Success<CompiledProgram>) result).value();
    }

    private static Result.Error.SingleError compileFailure(String code) {
        var result = CapybaraCompiler.INSTANCE.compile(
                List.of(new RawModule("TuplePipes", "/foo/boo", code)),
                new java.util.TreeSet<>()
        );
        if (result instanceof Result.Success<CompiledProgram> value) {
            fail("Expected compilation to fail but it succeeded: " + value);
        }
        var errors = ((Result.Error<CompiledProgram>) result).errors();
        assertThat(errors).isNotEmpty();
        return errors.first();
    }
}