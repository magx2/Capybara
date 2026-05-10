package dev.capylang.test.compilation_error;

import dev.capylang.compiler.CapybaraCompiler;
import dev.capylang.compiler.CompiledProgram;
import dev.capylang.compiler.Result;
import dev.capylang.compiler.parser.RawModule;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class TupleDestructuringCompilationErrorTest {
    @Test
    void shouldFailWhenTupleDestructuringIsUsedForNonTupleElements() {
        var error = compileFailure(
                "tuple_destructuring_requires_tuple_elements",
                """
                from /capy/lang/Collections import { * }
                fun foo(values: List[int]) = values | (a, b) => a + b
                """
        );

        assertThat(error.file()).isEqualTo("/foo/boo/tuple_destructuring_requires_tuple_elements.cfun");
        assertThat(error.message()).contains("Tuple destructuring can only be used for tuple elements");
    }

    @Test
    void shouldFailWhenTupleDestructuringArityDoesNotMatchTupleSize() {
        var error = compileFailure(
                "tuple_destructuring_arity_mismatch",
                """
                from /capy/lang/Collections import { * }
                fun foo(values: List[Tuple[int, int]]) = values | (a, b, c) => a + b + c
                """
        );

        assertThat(error.file()).isEqualTo("/foo/boo/tuple_destructuring_arity_mismatch.cfun");
        assertThat(error.message()).contains("Tuple destructuring expects 2 arguments, got 3");
    }

    private static Result.Error.SingleError compileFailure(String moduleName, String code) {
        var result = CapybaraCompiler.INSTANCE.compile(
                List.of(new RawModule(moduleName, "/foo/boo", code)),
                new java.util.TreeSet<>()
        );
        if (result instanceof Result.Success<CompiledProgram> value) {
            throw new AssertionError("Expected compilation error but got CompiledProgram: " + value);
        }
        var errors = ((Result.Error<CompiledProgram>) result).errors();
        assertThat(errors).isNotEmpty();
        return errors.first();
    }
}
