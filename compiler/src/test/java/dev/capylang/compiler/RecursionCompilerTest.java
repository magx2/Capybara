package dev.capylang.compiler;

import dev.capylang.compiler.parser.RawModule;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.TreeSet;

import static org.assertj.core.api.Assertions.assertThat;

class RecursionCompilerTest {
    @Test
    void shouldClassifyUnmarkedRecursiveFunctions() {
        var program = compileProgram("""
                fun unmarked_tail_sum(n: int, acc: int): int =
                    if n <= 0 then acc else unmarked_tail_sum(n - 1, acc + n)

                fun unmarked_non_tail_sum(n: int): int =
                    if n <= 0 then 0 else n + unmarked_non_tail_sum(n - 1)

                fun plain(n: int): int = n + 1
                """);

        assertThat(function(program, "unmarked_tail_sum"))
                .satisfies(function -> {
                    assertThat(function.recursive()).isTrue();
                    assertThat(function.tailRecursive()).isTrue();
                });
        assertThat(function(program, "unmarked_non_tail_sum"))
                .satisfies(function -> {
                    assertThat(function.recursive()).isTrue();
                    assertThat(function.tailRecursive()).isFalse();
                });
        assertThat(function(program, "plain"))
                .satisfies(function -> {
                    assertThat(function.recursive()).isFalse();
                    assertThat(function.tailRecursive()).isFalse();
                });
    }

    @Test
    void shouldClassifyUnmarkedLocalTailRecursiveFunctions() {
        var program = compileProgram("""
                fun unmarked_local_tail_sum(n: int): int =
                    fun __sum(n: int, acc: int): int =
                        if n <= 0 then acc else __sum(n - 1, acc + n)
                    ---
                    __sum(n, 0)
                """);

        assertThat(program.modules().first().functions())
                .filteredOn(function -> function.name().contains("__local_fun_"))
                .singleElement()
                .satisfies(function -> {
                    assertThat(function.recursive()).isTrue();
                    assertThat(function.tailRecursive()).isTrue();
                });
    }

    private static CompiledProgram compileProgram(String source) {
        var result = CapybaraCompiler.INSTANCE.compile(
                List.of(new RawModule("Recursion", "/foo/bar", source)),
                new TreeSet<>()
        );
        if (result instanceof Result.Error<CompiledProgram> error) {
            throw new AssertionError(error.errors().toString());
        }
        return ((Result.Success<CompiledProgram>) result).value();
    }

    private static CompiledFunction function(CompiledProgram program, String name) {
        return program.modules().first().functions().stream()
                .filter(function -> function.name().equals(name))
                .findFirst()
                .orElseThrow();
    }
}
