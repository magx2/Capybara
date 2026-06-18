package dev.capylang.compiler;

import dev.capylang.compiler.parser.RawModule;
import org.junit.jupiter.api.Test;

import java.time.Duration;
import java.util.List;
import java.util.TreeSet;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertTimeoutPreemptively;

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

    @Test
    void shouldUseStdlibRecursiveAnnotationAsTailRecursionContract() {
        var program = compileProgram(List.of(
                recursiveAnnotationModule(),
                new RawModule("Recursion", "/foo/bar", """
                        from /capy/meta_prog/Recursive import { Recursive }

                        @Recursive
                        fun sum(n: int, acc: int): int =
                            if n <= 0 then acc else sum(n - 1, acc + n)
                        """)
        ));

        assertThat(function(program, "sum"))
                .satisfies(function -> {
                    assertThat(function.recursive()).isTrue();
                    assertThat(function.tailRecursive()).isTrue();
                    assertThat(function.annotations()).singleElement().satisfies(annotation -> {
                        assertThat(annotation.name()).isEqualTo("Recursive");
                        assertThat(annotation.packageName()).isEqualTo("Recursive");
                        assertThat(annotation.packagePath()).isEqualTo("/capy/meta_prog");
                    });
                });
    }

    @Test
    void shouldNotTreatUserAnnotationNamedRecursiveAsTailRecursionContract() {
        var program = compileProgram("""
                annotation Recursive on fun {}

                @Recursive
                fun non_tail_sum(n: int): int =
                    if n <= 0 then 0 else n + non_tail_sum(n - 1)
                """);

        assertThat(function(program, "non_tail_sum"))
                .satisfies(function -> {
                    assertThat(function.recursive()).isTrue();
                    assertThat(function.tailRecursive()).isFalse();
                    assertThat(function.annotations()).singleElement().satisfies(annotation -> {
                        assertThat(annotation.name()).isEqualTo("Recursive");
                        assertThat(annotation.packageName()).isEqualTo("Recursion");
                        assertThat(annotation.packagePath()).isEqualTo("/foo/bar");
                    });
                });
    }

    @Test
    void shouldCompileRecursiveDataSignaturesWithoutWalkingFields() {
        var program = assertTimeoutPreemptively(
                Duration.ofSeconds(10),
                () -> compileProgram(recursiveSignatureSource())
        );

        assertThat(program.modules().first().functions())
                .extracting(CompiledFunction::name)
                .contains("wrap_tree", "wrap_trees", "describe");
    }

    private static CompiledProgram compileProgram(String source) {
        return compileProgram(List.of(new RawModule("Recursion", "/foo/bar", source)));
    }

    private static String recursiveSignatureSource() {
        return """
                from /capy/collection/List import { * }

                union Tree = Leaf | Branch
                data Leaf { value: int }
                data Branch { children: List[Tree] }

                union Result[T] = Success[T] | Error
                data Success[T] { value: T }
                data Error { message: String }

                fun wrap_tree(tree: Tree): Result[Tree] = Success { value: tree }
                fun wrap_trees(trees: List[Tree]): Result[List[Tree]] = Success { value: trees }
                fun describe(result: Result[Tree]): int = 1
                fun describe(result: Result[List[Tree]]): int = 2
                """;
    }

    private static RawModule recursiveAnnotationModule() {
        return new RawModule("Recursive", "/capy/meta_prog", "annotation Recursive on fun {}");
    }

    private static CompiledProgram compileProgram(List<RawModule> modules) {
        var result = CapybaraCompiler.INSTANCE.compile(
                modules,
                new TreeSet<>()
        );
        if (result instanceof Result.Error<CompiledProgram> error) {
            throw new AssertionError(error.errors().toString());
        }
        return ((Result.Success<CompiledProgram>) result).value();
    }

    private static CompiledFunction function(CompiledProgram program, String name) {
        return program.modules().stream()
                .flatMap(module -> module.functions().stream())
                .filter(function -> function.name().equals(name))
                .findFirst()
                .orElseThrow();
    }
}
