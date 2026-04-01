package dev.capylang.compiler;

import org.junit.jupiter.api.Test;
import dev.capylang.compiler.parser.RawModule;

import java.util.List;
import java.util.SortedSet;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.fail;

class CapybaraCompilerLibrariesTest {
    @Test
    void shouldCompileAgainstLibrariesWithoutIncludingThemInOutput() {
        var librarySource = """
                data Message { value: string }
                fun make_message(value: string): Message = Message { value: value }
                """;
        var libraries = compileProgram(List.of(new RawModule("Library", "/foo/lib", librarySource)), new java.util.TreeSet<>()).modules();

        var consumerSource = """
                from Library import { * }
                fun consume(value: string): Message = make_message(value)
                fun unwrap(message: Message): string = message.value
                """;
        var compiled = compileProgram(List.of(new RawModule("Consumer", "/foo/app", consumerSource)), libraries);

        assertThat(compiled.modules()).extracting(CompiledModule::name).containsExactly("Consumer");
        assertThat(compiled.modules().first().functions())
                .extracting(CompiledFunction::name)
                .containsExactlyInAnyOrder("consume", "unwrap");
    }

    @Test
    void shouldResolveImportedParentTypeFieldsAcrossModules() {
        var compiled = compileProgram(List.of(
                new RawModule("Assert", "/capy/test", """
                        data Assertion { result: bool, message: string }
                        type Assert { assertions: list[Assertion] } = StringAssert
                        data StringAssert { value: string }
                        """),
                new RawModule("CapyTest", "/capy/test", """
                        from Assert import { * }

                        data TestCase { asserts: list[Assert] }
                        fun assertion_count(test_case: TestCase): int =
                            test_case.asserts
                                |> 0, (acc, a) => acc + a.assertions.size
                        """)
        ), new java.util.TreeSet<>());

        assertThat(compiled.modules())
                .extracting(CompiledModule::name)
                .containsExactlyInAnyOrder("Assert", "CapyTest");
        assertThat(compiled.modules().stream()
                .filter(module -> module.name().equals("CapyTest"))
                .findFirst()
                .orElseThrow()
                .functions())
                .extracting(CompiledFunction::name)
                .contains("assertion_count");
    }


    @Test
    void shouldResolveImportedDataFieldsNestedInCollectionsAcrossModules() {
        var compiled = compileProgram(List.of(
                new RawModule("CapyTest", "/capy/test", """
                        data TestCase { asserts: list[string] }
                        data TestFile { test_cases: list[TestCase] }
                        """),
                new RawModule("Runtime", "/capy/test", """
                        from CapyTest import { * }

                        fun assertion_count(test_file: TestFile): int =
                            test_file.test_cases | tc => tc.asserts.size |> 0, (acc, count) => acc + count
                        """)
        ), new java.util.TreeSet<>());

        assertThat(compiled.modules())
                .extracting(CompiledModule::name)
                .containsExactlyInAnyOrder("CapyTest", "Runtime");
        assertThat(compiled.modules().stream()
                .filter(module -> module.name().equals("Runtime"))
                .findFirst()
                .orElseThrow()
                .functions())
                .extracting(CompiledFunction::name)
                .contains("assertion_count");
    }

    @Test
    void shouldCompileDataWithExpression() {
        var compiled = compileProgram(List.of(
                new RawModule("WithData", "/foo/with", """
                        data Foo { a: int, b: string }
                        fun update(foo: Foo): Foo = foo.with(a = foo.a + 1, b = "x")
                        """)
        ), new java.util.TreeSet<>());

        assertThat(compiled.modules().stream()
                .filter(module -> module.name().equals("WithData"))
                .findFirst()
                .orElseThrow()
                .functions())
                .extracting(CompiledFunction::name)
                .contains("update");
    }

    @Test
    void shouldCompileParentWithExpression() {
        var compiled = compileProgram(List.of(
                new RawModule("WithParent", "/foo/with", """
                        type Letter { x: int } = A | B
                        data A { a: string }
                        data B { b: int }
                        fun update(letter: Letter): Letter = letter.with(x = letter.x + 1)
                        """)
        ), new java.util.TreeSet<>());

        assertThat(compiled.modules().stream()
                .filter(module -> module.name().equals("WithParent"))
                .findFirst()
                .orElseThrow()
                .functions())
                .extracting(CompiledFunction::name)
                .contains("update");
    }

    private static CompiledProgram compileProgram(List<RawModule> rawModules, SortedSet<CompiledModule> libraries) {
        var result = CapybaraCompiler.INSTANCE.compile(rawModules, libraries);
        if (result instanceof Result.Error<CompiledProgram> error) {
            fail(error.errors().toString());
        }
        return ((Result.Success<CompiledProgram>) result).value();
    }
}

