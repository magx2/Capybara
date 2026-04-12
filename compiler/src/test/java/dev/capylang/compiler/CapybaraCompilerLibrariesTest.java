package dev.capylang.compiler;

import dev.capylang.compiler.expression.CompiledFunctionCall;
import dev.capylang.compiler.expression.CompiledLetExpression;
import dev.capylang.compiler.expression.CompiledMatchExpression;
import dev.capylang.compiler.expression.CompiledNewData;
import org.junit.jupiter.api.Test;
import dev.capylang.compiler.parser.RawModule;

import java.util.List;
import java.util.SortedSet;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.fail;

class CapybaraCompilerLibrariesTest {
    @Test
    void shouldReturnErrorInsteadOfThrowingForSyntaxError() {
        var result = CapybaraCompiler.INSTANCE.compile(
                List.of(new RawModule("Broken", "/foo/app", "fun broken(): int = if true then {")),
                new java.util.TreeSet<>()
        );

        assertThat(result).isInstanceOf(Result.Error.class);
    }

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
    void shouldApplyLibraryDataConstructorWhenInstantiatingImportedData() {
        var librarySource = """
                data OddInt { number: int } with constructor {
                    if number % 2 == 0 then
                        * { number: number + 1 }
                    else
                        * { number: number }
                }
                """;
        var libraries = compileProgram(List.of(new RawModule("Library", "/foo/lib", librarySource)), new java.util.TreeSet<>()).modules();

        var consumerSource = """
                from Library import { * }
                fun build(value: int): OddInt = OddInt { number: value }
                """;
        var compiled = compileProgram(List.of(new RawModule("Consumer", "/foo/app", consumerSource)), libraries);

        assertThat(compiledFunction(compiled, "Consumer", "build").expression())
                .isInstanceOfSatisfying(CompiledFunctionCall.class, call ->
                        assertThat(call.name()).endsWith(".Library.__constructor__data__OddInt"));
    }

    @Test
    void shouldApplyLibraryTypeConstructorWhenInstantiatingImportedSubtype() {
        var librarySource = """
                from /capy/lang/Result import { * }

                type ValidatedName { name: string } with constructor {
                   if name.size == 0 then
                       Error { message: "Name was empty" }
                   else
                       Success { value: * { name: name } }
                } = NamedUser

                data NamedUser { name: string, role: string }
                """;
        var libraries = compileProgram(List.of(new RawModule("Library", "/foo/lib", librarySource)), new java.util.TreeSet<>()).modules();

        var consumerSource = """
                from Library import { * }

                fun build(name: string, role: string): Result[NamedUser] =
                    NamedUser { name: name, role: role }
                """;
        var compiled = compileProgram(List.of(new RawModule("Consumer", "/foo/app", consumerSource)), libraries);

        assertThat(compiledFunction(compiled, "Consumer", "build").expression())
                .isInstanceOf(CompiledMatchExpression.class);
    }

    @Test
    void shouldApplyTransitiveLibraryTypeConstructorWhenInstantiatingImportedSubtype() {
        var librarySource = """
                from /capy/lang/Result import { * }

                type ValidatedName { name: string } with constructor {
                   if name.size == 0 then
                       Error { message: "Name was empty" }
                   else
                       Success { value: * { name: name } }
                } = NamedEntity

                type NamedEntity { name: string } = NamedUser

                data NamedUser { name: string, role: string }
                """;
        var libraries = compileProgram(List.of(new RawModule("Library", "/foo/lib", librarySource)), new java.util.TreeSet<>()).modules();

        var consumerSource = """
                from Library import { * }

                fun build(name: string, role: string): Result[NamedUser] =
                    NamedUser { name: name, role: role }
                """;
        var compiled = compileProgram(List.of(new RawModule("Consumer", "/foo/app", consumerSource)), libraries);

        assertThat(compiledFunction(compiled, "Consumer", "build").expression())
                .isInstanceOf(CompiledMatchExpression.class);
    }

    @Test
    void shouldApplyDeclaredTypeCoercionInsideResultBindSuccessBranch() {
        var compiled = compileProgram(List.of(
                new RawModule("Consumer", "/foo/app", """
                        from /capy/lang/Result import { * }

                        data Parent { name: string }
                        data Child { age: int, ...Parent }

                        fun child(): Result[Child] =
                            Success { value: 1 } | value => Success { value: Child { age: value, name: "Ada" } }

                        fun use_parent(): Result[string] =
                            let parent: Parent <- child()
                            parent.name
                        """)
        ), new java.util.TreeSet<>());

        assertThat(compiledFunction(compiled, "Consumer", "use_parent").expression())
                .isInstanceOfSatisfying(CompiledMatchExpression.class, match ->
                        assertThat(match.cases().getFirst().expression())
                                .isInstanceOfSatisfying(CompiledNewData.class, success ->
                                        assertThat(success.assignments().getFirst().value())
                                                .isInstanceOf(CompiledLetExpression.class)));
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
                        fun update(foo: Foo): Foo = foo.with(a: foo.a + 1, b: "x")
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
                        fun update(letter: Letter): Letter = letter.with(x: letter.x + 1)
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

    @Test
    void shouldResolveModuleQualifiedTypeWithoutImport() {
        var compiled = compileProgram(List.of(
                new RawModule("Types", "/foo/lib", """
                        data Thing { value: int }
                        """),
                new RawModule("Consumer", "/foo/app", """
                        data Thing { label: string }

                        fun local_thing(): Thing = Thing { label: "local" }
                        fun imported_thing(value: int): Types.Thing = Types.Thing { value: value }
                        """)
        ), new java.util.TreeSet<>());

        assertThat(compiledFunction(compiled, "Consumer", "local_thing").returnType().name()).isEqualTo("Thing");
        assertThat(compiledFunction(compiled, "Consumer", "imported_thing").returnType().name()).isEqualTo("Types.Thing");
    }

    @Test
    void shouldResolvePathQualifiedTypeWithoutImport() {
        var compiled = compileProgram(List.of(
                new RawModule("Types", "/foo/bar", """
                        data Thing { value: int }
                        """),
                new RawModule("Consumer", "/foo/app", """
                        fun imported_thing(value: int): /foo/bar/Types.Thing = /foo/bar/Types.Thing { value: value }
                        """)
        ), new java.util.TreeSet<>());

        assertThat(compiledFunction(compiled, "Consumer", "imported_thing").returnType().name())
                .isEqualTo("/foo/bar/Types.Thing");
    }

    @Test
    void shouldResolvePathQualifiedModuleShorthandWhenModuleNameMatchesType() {
        var compiled = compileProgram(List.of(
                new RawModule("Program", "/foo/bar", """
                        data Program { value: int }
                        """),
                new RawModule("Consumer", "/foo/app", """
                        fun build(value: int): /foo/bar/Program = /foo/bar/Program { value: value }
                        """)
        ), new java.util.TreeSet<>());

        assertThat(compiledFunction(compiled, "Consumer", "build").returnType().name()).isEqualTo("/foo/bar/Program");
    }

    @Test
    void shouldPreferResultOverDataOverloadForConstructorExpressionsInMethodChains() {
        var compiled = compileProgram(List.of(
                new RawModule("Result", "/capy/lang", """
                        type Result[T] = Success[T] | Error
                        data Success[T] { value: T }
                        data Error { message: string }
                        """),
                new RawModule("Assert", "/capy/test", """
                        from /capy/lang/Result import { * }

                        data ResultAssert[T] { value: Result[T] }
                        data DataAssert { value: data }

                        fun ResultAssert[T].succeeds(): bool = true

                        fun assert_that(value: Result[T]): ResultAssert[T] = ResultAssert { value: value }
                        fun assert_that(value: data): DataAssert = DataAssert { value: value }
                        """),
                new RawModule("Date", "/capy/date_time", """
                        from /capy/lang/Result import { * }

                        data Date { day: int } with constructor {
                            if day > 0
                            then Success { value: * { day: day } }
                            else Error { message: "invalid" }
                        }
                        """),
                new RawModule("Consumer", "/foo/app", """
                        from /capy/test/Assert import { * }
                        from /capy/date_time/Date import { * }

                        fun prefer_result(): bool = assert_that(Date { day: 1 }).succeeds()
                        """)
        ), new java.util.TreeSet<>());

        assertThat(compiledFunction(compiled, "Consumer", "prefer_result").returnType()).isEqualTo(PrimitiveLinkedType.BOOL);
    }

    @Test
    void shouldPreferResultOverDataOverloadForConstructorExpressionsAgainstLinkedLibraries() {
        var libraries = compileProgram(List.of(
                new RawModule("Result", "/capy/lang", """
                        type Result[T] = Success[T] | Error
                        data Success[T] { value: T }
                        data Error { message: string }
                        """),
                new RawModule("Assert", "/capy/test", """
                        from /capy/lang/Result import { * }

                        data ResultAssert[T] { value: Result[T] }
                        data DataAssert { value: data }

                        fun assert_that(value: Result[T]): ResultAssert[T] = ResultAssert { value: value }
                        fun assert_that(value: data): DataAssert = DataAssert { value: value }
                        """),
                new RawModule("Date", "/capy/date_time", """
                        from /capy/lang/Result import { * }

                        data Date { day: int } with constructor {
                            if day > 0
                            then Success { value: * { day: day } }
                            else Error { message: "invalid" }
                        }
                        """)
        ), new java.util.TreeSet<>()).modules();

        var compiled = compileProgram(List.of(
                new RawModule("Consumer", "/foo/app", """
                        from /capy/test/Assert import { * }
                        from /capy/date_time/Date import { * }

                        fun prefer_result(): ResultAssert[Date] = assert_that(Date { day: 1 })
                        """)
        ), libraries);

        assertThat(compiledFunction(compiled, "Consumer", "prefer_result").returnType().name())
                .isEqualTo("ResultAssert");
    }

    @Test
    void shouldRejectNestedResultWhenResultAssertExpectsSingleResult() {
        var error = compileFailure(List.of(
                new RawModule("Result", "/capy/lang", """
                        type Result[T] = Success[T] | Error
                        data Success[T] { value: T }
                        data Error { message: string }
                        """),
                new RawModule("Assert", "/capy/test", """
                        from /capy/lang/Result import { * }

                        data ResultAssert[T] { value: Result[T] }
                        data DataAssert { value: data }

                        fun ResultAssert[T].is_equal_to(other: Result[T]): ResultAssert[T] = this
                        fun DataAssert.is_equal_to(other: data): DataAssert = this

                        fun assert_that(value: Result[T]): ResultAssert[T] = ResultAssert { value: value }
                        fun assert_that(value: data): DataAssert = DataAssert { value: value }
                        """),
                new RawModule("Widget", "/foo/model", """
                        from /capy/lang/Result import { * }

                        data Widget { size: int }
                        fun Widget.wrap(): Result[Widget] = Success { value: this }
                        """),
                new RawModule("Consumer", "/foo/app", """
                        from /capy/lang/Result import { * }
                        from /capy/test/Assert import { * }
                        from /foo/model/Widget import { * }

                        fun broken() =
                            assert_that(Success { value: Widget { size: 1 } } | widget => Success { value: widget.wrap() })
                                .is_equal_to(Success { value: Widget { size: 2 } })
                        """)
        ));

        assertThat(error.message()).contains("Expected `Widget`, got `Success`");
    }

    @Test
    void shouldPreferLocalConstructorOverUnimportedLinkedLibraryConstructorWithSameName() {
        var libraries = compileProgram(List.of(
                new RawModule("Result", "/capy/lang", """
                        type Result[T] = Success[T] | Error
                        data Success[T] { value: T }
                        data Error { message: string }
                        """),
                new RawModule("Assert", "/capy/test", """
                        from /capy/lang/Result import { * }

                        data ResultAssert[T] { value: Result[T] }

                        fun ResultAssert[T].succeeds(): bool = true
                        fun ResultAssert[T].fails(): bool = true

                        fun assert_that(value: Result[T]): ResultAssert[T] = ResultAssert { value: value }
                        """),
                new RawModule("Date", "/capy/date_time", """
                        from /capy/lang/Result import { * }

                        data Date { day: int, month: int, year: int } with constructor {
                            if day > 0 then
                                Success { value: * { day: day, month: month, year: year } }
                            else
                                Error { message: "invalid" }
                        }
                        """)
        ), new java.util.TreeSet<>()).modules();

        var compiled = compileProgram(List.of(
                new RawModule("Consumer", "/dev/capylang/test", """
                        from /capy/lang/Result import { * }
                        from /capy/test/Assert import { * }

                        data Date { day: int } with constructor {
                            if day > 0
                            then Success { value: * { day: day } }
                            else Error { message: "invalid" }
                        }

                        fun valid_date_assert_succeeds(): bool =
                            assert_that(Date { day: 1 }).succeeds()

                        fun invalid_date_assert_fails(): bool =
                            assert_that(Date { day: 0 }).fails()
                        """)
        ), libraries);

        assertThat(compiledFunction(compiled, "Consumer", "valid_date_assert_succeeds").returnType())
                .isEqualTo(PrimitiveLinkedType.BOOL);
        assertThat(compiledFunction(compiled, "Consumer", "invalid_date_assert_fails").returnType())
                .isEqualTo(PrimitiveLinkedType.BOOL);
    }

    @Test
    void shouldResolveMethodsDefinedInLinkedLibraryModules() {
        var libraries = compileProgram(List.of(
                new RawModule("FooAssert", "/foo/test", """
                        data OutcomeAssert { value: bool }
                        fun OutcomeAssert.ok(): bool = this.value
                        fun check(value: bool): OutcomeAssert = OutcomeAssert { value: value }
                        """)
        ), new java.util.TreeSet<>()).modules();

        var compiled = compileProgram(List.of(
                new RawModule("Consumer", "/foo/app", """
                        from /foo/test/FooAssert import { * }

                        fun use_method(): bool = check(true).ok()
                        """)
        ), libraries);

        assertThat(compiledFunction(compiled, "Consumer", "use_method").returnType())
                .isEqualTo(PrimitiveLinkedType.BOOL);
    }

    @Test
    void shouldResolveQualifiedImportsWhenSimpleModuleNamesCollide() {
        var libraries = compileProgram(List.of(
                new RawModule("Assert", "/foo/test", """
                        data OutcomeAssert { value: bool }
                        fun OutcomeAssert.ok(): bool = this.value
                        fun check(value: bool): OutcomeAssert = OutcomeAssert { value: value }
                        """)
        ), new java.util.TreeSet<>()).modules();

        var compiled = compileProgram(List.of(
                new RawModule("Consumer", "/foo/app", """
                        from /foo/test/Assert import { * }

                        fun use_method(): bool = check(true).ok()
                        """)
        ), libraries);

        assertThat(compiledFunction(compiled, "Consumer", "use_method").returnType())
                .isEqualTo(PrimitiveLinkedType.BOOL);
    }

    private static CompiledProgram compileProgram(List<RawModule> rawModules, SortedSet<CompiledModule> libraries) {
        var result = CapybaraCompiler.INSTANCE.compile(rawModules, libraries);
        if (result instanceof Result.Error<CompiledProgram> error) {
            fail(error.errors().toString());
        }
        return ((Result.Success<CompiledProgram>) result).value();
    }

    private static Result.Error.SingleError compileFailure(List<RawModule> rawModules) {
        var result = CapybaraCompiler.INSTANCE.compile(rawModules, new java.util.TreeSet<>());
        if (result instanceof Result.Success<CompiledProgram> value) {
            fail("Expected compilation to fail but it succeeded: " + value);
        }
        var errors = ((Result.Error<CompiledProgram>) result).errors();
        assertThat(errors).isNotEmpty();
        return errors.first();
    }

    private static CompiledFunction compiledFunction(CompiledProgram program, String moduleName, String functionName) {
        return program.modules().stream()
                .filter(module -> module.name().equals(moduleName))
                .findFirst()
                .orElseThrow()
                .functions().stream()
                .filter(function -> function.name().equals(functionName))
                .findFirst()
                .orElseThrow();
    }
}
