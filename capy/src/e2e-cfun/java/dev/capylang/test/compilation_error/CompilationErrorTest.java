package dev.capylang.test.compilation_error;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.api.Test;
import dev.capylang.compiler.ImportDeclaration;
import dev.capylang.compiler.CapybaraCompiler;
import dev.capylang.compiler.CompiledProgram;
import dev.capylang.compiler.Result;
import dev.capylang.compiler.parser.RawModule;

import java.util.*;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertAll;

public class CompilationErrorTest {
    @Test
    void shouldKeepOriginalLocalNamesInUserVisibleErrors() {
        var errors = compileProgram("""
                        fun parse_semver(version: string): Result[int] =
                            data __Parse[T] { buffer: string, value: T }
                            fun __parse_digit(buffer: string): Result[__Parse[int]] =
                                Success { __Parse { buffer: buffer[1:], value: 1 } }
                            fun __parse_digits(parse: __Parse[Option[int]]): Result[__Parse[int]] =
                                match parse.buffer[0] with
                                case None ->
                                    match parse.value with
                                    case Some { value } -> Success { __Parse { '', value } }
                                    case None -> Error { 'Expected digit, but got end of version string!' }
                                case Some { next_char } ->
                                    let next_digit: Result[__Parse[int]] = __parse_digit(parse.buffer)
                                    match next_digit with
                                    case Error error ->
                                        match parse.value with
                                        case Some { value } -> Success { __Parse { parse.buffer[1:], value } }
                                        case None -> error
                                    case Success { next_digit_parse } ->
                                        let new_parse: __Parse[Option[int]] = __Parse {
                                            buffer: next_digit_parse.buffer,
                                            value:
                                                match parse.value with
                                                case Some { value } -> Some { value * 10 + next_digit_parse.value }
                                                case None -> Some { next_digit_parse.value }
                                        }
                                        __parse_digits(new_parse)
                            __parse_digits(__Parse { buffer: version, value: None })|version_parse =>
                                if version_parse.buffer==false.is_empty then Error { "Unexpected characters after patch version: `"+version_parse.buffer+"`!" } else Success { version_parse.value }
                        """,
                "local_names_in_errors");

        assertThat(errors).isNotEmpty();
        assertThat(errors.stream().map(Result.Error.SingleError::message))
                .allMatch(message -> !message.contains("__parse_semver__local_fun_"))
                .allMatch(message -> !message.contains("__parse_semver__local_type_"))
                .anyMatch(message -> message.contains("fun __parse_digits(parse: __Parse[Option[int]]): Result[__Parse[int]] ="));
    }

    @Test
    void shouldNormalizeLocalTypeNamesInMatchExhaustivenessErrors() {
        var errors = compileProgram("""
                        fun parse(value: int): int =
                            type __Token = __Number | Stop
                            data __Number { value: int }
                            single Stop
                            ---
                            let token: __Token = __Number { value }
                            match token with
                            case __Number { value } -> value
                        """,
                "local_match_not_exhaustive");

        assertThat(errors).hasSize(1);
        assertThat(errors.first().message())
                .contains("`match` is not exhaustive. Use wildcard `case _ -> ...` or add missing branches:`Stop`.")
                .doesNotContain("__local_type_")
                .doesNotContain("__local_single_");
    }

    @Test
    void shouldRejectParameterizedRuntimeTypePatterns() {
        var errors = compileProgram("""
                        fun classify(value: any): int =
                            match value with
                            case list[string] -> 1
                            case _ -> 0
                        """,
                "parameterized_runtime_type_pattern");

        assertThat(errors).hasSize(1);
        assertThat(errors.first().message())
                .contains("Parameterized runtime type patterns are not supported in v1; use bare `list`");
    }

    @Test
    void shouldRejectConstructorStarOutsideConstructor() {
        var errors = compileProgram("""
                        fun parse(): int =
                            * { value: 1 }
                        """,
                "constructor_star_outside_constructor");

        assertThat(errors).hasSize(1);
        assertThat(errors.first().message())
                .contains("`* { ... }` can only be used inside `with constructor`");
    }

    @Test
    void shouldNormalizeLocalTypeNamesInConstructorReturnErrors() {
        var errors = compileProgram("""
                        fun parse(value: int): int =
                            data __Token { value: int } with constructor {
                                value
                            }
                            ---
                            let token = __Token { value: value }
                            token.value
                        """,
                "local_constructor_invalid_return");

        assertThat(errors).hasSize(1);
        assertThat(errors.first().message())
                .contains("Constructor for `__Token` must return `__Token` or `Result[__Token]`")
                .doesNotContain("__local_type_");
    }

    @Test
    void shouldRequireResultDataConstructorWhenParentTypeConstructorReturnsResult() {
                var errors = compileProgram("""
                        from /capy/lang/Result import { * }
                        type Parent { foo: string } with constructor {
                           if foo.size == 0 then
                               Error { message: "missing" }
                           else
                               Success { value: * { foo: foo } }
                        } = Child
                        data Child { foo: string, bar: string } with constructor {
                            * { foo: foo, bar: bar }
                        }
                        fun create(foo: string, bar: string): string =
                            ""
                        """,
                "type_constructor_requires_result_child");

        assertThat(errors).hasSize(1);
        assertThat(errors.first().message())
                .contains("Constructor for `Child` must return `Result[Child]` because parent type constructor for `Parent` returns `Result[Parent]`");
    }

    @Test
    void shouldRejectConstructorBypassOutsideDeclaringModule() {
        var result = CapybaraCompiler.INSTANCE.compile(List.of(
                new RawModule("UserModel", "/foo/model", """
                        from /capy/lang/Result import { * }

                        data User { age: int } with constructor {
                            if age <= 0 then
                                Error { message: "invalid" }
                            else
                                Success { value: * { age: age } }
                        }
                        """),
                new RawModule("Consumer", "/foo/app", """
                        from /foo/model/UserModel import { * }

                        fun broken(): User = User! { age: 1 }
                        """)
        ), new TreeSet<>());

        assertThat(result).isInstanceOf(Result.Error.class);
        var error = ((Result.Error<CompiledProgram>) result).errors().first();
        assertThat(error.message())
                .contains("Constructor bypass `User! { ... }` can only be used in module `foo/model/UserModel` where `User` is defined");
    }

    @Test
    void shouldRejectConstructorBypassForNonResultConstructor() {
        var errors = compileProgram("""
                        data OddInt { value: int } with constructor {
                            if value % 2 == 0 then
                                * { value: value + 1 }
                            else
                                * { value: value }
                        }

                        fun broken(): OddInt = OddInt! { value: 1 }
                        """,
                "constructor_bypass_non_result");

        assertThat(errors).hasSize(1);
        assertThat(errors.first().message())
                .contains("Constructor bypass `!` is available only for data with Result-returning constructor");
    }

    @Test
    void shouldRejectUnknownDeriver() {
        var errors = compileProgram("""
                        data User { name: string } derive Missing
                        """,
                "derive_unknown_deriver");

        assertThat(errors).hasSize(1);
        assertThat(errors.first().message())
                .contains("Deriver `Missing` not found for `User`");
    }

    @Test
    void shouldRejectDerivedMethodCollision() {
        var errors = compileProgram("""
                        deriver Show {
                            fun show(): string = "generated"
                        }

                        data User { name: string } derive Show

                        fun User.show(): string = "manual"
                        """,
                "derive_method_collision");

        assertThat(errors).hasSize(1);
        assertThat(errors.first().message())
                .contains("Duplicate function signature `User.show`");
    }

    @Test
    void shouldRejectDuplicateDeriver() {
        var errors = compileProgram("""
                        deriver Show {
                            fun show(): string = "generated"
                        }

                        deriver Show {
                            fun print(): string = "generated"
                        }

                        data User { name: string } derive Show
                        """,
                "derive_duplicate_deriver");

        assertThat(errors).hasSize(1);
        assertThat(errors.first().message())
                .contains("Duplicate deriver `Show`");
    }

    @Test
    void shouldRejectLegacyDeriveTypeNameHelper() {
        var errors = compileProgram("""
                        deriver Show {
                            fun show(): string = derive_type_name()
                        }

                        data User { name: string } derive Show
                        """,
                "derive_type_name_legacy");

        assertThat(errors).hasSize(1);
        assertThat(errors.first().message())
                .contains("`derive_type_name()` has been replaced by `reflection(receiver)`");
    }

    @Test
    void shouldRejectLegacyDeriveFieldsJoinHelper() {
        var errors = compileProgram("""
                        deriver Show {
                            fun show(): string = derive_fields_join(", ")
                        }

                        data User { name: string } derive Show
                        """,
                "derive_fields_join_legacy");

        assertThat(errors).hasSize(1);
        assertThat(errors.first().message())
                .contains("`derive_fields_join(...)` has been replaced by `reflection(receiver)`")
                .contains("reflection(receiver).fields");
    }

    @Test
    void shouldRejectIndirectLegacyDeriveHelperReference() {
        var errors = compileProgram("""
                        deriver Show {
                            fun show(): string = (:derive_type_name)()
                        }

                        data User { name: string } derive Show
                        """,
                "derive_type_name_reference_legacy");

        assertThat(errors).hasSize(1);
        assertThat(errors.first().message())
                .contains("`derive_type_name()` has been replaced by `reflection(receiver)`");
    }

    @Test
    void shouldRejectMatchAssignedToIncompatibleTypedLet() {
        var errors = compileProgram("""
                        fun broken(): int =
                            let xs: list[() => int] =
                                match true with
                                case true -> [1]
                                case false -> [() => 2]
                            0
                        """,
                "typed_let_match_branch_mismatch");

        assertThat(errors).hasSize(1);
        assertThat(errors.first().message())
                .contains("Expected `nothing=>int`, got `int`");
    }

    @Test
    void shouldRejectBareDecimalAssignedToFloat() {
        var errors = compileProgram("""
                        fun broken(): float = 3.14
                        """,
                "bare_decimal_float");

        assertThat(errors).hasSize(1);
        assertThat(errors.first().message())
                .contains("Expected `float`, got `double`");
    }

    @Test
    void shouldRejectRecFunctionWithNonTailRecursiveCall() {
        var errors = compileProgram("""
                        fun rec sum(n: int): int = n + sum(n - 1)
                        """,
                "rec_function_non_tail_call");

        assertThat(errors).hasSize(1);
        assertThat(errors.first().message())
                .contains("Recursive call to `sum` is not in tail position");
    }

    @Test
    void shouldRejectRecFunctionWithoutSelfCall() {
        var errors = compileProgram("""
                        fun rec sum(n: int): int = n - 1
                        """,
                "rec_function_without_self_call");

        assertThat(errors).hasSize(1);
        assertThat(errors.first().message())
                .contains("`fun rec` function `sum` must call itself in tail position");
    }

    @Test
    void shouldRejectRecLocalFunctionWithNonTailRecursiveCall() {
        var errors = compileProgram("""
                        fun sum(n: int): int =
                            fun rec __sum(n: int): int = n + __sum(n - 1)
                            ---
                            __sum(n)
                        """,
                "rec_local_function_non_tail_call");

        assertThat(errors).hasSize(1);
        assertThat(errors.first().message())
                .contains("Recursive call to `sum` is not in tail position")
                .doesNotContain("__local_fun_");
    }

    @Test
    void shouldRejectRecLocalFunctionWithoutSelfCall() {
        var errors = compileProgram("""
                        fun sum(n: int): int =
                            fun rec __sum(n: int): int = n - 1
                            ---
                            __sum(n)
                        """,
                "rec_local_function_without_self_call");

        assertThat(errors).hasSize(1);
        assertThat(errors.first().message())
                .contains("`fun rec` function `sum` must call itself in tail position")
                .doesNotContain("__local_fun_");
    }

    @Test
    void shouldRejectLongReturnedWhereIntIsExpected() {
        var errors = compileProgram("""
                        fun broken(): int = 1L
                        """,
                "long_to_int_not_allowed");

        assertThat(errors).hasSize(1);
        assertThat(errors.first().message())
                .contains("Expected `int`, got `long`");
    }

    @Test
    void shouldRejectDoubleAssignedWhereFloatFieldIsExpected() {
        var errors = compileProgram("""
                        data Measurement { ratio: float }
                        fun broken(): Measurement = Measurement { ratio: 1.5 }
                        """,
                "double_to_float_not_allowed");

        assertThat(errors).hasSize(1);
        assertThat(errors.first().message())
                .contains("Expected `float`")
                .contains("double");
    }

    @Test
    void shouldRejectIncompatibleConcreteGenericArguments() {
        var errors = compileProgram("""
                        data Box[T] { value: T }
                        fun broken(): Box[int] =
                            let box: Box[bool] = Box { value: true }
                            box
                        """,
                "generic_type_argument_mismatch");

        assertThat(errors).hasSize(1);
        assertThat(errors.first().message())
                .contains("Box[bool]")
                .contains("Box[int]");
    }

    @Test
    void shouldRejectResultBindOnNonResultExpression() {
        var errors = compileProgram("""
                        fun broken(): int =
                            let value <- 1
                            value
                        """,
                "result_bind_non_result");

        assertThat(errors).hasSize(1);
        assertThat(errors.first().message())
                .contains("`<-` can only be used with `Result[...]` or `Effect[...]`")
                .contains("`INT`");
    }

    @Test
    void shouldRejectEffectBindInNonEffectContext() {
        var errors = compileProgram("""
                        from /capy/lang/Effect import { * }
                        fun broken(): int =
                            let value <- pure(1)
                            value
                        """,
                "effect_bind_non_effect_context");

        assertThat(errors).hasSize(1);
        assertThat(errors.first().message())
                .contains("Expected `int`, got `Effect`");
    }

    @Test
    void shouldRejectResultBindDeclaredTypeMismatch() {
        var errors = compileProgram("""
                        from /capy/lang/Result import { * }
                        fun broken(): Result[int] =
                            let value: string <- Success { value: 1 }
                            value
                        """,
                "result_bind_type_mismatch");

        assertThat(errors).hasSize(1);
        assertThat(errors.first().message())
                .contains("Expected `string`, got `int`");
    }

    @Test
    void shouldRejectResultBindInNonResultContext() {
        var errors = compileProgram("""
                        from /capy/lang/Result import { * }
                        fun broken(): int =
                            let value <- Success { value: 1 }
                            value
                        """,
                "result_bind_non_result_context");

        assertThat(errors).hasSize(1);
        assertThat(errors.first().message())
                .contains("Expected `int`, got `Result[T]`");
    }

    @Test
    void shouldRejectNestedResultPipeBeforeJavaGeneration() {
        var result = CapybaraCompiler.INSTANCE.compile(List.of(
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

                        fun assert_that(value: Result[T]): ResultAssert[T] = ResultAssert { value }
                        fun assert_that(value: data): DataAssert = DataAssert { value }
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

                        fun broken(): ResultAssert[Widget] =
                            assert_that(Success { value: Widget { size: 1 } } | widget => Success { value: widget.wrap() })
                                .is_equal_to(Success { value: Widget { size: 2 } })
                        """)
        ), new java.util.TreeSet<>());

        assertThat(result).isInstanceOf(Result.Error.class);
        var errors = ((Result.Error<CompiledProgram>) result).errors();
        assertThat(errors).hasSize(1);
        assertThat(errors.first().message())
                .contains("Expected `Widget`, got `Success`");
    }

    @ParameterizedTest(name = "{index}: should fail when compiling `{0}.cfun`")
    @MethodSource
    void compilationError(
            String moduleName,
            String code,
            Position expectedPosition,
            String errorMessage) {
        // given
        var finalErrorMessage = errorMessage.formatted(expectedPosition.line(), expectedPosition.column(), " ".repeat(expectedPosition.column()));

        // when
        var errors = compileProgram(code, moduleName);

        // then
        assertThat(errors).hasSize(1);
        var error = errors.first();
        assertAll("Assertions on error",
                () -> assertThat(error.line()).isEqualTo(expectedPosition.line()),
                () -> assertThat(error.column()).isEqualTo(expectedPosition.column()),
                () -> assertThat(error.file()).isEqualTo("/foo/boo/%s.cfun".formatted(moduleName)),
                () -> assertThat(error.message()).isEqualTo(finalErrorMessage));
    }

    @ParameterizedTest(name = "{index}: should fail when compiling `{0}.cfun` (imports flavour)")
    @MethodSource
    void compilationErrorWithImports(
            String moduleName,
            String code,
            Position expectedPosition,
            String errorMessage,
            List<ImportDeclaration> imports) {
        // given
        var finalErrorMessage = errorMessage.formatted(expectedPosition.line(), expectedPosition.column(), " ".repeat(expectedPosition.column()));

        // when
        var errors = compileProgram(code, moduleName, imports);

        // then
        assertThat(errors).hasSize(1);
        var error = errors.first();
        assertAll("Assertions on error",
                () -> assertThat(error.line()).isEqualTo(expectedPosition.line()),
                () -> assertThat(error.column()).isEqualTo(expectedPosition.column()),
                () -> assertThat(error.file()).isEqualTo("/foo/boo/%s.cfun".formatted(moduleName)),
                () -> assertThat(error.message()).isEqualTo(finalErrorMessage));
    }

    @SuppressWarnings("unchecked")
    static Stream<Arguments> compilationError() {
        return concat(simpleCompilationError(), multilineCompilationError(), infixOperations(), bitwiseOperations(), matchCompilationErrors());
    }

    static Stream<Arguments> compilationErrorWithImports() {
        return Stream.of(
                Arguments.of(
                        "infix_operator_outside_of_package",
                        """
                                fun Name[T].foo(map: T => Name[Y]): Name[Y] =
                                   match this with
                                   case Boo e -> e
                                   case Foo s -> map(s.value)
                                """,
                        new Position(3, 14),
                        """
                                error: mismatched types
                                 --> /foo/boo/infix_operator_outside_of_package.cfun:%d:%d
                                fun Name[T].foo(map: T => Name[Y]): Name[Y] =
                                            ^ Cannot declare method on external type `Name`. Type methods/infix operators must be declared in the module where the type is defined.
                                """,
                        List.of(new ImportDeclaration("/capy/compilation_test/Name", List.of("Name", "Foo", "Boo"), List.of()))
                ),
                Arguments.of(
                        "infix_special_operator_outside_of_package",
                        """
                                fun Name[T].`+`(map: T => Name[Y]): Name[Y] =
                                   match this with
                                   case Boo e -> e
                                   case Foo s -> map(s.value)
                                """,
                        new Position(3, 13),
                        """
                                error: mismatched types
                                 --> /foo/boo/infix_special_operator_outside_of_package.cfun:%d:%d
                                fun Name[T].`+`(map: T => Name[Y]): Name[Y] =
                                             ^ Cannot declare method on external type `Name`. Type methods/infix operators must be declared in the module where the type is defined.
                                """,
                        List.of(new ImportDeclaration("/capy/compilation_test/Name", List.of("Name", "Foo", "Boo"), List.of()))
                )
        );
    }

    static Stream<Arguments> matchCompilationErrors() {
        var base = Stream.of(
                Arguments.of(
                        "match_not_exhaustive",
                        """
                                type Letter = A | B | C | D
                                data A { a: int }
                                data B { b: int }
                                data C { c: int }
                                data D { d: int }
                                fun foo(letter: Letter): int =
                                    match letter with
                                    case A { a } -> a
                                    case B { b } -> b
                                """,
                        new Position(7, 4),
                        """
                                error: mismatched types
                                 --> /foo/boo/match_not_exhaustive.cfun:7:4
                                fun foo(letter: Letter): int =
                                    match letter with
                                    ^ `match` is not exhaustive. Use wildcard `case _ -> ...` or add missing branches:`C`, `D`.
                                """
                ),
                Arguments.of(
                        "match_enum_not_exhaustive",
                        """
                                enum Color { RED, BLUE, GREEN_BLUE }
                                fun foo(color: Color): int =
                                    match color with
                                    case RED -> 1
                                    case BLUE -> 2
                                """,
                        new Position(3, 4),
                        """
                                error: mismatched types
                                 --> /foo/boo/match_enum_not_exhaustive.cfun:3:4
                                fun foo(color: Color): int =
                                    match color with
                                    ^ `match` is not exhaustive. Use wildcard `case _ -> ...` or add missing branches:`GREEN_BLUE`.
                                """
                ),
                Arguments.of(
                        "match_guard_not_bool",
                        """
                                from /capy/lang/Option import { * }
                                fun foo(option: Option[int]): string =
                                    match option with
                                    case Some { value } when value + 1 -> "x"
                                    case None -> "none"
                                """,
                        new Position(4, 35),
                        """
                                error: mismatched types
                                 --> /foo/boo/match_guard_not_bool.cfun:4:35
                                fun foo(option: Option[int]): string =
                                    match option with ...
                                %3$s^ `when` guard has to be `bool`, was `INT`
                                """
                ),
                Arguments.of(
                        "match_guard_does_not_make_case_exhaustive",
                        """
                                type Letter = A | B
                                data A { a: int }
                                data B { b: int }
                                fun foo(letter: Letter): int =
                                    match letter with
                                    case A { a } when a > 0 -> a
                                    case B { b } -> b
                                """,
                        new Position(5, 4),
                        """
                                error: mismatched types
                                 --> /foo/boo/match_guard_does_not_make_case_exhaustive.cfun:5:4
                                fun foo(letter: Letter): int =
                                    match letter with
                                    ^ `match` is not exhaustive. Use wildcard `case _ -> ...` or add missing branches:`A`.
                                """
                ),
                Arguments.of(
                        "match_guard_where_keyword_syntax_error",
                        """
                                from /capy/lang/Option import { * }
                                fun foo(option: Option[string]): string =
                                    match option with
                                    case Some { value } where value == "+" -> "plus"
                                    case Some { value } -> "other"
                                    case None -> "none"
                                """,
                        new Position(4, 24),
                        """
                                error: mismatched types
                                 --> /foo/boo/match_guard_where_keyword_syntax_error.cfun:4:24
                                fun foo(option: Option[string]): string =
                                    match option with
                                    case Some { value } where value == "+" -> "plus"
                                %3$s^ Syntax error, `where` not expected here. Use `when` for match guards
                                """
                ),
                Arguments.of(
                        "match_result_branch_type_mismatch_to_int",
                        """
                                from /capy/lang/Option import { * }
                                from /capy/lang/Result import { * }
                                fun foo(v: string): Result[int] =
                                    match v[0] with
                                    case None -> Error { "missing" }
                                    case Some { value } -> value
                                """,
                        new Position(6, 27),
                        """
                                error: mismatched types
                                 --> /foo/boo/match_result_branch_type_mismatch_to_int.cfun:6:27
                                fun foo(v: string): Result[int] =
                                    match v[0] with ...
                                                           ^ Cannot coerce constructor result to `INT`
                                """
                ),
                Arguments.of(
                        "match_option_branch_type_mismatch",
                        """
                                from /capy/lang/Option import { * }
                                fun foo(v: string): Option[string] =
                                    match v[0] with
                                    case None -> None
                                    case Some { value } -> value + ""
                                """,
                        new Position(5, 33),
                        """
                                error: mismatched types
                                 --> /foo/boo/match_option_branch_type_mismatch.cfun:5:33
                                fun foo(v: string): Option[string] =
                                    match v[0] with ...
                                                                 ^ Expected `Option[string]`, got `string`
                                """
                ),
                Arguments.of(
                        "match_bool_not_exhaustive",
                        """
                                fun foo(x: bool): int =
                                    match x with
                                    case true -> 1
                                """,
                        new Position(2, 4),
                        """
                                error: mismatched types
                                 --> /foo/boo/match_bool_not_exhaustive.cfun:2:4
                                fun foo(x: bool): int =
                                    match x with
                                    ^ `match` is not exhaustive. Use wildcard `case _ -> ...` or add missing branches:`false`.
                                """
                ));
        var primitiveTypes = List.of("string", "byte", "int", "long", "float", "double");
        var primitiveCases = primitiveTypes.stream().map(CompilationErrorTest::primitiveMatchNotExhaustiveCase);
        return Stream.concat(base, primitiveCases);
    }

    private static Arguments primitiveMatchNotExhaustiveCase(String primitive) {
        var moduleName = "match_%s".formatted(primitive);
        var code = """
                fun foo(x: %s): int =
                    match x with
                    case %s a -> 1
                    case %s b -> 2
                """.formatted(primitive, primitive, primitive);
        var message = """
                error: mismatched types
                 --> /foo/boo/%s.cfun:2:4
                fun foo(x: %s): int =
                    match x with
                    ^ `match` is not exhaustive. Use wildcard `case _ -> ...`.
                """.formatted(moduleName, primitive);
        return Arguments.of(moduleName, code, new Position(2, 4), message);
    }

    static Stream<Arguments> infixOperations() {
        var operators = List.of("+", "-", "*", "^", "/");
        var numericTypes = List.of(
                new InfixOperand("byte", "byte", "byte"),
                new InfixOperand("int", "int", "int"),
                new InfixOperand("long", "long", "long"),
                new InfixOperand("float", "float", "float"),
                new InfixOperand("double", "double", "double")
        );
        var primitiveTypes = List.of(
                new InfixOperand("bool", "bool", "bool"),
                new InfixOperand("byte", "byte", "byte"),
                new InfixOperand("int", "int", "int"),
                new InfixOperand("long", "long", "long"),
                new InfixOperand("float", "float", "float"),
                new InfixOperand("double", "double", "double")
        );
        var anyType = new InfixOperand("any", "any", "any");
        var byModule = new LinkedHashMap<String, Arguments>();

        for (var op : operators) {
            for (var right : primitiveTypes) {
                add(byModule, op, new InfixOperand("bool", "bool", "bool"), right);
                add(byModule, op, right, new InfixOperand("bool", "bool", "bool"));
            }

            if (!op.equals("+")) {
                for (var left : numericTypes) {
                    add(byModule, op, left, new InfixOperand("string", "string", "string"));
                }
                add(byModule, op, new InfixOperand("string", "string", "string"), new InfixOperand("string", "string", "string"));
            }
            add(byModule, op, anyType, anyType);
        }

        byModule.putIfAbsent(
                "infix_plus_data_parent_and_subtype",
                Arguments.of(
                        "infix_plus_data_parent_and_subtype",
                        """
                                type Json = JsonArray | JsonNull
                                data JsonArray { value: list[Json] }
                                single JsonNull
                                fun foo(left: JsonArray, right: Json): Json =
                                    left + right
                                """,
                        new Position(5, 9),
                        """
                                error: mismatched types
                                 --> /foo/boo/infix_plus_data_parent_and_subtype.cfun:5:9
                                fun foo(left: JsonArray, right: Json): Json = left + right
                                         ^ `+` operator is not defined for `JsonArray + Json`
                                """
                )
        );

        return byModule.values().stream();
    }

    static Stream<Arguments> bitwiseOperations() {
        var nonIntPrimitives = List.of(
                new InfixOperand("byte", "byte", "byte"),
                new InfixOperand("long", "long", "long"),
                new InfixOperand("float", "float", "float"),
                new InfixOperand("double", "double", "double"),
                new InfixOperand("bool", "bool", "bool"),
                new InfixOperand("string", "string", "string")
        );
        var byModule = new LinkedHashMap<String, Arguments>();
        var intType = new InfixOperand("int", "int", "int");

        for (var op : List.of(".and.", ".nand.", ".or.", ".xor.")) {
            for (var t : nonIntPrimitives) {
                addBitwiseInfix(byModule, op, intType, t);
                addBitwiseInfix(byModule, op, t, intType);
            }
        }

        for (var t : nonIntPrimitives) {
            addBitwiseNot(byModule, t);
        }

        return byModule.values().stream();
    }

    private static void addBitwiseInfix(Map<String, Arguments> byModule, String op, InfixOperand left, InfixOperand right) {
        var module = "bitwise_%s_%s_%s".formatted(bitwiseOpName(op), left.id(), right.id());
        byModule.putIfAbsent(module, bitwiseInfixCase(module, op, left, right));
    }

    private static void addBitwiseNot(Map<String, Arguments> byModule, InfixOperand operand) {
        var module = "bitwise_not_%s".formatted(operand.id());
        byModule.putIfAbsent(module, bitwiseNotCase(module, operand));
    }

    private static Arguments bitwiseInfixCase(String moduleName, String op, InfixOperand left, InfixOperand right) {
        var code = "fun foo(left: %s, right: %s) = left %s right".formatted(left.decl(), right.decl(), op);
        var column = code.indexOf(op);
        var pointer = " ".repeat(column)
                      + "^ `%s` operator is not defined for `%s %s %s`".formatted(op, left.shownType(), op, right.shownType());
        var errorMessage = "error: mismatched types\n"
                           + " --> /foo/boo/%s.cfun:1:%d\n".formatted(moduleName, column)
                           + code + "\n"
                           + pointer + "\n";
        return Arguments.of(moduleName, code, new Position(1, column), errorMessage);
    }

    private static Arguments bitwiseNotCase(String moduleName, InfixOperand operand) {
        var code = "fun foo(value: %s) = .not.value".formatted(operand.decl());
        var renderedCode = "fun foo(value: %s) = value .not. 0".formatted(operand.decl());
        var column = code.indexOf(".not.");
        var pointer = " ".repeat(column)
                      + "^ `.not.` operator is not defined for `%s .not. int`".formatted(operand.shownType());
        var errorMessage = "error: mismatched types\n"
                           + " --> /foo/boo/%s.cfun:1:%d\n".formatted(moduleName, column)
                           + renderedCode + "\n"
                           + pointer + "\n";
        return Arguments.of(moduleName, code, new Position(1, column), errorMessage);
    }

    private static void add(Map<String, Arguments> byModule, String op, InfixOperand left, InfixOperand right) {
        var module = "infix_%s_%s_%s".formatted(opName(op), left.id(), right.id());
        byModule.putIfAbsent(module, infixCase(module, op, left, right));
    }

    private static Arguments infixCase(String moduleName, String op, InfixOperand left, InfixOperand right) {
        var code = "fun foo(left: %s, right: %s) = left %s right".formatted(left.decl(), right.decl(), op);
        var column = code.indexOf(op);
        var pointer = " ".repeat(column)
                      + "^ `%s` operator is not defined for `%s %s %s`".formatted(op, left.shownType(), op, right.shownType());
        var errorMessage = "error: mismatched types\n"
                           + " --> /foo/boo/%s.cfun:1:%d\n".formatted(moduleName, column)
                           + code + "\n"
                           + pointer + "\n";
        return Arguments.of(moduleName, code, new Position(1, column), errorMessage);
    }

    private static String opName(String op) {
        return switch (op) {
            case "+" -> "plus";
            case "-" -> "minus";
            case "*" -> "mul";
            case "^" -> "pow";
            case "/" -> "div";
            default -> op;
        };
    }

    private static String bitwiseOpName(String op) {
        return switch (op) {
            case ".and." -> "and";
            case ".nand." -> "nand";
            case ".or." -> "or";
            case ".xor." -> "xor";
            default -> op;
        };
    }

    static Stream<Arguments> simpleCompilationError() {
        return Stream.of(
                Arguments.of(
                        "empty_data_declaration",
                        "data Empty {}",
                        new Position(1, 0),
                        """
                                error: mismatched types
                                 --> /foo/boo/empty_data_declaration.cfun:%d:%d
                                data Empty {}
                                ^ Data `Empty` must declare at least one field; use `single` for empty values
                                """
                ),
                Arguments.of(
                        "empty_local_data_declaration",
                        """
                                fun parse(): int =
                                    data __Empty {}
                                    ---
                                    1
                                """,
                        new Position(2, 4),
                        """
                                error: mismatched types
                                 --> /foo/boo/empty_local_data_declaration.cfun:%d:%d
                                fun parse(): int =
                                    data __Empty {}
                                %s^ Data `__Empty` must declare at least one field; use `single` for empty values
                                """
                ),
                Arguments.of(
                        "data_extension_conflicting_inherited_field_types",
                        """
                                data Foo { a: int, b: int }
                                data Boo { b: string, c: int }
                                data Bar { d: int, ...Foo, ...Boo }
                                """,
                        new Position(3, 0),
                        "Conflicting inherited field `b` for data `Bar`: `Foo.b` has type `INT` and `Boo.b` has type `STRING`"
                ),
                Arguments.of(
                        "parser_syntax_error_missing_brace_in_then_branch",
                        """
                                type Seq[T] = Cons[T] | End
                                data Cons[T] { value: T, rest: Seq[T] }
                                single End
                                fun to_seq(list: list[int]): Seq[int] =
                                    if list.size > 0
                                    then Cons { list[0], to_seq(list[1:])
                                    else End
                                """,
                        new Position(7, 4),
                        """
                                error: mismatched types
                                 --> /foo/boo/parser_syntax_error_missing_brace_in_then_branch.cfun:%d:%d
                                type Seq[T] = Cons[T] | End
                                data Cons[T] { value: T, rest: Seq[T] }
                                single End
                                fun to_seq(list: list[int]): Seq[int] =
                                    if list.size > 0
                                    then Cons { list[0], to_seq(list[1:])
                                    else End
                                    ^ Syntax error near `iflist.size>0thenCons{list[0],to_seq(list[1:])else`
                                """
                ),
                Arguments.of(
                        "lambda_outside_pipe_reports_clear_error",
                        "fun foo() = x => x",
                        new Position(1, 12),
                        """
                                error: mismatched types
                                 --> /foo/boo/lambda_outside_pipe_reports_clear_error.cfun:%d:%d
                                fun foo() = x => x
                                            ^ Lambda expression with parameters requires expected function type
                                """
                ),
                Arguments.of(
                        "pipe_lambda_postfix_requires_parentheses",
                        """
                                fun foo(): string =
                                    " x "
                                        | value => value.trim()
                                        .trim()
                                """,
                        new Position(4, 8),
                        """
                                error: mismatched types
                                 --> /foo/boo/pipe_lambda_postfix_requires_parentheses.cfun:%d:%d
                                fun foo(): string =
                                    " x "
                                        | value => value.trim()
                                        .trim()
                                %s^ Parenthesize lambda before chaining postfix operations
                                """
                ),
                Arguments.of(
                        "seq_match_case_wrong_arrow",
                        """
                                type Seq[T] = Cons[T] | End
                                data Cons[T] { value: T, rest: Seq[T] }
                                single End
                                fun Seq[T].take(n: int): list[T] =
                                    match this with
                                    case End => []
                                    case Cons { value, rest } -> []
                                """,
                        new Position(6, 13),
                        """
                                error: mismatched types
                                 --> /foo/boo/seq_match_case_wrong_arrow.cfun:%d:%d
                                type Seq[T] = Cons[T] | End
                                data Cons[T] { value: T, rest: Seq[T] }
                                single End
                                fun Seq[T].take(n: int): list[T] =
                                    match this with
                                    case End => []
                                %3$s^ Expected `->`, found `=`
                                """
                ),
                Arguments.of(
                        "missing_generic_type_closing_bracket_in_let_annotation",
                        """
                                fun parse(): int =
                                    let new_parse: __Parse[Option[int] = __Parse {
                                        value: None
                                    }
                                    1
                                """,
                        new Position(2, 39),
                        """
                                error: mismatched types
                                 --> /foo/boo/missing_generic_type_closing_bracket_in_let_annotation.cfun:%d:%d
                                fun parse(): int =
                                    let new_parse: __Parse[Option[int] = __Parse {
                                %s^ Expected `]`, found `=`
                                """
                ),
                Arguments.of(
                        "data_type_not_found_sqe",
                        """
                                type Seq[T] = Cons[T] | End
                                data Cons[T] { value: T, rest: Sqe[T] }
                                single End
                                """,
                        new Position(2, 31),
                        """
                                error: mismatched types
                                 --> /foo/boo/data_type_not_found_sqe.cfun:%d:%d
                                data Cons[T] { value: T, rest: Sqe[T] }
                                                               ^ Data type `Sqe` not found
                                """
                ),
                Arguments.of(
                        "function_unknown_return_type",
                        """ 
                                fun broken(x: int): Str =
                                    if x > 0
                                    then "+"
                                    else "-"
                                """,
                        new Position(1, 20),
                        """
                                error: mismatched types
                                 --> /foo/boo/function_unknown_return_type.cfun:1:20
                                fun broken(x: int): Str =
                                                    ^ Data type `Str` not found
                                """
                ),
                Arguments.of(
                        "function_variable_not_found",
                        "fun foo(): int = missing",
                        new Position(1, "fun foo(): int = "),
                        """
                                error: mismatched types
                                 --> /foo/boo/function_variable_not_found.cfun:1:17
                                fun foo(): int = missing
                                                 ^ Variable missing not found
                                """
                ),
                Arguments.of(
                        "function_field_access_requires_data_type",
                        "fun foo(): int = \"abc\".foo",
                        new Position(1, "fun foo(): int = "),
                        """
                                error: mismatched types
                                 --> /foo/boo/function_field_access_requires_data_type.cfun:1:17
                                fun foo(): int = "abc".foo
                                                 ^ Field `foo` not found in type `STRING`
                                """
                ),
                Arguments.of(
                        "match_case_wrong_operator",
                        """ 
                                fun broken(result: /capy/lang/Result.Result[any]): bool =
                                    match result with
                                    case Error _ = true
                                    case Success s -> false
                                """,
                        new Position(3, 17),
                        """
                                error: mismatched types
                                 --> /foo/boo/match_case_wrong_operator.cfun:3:17
                                fun broken(result: /capy/lang/Result.Result[any]): bool =
                                    match result with
                                    case Error _ = true
                                %3$s^ Expected `->`, found `=`
                                """
                ),
                Arguments.of(
                        "function_call_reports_deepest_argument_error",
                        """
                                data JsonNumberLong { value: long }
                                fun succeeded(value: JsonNumberLong): int = 1
                                fun foo(): int = succeeded(JsonNumberLong { value: 5.5 })
                                """,
                        new Position(3, "fun foo(): int = succeeded(JsonNumberLong { value: "),
                        """
                                error: mismatched types
                                 --> /foo/boo/function_call_reports_deepest_argument_error.cfun:%d:%d
                                fun foo(): int = succeeded(JsonNumberLong { value: 5.5 })
                                                                                   ^ Expected `long`, but got `double`
                                """
                ),
                Arguments.of(
                        "function_json_null_wrong_generic_return_type",
                        """
                                type Result[T] = Success[T] | Error
                                data Success[T] { value: T }
                                data Error { message: string }
                                data _Parse[T] { value: T, parsing_string: string }
                                data JsonBool { value: bool }
                                single JsonNull
                                  fun _deserialize_json_null(json: string): Result[_Parse[JsonBool]] =
                                    let parsed: _Parse[JsonNull] = _Parse { JsonNull, json[4:] }
                                    Success { parsed }
                                """,
                        new Position(9, 4),
                        "error: mismatched types\n"
                        + " --> /foo/boo/function_json_null_wrong_generic_return_type.cfun:%d:%d\n"
                        + "fun _deserialize_json_null(json: string): Result[_Parse[JsonBool]] =\n"
                        + "    let parsed: _Parse[JsonNull] = _Parse {\n"
                        + "        JsonNull(),\n"
                        + "    ^ Expected `Result`, got `Success`\n"
                ),
                Arguments.of(
                        "json_assertion_no_viable_alternative",
                        """
                                data JsonNumberLong { value: long }
                                data JsonObject { value: dict[JsonNumberLong] }
                                fun foo(): JsonObject =
                                    JsonObject {
                                        value: {
                                            "int": JsonNumberLong { value: 5.5 },
                                            "int_minus": JsonNumberLong { value: -5.5 },
                                            "long": JsonNumberLong { value: 97387717187.4 },
                                            "long_minus": JsonNumberLong { value: -973877177.4 },
                                        }
                                    }
                                """,
                        new Position(6, 43),
                        """
                                error: mismatched types
                                 --> /foo/boo/json_assertion_no_viable_alternative.cfun:%d:%d
                                fun foo(): JsonObject =
                                    JsonObject {
                                        value: {
                                            "int": JsonNumberLong { value: 5.5 },
                                                                           ^ Expected `long`, but got `double`
                                """
                ),
                Arguments.of(
                        "json_assertion_int_no_viable_alternative",
                        """
                                data JsonNumberInt { value: int }
                                data JsonObject { value: dict[JsonNumberInt] }
                                fun foo(): JsonObject =
                                    JsonObject {
                                        value: {
                                            "int": JsonNumberInt { value: 5.5 },
                                            "int_minus": JsonNumberInt { value: -5.5 },
                                        }
                                    }
                                """,
                        new Position(6, "            \"int\": JsonNumberInt { value: "),
                        """
                                error: mismatched types
                                 --> /foo/boo/json_assertion_int_no_viable_alternative.cfun:%d:%d
                                fun foo(): JsonObject =
                                    JsonObject {
                                        value: {
                                            "int": JsonNumberInt { value: 5.5 },
                                                                          ^ Expected `int`, but got `double`
                                """
                ),
                Arguments.of(
                        "json_assertion_string_no_viable_alternative",
                        """
                                data JsonString { value: string }
                                data JsonObject { value: dict[JsonString] }
                                fun foo(): JsonObject =
                                    JsonObject {
                                        value: {
                                            "name": JsonString { value: 123 },
                                            "surname": JsonString { value: 456 },
                                        }
                                    }
                                """,
                        new Position(6, 40),
                        """
                                error: mismatched types
                                 --> /foo/boo/json_assertion_string_no_viable_alternative.cfun:%d:%d
                                fun foo(): JsonObject =
                                    JsonObject {
                                        value: {
                                            "name": JsonString { value: 123 },
                                                                        ^ Expected `string`, but got `int`
                                """
                ),
                Arguments.of(
                        "json_assertion_bool_no_viable_alternative",
                        """
                                data JsonBool { value: bool }
                                data JsonObject { value: dict[JsonBool] }
                                fun foo(): JsonObject =
                                    JsonObject {
                                        value: {
                                            "married": JsonBool { value: "yes" },
                                            "active": JsonBool { value: "no" },
                                        }
                                    }
                                """,
                        new Position(6, 41),
                        """
                                error: mismatched types
                                 --> /foo/boo/json_assertion_bool_no_viable_alternative.cfun:%d:%d
                                fun foo(): JsonObject =
                                    JsonObject {
                                        value: {
                                            "married": JsonBool { value: "yes" },
                                                                         ^ Expected `bool`, but got `string`
                                """
                ),
                Arguments.of(
                        "json_assertion_list_no_viable_alternative",
                        """
                                data JsonListInt { value: list[int], size: int }
                                data JsonObject { value: dict[JsonListInt] }
                                fun foo(v: list[int]): JsonObject =
                                    JsonObject {
                                        value: {
                                            "numbers": JsonListInt { value: v, size: "x" },
                                        }
                                    }
                                """,
                        new Position(6, "            \"numbers\": JsonListInt { value: v, size: "),
                        """
                                error: mismatched types
                                 --> /foo/boo/json_assertion_list_no_viable_alternative.cfun:%d:%d
                                fun foo(v: list[int]): JsonObject =
                                    JsonObject {
                                        value: {
                                            "numbers": JsonListInt { value: v, size: "x" }
                                                                                     ^ Expected `int`, but got `string`
                                """
                ),
                Arguments.of(
                        "json_assertion_set_no_viable_alternative",
                        """
                                data JsonSetBool { value: set[bool], ok: bool }
                                data JsonObject { value: dict[JsonSetBool] }
                                fun foo(v: set[bool]): JsonObject =
                                    JsonObject {
                                        value: {
                                            "flags": JsonSetBool { value: v, ok: "yes" },
                                        }
                                    }
                                """,
                        new Position(6, "            \"flags\": JsonSetBool { value: v, ok: "),
                        """
                                error: mismatched types
                                 --> /foo/boo/json_assertion_set_no_viable_alternative.cfun:%d:%d
                                fun foo(v: set[bool]): JsonObject =
                                    JsonObject {
                                        value: {
                                            "flags": JsonSetBool { value: v, ok: "yes" }
                                                                                 ^ Expected `bool`, but got `string`
                                """
                ),
                Arguments.of(
                        "json_assertion_dict_no_viable_alternative",
                        """
                                data JsonByte { value: byte }
                                data JsonObject { value: dict[JsonByte] }
                                fun foo(): JsonObject =
                                    JsonObject {
                                        value: {
                                            "tiny": JsonByte { value: "7" },
                                        }
                                    }
                                """,
                        new Position(6, "            \"tiny\": JsonByte { value: "),
                        """
                                error: mismatched types
                                 --> /foo/boo/json_assertion_dict_no_viable_alternative.cfun:%d:%d
                                fun foo(): JsonObject =
                                    JsonObject {
                                        value: {
                                            "tiny": JsonByte { value: "7" }
                                                                      ^ Expected `byte`, but got `string`
                                """
                ),
                Arguments.of(
                        "json_assertion_primitive_no_viable_alternative",
                        """
                                data JsonByte { value: byte }
                                data JsonObject { value: dict[JsonByte] }
                                fun foo(): JsonObject =
                                    JsonObject {
                                        value: {
                                            "tiny": JsonByte { value: "7" },
                                        }
                                    }
                                """,
                        new Position(6, "            \"tiny\": JsonByte { value: "),
                        """
                                error: mismatched types
                                 --> /foo/boo/json_assertion_primitive_no_viable_alternative.cfun:%d:%d
                                fun foo(): JsonObject =
                                    JsonObject {
                                        value: {
                                            "tiny": JsonByte { value: "7" }
                                                                      ^ Expected `byte`, but got `string`
                                """
                ),
                Arguments.of(
                        "function_wrong_return_type",
                        "fun foo(x: int): int = \"boo\"",
                        new Position(1, 23),
                        """
                                error: mismatched types
                                 --> /foo/boo/function_wrong_return_type.cfun:1:23
                                fun foo(x: int): int = "boo"
                                                       ^ Expected `int`, got `string`
                                """
                ),
                Arguments.of(
                        "function_wrong_return_type_bool",
                        "fun foo(x: int): int = true",
                        new Position(1, 23),
                        """
                                error: mismatched types
                                 --> /foo/boo/function_wrong_return_type_bool.cfun:1:23
                                fun foo(x: int): int = true
                                                       ^ Expected `int`, got `bool`
                                """
                ),
                Arguments.of(
                        "function_wrong_return_type_long",
                        "fun foo(x: int): int = 1L",
                        new Position(1, 23),
                        """
                                error: mismatched types
                                 --> /foo/boo/function_wrong_return_type_long.cfun:1:23
                                fun foo(x: int): int = 1L
                                                       ^ Expected `int`, got `long`
                                """
                ),
                Arguments.of(
                        "function_wrong_return_type_float",
                        "fun foo(x: int): int = 1.5f",
                        new Position(1, 23),
                        """
                                error: mismatched types
                                 --> /foo/boo/function_wrong_return_type_float.cfun:1:23
                                fun foo(x: int): int = 1.5f
                                                       ^ Expected `int`, got `float`
                                """
                ),
                Arguments.of(
                        "function_wrong_return_type_string",
                        "fun foo(x: int): string = 1",
                        new Position(1, 26),
                        """
                                error: mismatched types
                                 --> /foo/boo/function_wrong_return_type_string.cfun:1:26
                                fun foo(x: int): string = 1
                                                          ^ Expected `string`, got `int`
                                """
                ),
                Arguments.of(
                        "function_index_literal_of_of_range",
                        "fun foo(x: int) = x + 97387717187",
                        new Position(1, 26),
                        """
                                error: mismatched types
                                 --> /foo/boo/function_index_literal_of_of_range.cfun:1:22
                                fun foo(x: int) = x + 97387717187
                                                      ^ Int literal out of range: `97387717187`
                                """
                ),
                Arguments.of(
                        "function_private_type_escapes_signature",
                        """
                                fun foo_me(name: string): __Name =
                                    type __Name = __Foo | __Boo | __Unknown
                                    data __Foo { foo: string }
                                    data __Boo { boo: string }
                                    data __Unknown { unkn: string }
                                    ---
                                    match name with
                                    case "foo" -> __Foo { foo: "xyz" }
                                    case "boo" -> __Boo { boo: "xyz" }
                                    case _ -> __Unknown { unkn: name }
                                """,
                        new Position(1, 26),
                        """
                                error: mismatched types
                                 --> /foo/boo/function_private_type_escapes_signature.cfun:1:26
                                fun foo_me(name: string): __Name = ...
                                                          ^ Private type `__Name` cannot be used in function signature
                                """
                ),
                Arguments.of(
                        "local_function_error_restores_private_names",
                        """
                                    fun parse_semver(version: string): int =
                                        data __Parse[T] { value: T }
                                        fun parse_digits(parse: __Parse[Option[int]]): __Parse[int] = parse.value
                                        ---
                                        0
                                """,
                        new Position(3, 70),
                        """
                                error: mismatched types
                                 --> /foo/boo/local_function_error_restores_private_names.cfun:%d:%d
                                fun parse_digits(parse: __Parse[Option[int]]): __Parse[int] = parse.value
                                %3$s^ Expected `__Parse[int]`, got `Option[int]`
                                """
                )
        );
    }

    static Stream<Arguments> multilineCompilationError() {
        return Stream.of(
                Arguments.of(
                        "multiline_function_wrong_return_type",
                        """
                                // normal comment
                                data Foo {
                                    option: string,
                                    x: int
                                }
                                
                                /// some documentation
                                /// line 2 of doc
                                fun foo(x: int): int =
                                    let x = Foo {
                                        "boo",
                                        5
                                    }
                                    "boo"
                                """,
                        new Position(14, 4),
                        """
                                error: mismatched types
                                 --> /foo/boo/multiline_function_wrong_return_type.cfun:14:4
                                fun foo(x: int): int =
                                    let x = Foo {
                                        "boo",
                                        5
                                    }
                                    "boo"
                                    ^ Expected `int`, got `string`
                                """
                ),
                Arguments.of(
                        "multiline_let_chain_wrong_return_type",
                        """
                                /// docs
                                fun foo(x: int): int =
                                    let a = x + 1
                                    let b = a + 2
                                    "x"
                                """,
                        new Position(5, 4),
                        """
                                error: mismatched types
                                 --> /foo/boo/multiline_let_chain_wrong_return_type.cfun:5:4
                                fun foo(x: int): int =
                                    let a = x+1
                                    let b = a+2
                                    "x"
                                    ^ Expected `int`, got `string`
                                """
                ),
                Arguments.of(
                        "multiline_data_return_wrong_type",
                        """
                                data Bar { value: int }
                                
                                fun foo(x: int): Bar =
                                    let y = x + 1
                                    y
                                """,
                        new Position(5, 4),
                        """
                                error: mismatched types
                                 --> /foo/boo/multiline_data_return_wrong_type.cfun:5:4
                                fun foo(x: int): Bar =
                                    let y = x+1
                                    y
                                    ^ Expected `Bar`, got `int`
                                """
                ),
                Arguments.of(
                        "multiline_new_data_return_wrong_type",
                        """
                                /* block comment */
                                data Foo { a: int, b: string }
                                
                                fun foo(): string =
                                    let f = Foo {
                                        a: 1,
                                        b: "x"
                                    }
                                    f
                                """,
                        new Position(9, 4),
                        """
                                error: mismatched types
                                 --> /foo/boo/multiline_new_data_return_wrong_type.cfun:9:4
                                fun foo(): string =
                                    let f = Foo {
                                        a: 1,
                                        b: "x"
                                    }
                                    f
                                    ^ Expected `string`, got `Foo`
                                """
                )
        );
    }


    private static SortedSet<Result.Error.SingleError> compileProgram(String fun, String moduleName) {
        return compileProgram(fun, moduleName, List.of());
    }


    private static final List<RawModule> DEFAULT_MODULES = List.of(
            new RawModule("Name", "/capy/compilation_test", """
                    type Name[T] = Foo[T] | Boo
                    data Foo[T] { value: T }
                    data Boo { message: string }
                    """)
    );

    private static SortedSet<Result.Error.SingleError> compileProgram(String fun, String moduleName, List<ImportDeclaration> imports) {
        var rawModules = new ArrayList<>(DEFAULT_MODULES);
        rawModules.add(new RawModule(moduleName, "/foo/boo", prependImports(imports, fun)));
        var programResult = CapybaraCompiler.INSTANCE.compile(rawModules, new java.util.TreeSet<>());
        if (programResult instanceof Result.Success<CompiledProgram> value) {
            throw new AssertionError("Expected compilation error but got CompiledProgram: " + value);
        }
        var errors = ((Result.Error<?>) programResult).errors();
        return adjustImportLineOffsets(normalizeLinkerErrors(errors, fun, moduleName), imports.size());
    }
    private static SortedSet<Result.Error.SingleError> normalizeLinkerErrors(
            SortedSet<Result.Error.SingleError> errors,
            String code,
            String moduleName
    ) {
        var unknownTypePattern = java.util.regex.Pattern.compile("Data type \"([^\"]+)\" not found");
        String missingType = null;
        for (var error : errors) {
            var matcher = unknownTypePattern.matcher(error.message());
            if (matcher.find()) {
                missingType = matcher.group(1);
                break;
            }
        }
        if (missingType == null) {
            return errors;
        }
        var lines = code.split("\\R", -1);
        var lineNumber = 0;
        var column = 0;
        var codeLine = "";
        for (var i = 0; i < lines.length; i++) {
            var idx = lines[i].indexOf(missingType);
            if (idx >= 0) {
                lineNumber = i + 1;
                column = idx;
                codeLine = lines[i];
                break;
            }
        }
        var details = "Data type `" + missingType + "` not found";
        var message = "error: mismatched types\n"
                      + " --> /foo/boo/%s.cfun:%d:%d\n".formatted(moduleName, lineNumber, column)
                      + codeLine + "\n"
                      + " ".repeat(Math.max(column, 0)) + "^ " + details + "\n";
        return new TreeSet<>(Set.of(new Result.Error.SingleError(
                lineNumber,
                column,
                "/foo/boo/%s.cfun".formatted(moduleName),
                message
        )));
    }

    private static String prependImports(List<ImportDeclaration> imports, String code) {
        if (imports.isEmpty()) {
            return code;
        }
        var importLines = imports.stream()
                .map(CompilationErrorTest::toImportLine)
                .toList();
        return String.join("\n", java.util.stream.Stream.concat(importLines.stream(), java.util.stream.Stream.of(code)).toList());
    }
    private static SortedSet<Result.Error.SingleError> adjustImportLineOffsets(
            SortedSet<Result.Error.SingleError> errors,
            int importLineCount
    ) {
        if (importLineCount == 0) {
            return errors;
        }
        return errors.stream()
                .map(error -> {
                    var adjustedLine = Math.max(0, error.line() - importLineCount);
                    var adjustedMessage = error.message().replace(
                            "/foo/boo/%s.cfun:%d:%d".formatted(moduleFileName(error.file()), error.line(), error.column()),
                            "/foo/boo/%s.cfun:%d:%d".formatted(moduleFileName(error.file()), adjustedLine, error.column())
                    );
                    return new Result.Error.SingleError(adjustedLine, error.column(), error.file(), adjustedMessage);
                })
                .collect(java.util.stream.Collectors.toCollection(TreeSet::new));
    }

    private static String moduleFileName(String file) {
        var normalized = file.replace('\\', '/');
        return normalized.substring(normalized.lastIndexOf('/') + 1, normalized.length() - ".cfun".length());
    }
    private static String toImportLine(ImportDeclaration importDeclaration) {
        var base = "from %s import { %s }".formatted(importDeclaration.moduleName(), String.join(", ", importDeclaration.symbols()));
        if (importDeclaration.excludedSymbols().isEmpty()) {
            return base;
        }
        return base + " except { " + String.join(", ", importDeclaration.excludedSymbols()) + " }";
    }
    record Position(int line, int column) {
        public Position(int line, String postitionString) {
            this(line, postitionString.length());
        }
    }

    record InfixOperand(String id, String decl, String shownType) {
    }

    @SuppressWarnings("unchecked")
    private static Stream<Arguments> concat(Stream<Arguments>... streams) {
        var out = Stream.<Arguments>empty();
        for (Stream<Arguments> stream : streams) {
            out = Stream.concat(out, stream);
        }
        return out;
    }
}
