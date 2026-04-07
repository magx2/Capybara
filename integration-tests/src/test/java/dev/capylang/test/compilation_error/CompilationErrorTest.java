package dev.capylang.test.compilation_error;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
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
                        "match_result_branch_type_mismatch",
                        """
                                from /capy/lang/Option import { * }
                                from /capy/lang/Result import { * }
                                fun foo(v: string): Result[string] =
                                    match v[0] with
                                    case None -> Error { "missing" }
                                    case Some { value } -> value + ""
                                """,
                        new Position(4, 4),
                        """
                                error: mismatched types
                                 --> /foo/boo/match_result_branch_type_mismatch.cfun:4:4
                                fun foo(v: string): Result[string] = match v[0] with ...
                                    ^ expected `Result[string]`, found `string`
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
                        new Position(4, 4),
                        """
                                error: mismatched types
                                 --> /foo/boo/match_result_branch_type_mismatch_to_int.cfun:4:4
                                fun foo(v: string): Result[int] = match v[0] with ...
                                    ^ expected `Result[int]`, found `string`
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
                        new Position(3, 4),
                        """
                                error: mismatched types
                                 --> /foo/boo/match_option_branch_type_mismatch.cfun:3:4
                                fun foo(v: string): Option[string] = match v[0] with ...
                                    ^ expected `Option[string]`, found `string`
                                """
                ));
        var primitiveTypes = List.of("string", "byte", "int", "long", "float", "double", "bool");
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
                        + "  fun _deserialize_json_null(json: string): Result[_Parse[JsonBool]] =\n"
                        + "    let parsed: _Parse[JsonNull] = _Parse {\n"
                        + "        JsonNull(),\n"
                        + "        json[4:]\n"
                        + "    }\n"
                        + "    Success {\n"
                        + "        parsed\n"
                        + "    }\n"
                        + "    ^ expected `Result[_Parse[JsonBool]]`, found `Success[_Parse[JsonNull]]`\n"
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
                                                       ^ expected `int`, found `string`
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
                                                       ^ expected `int`, found `bool`
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
                                                       ^ expected `int`, found `long`
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
                                                       ^ expected `int`, found `float`
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
                                                          ^ expected `string`, found `int`
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
                                    ^ expected `int`, found `string`
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
                                    ^ expected `int`, found `string`
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
                                    ^ expected `Bar`, found `int`
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
                                    ^ expected `string`, found `Foo`
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






























