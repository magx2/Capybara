package dev.capylang.parser;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.Optional;
import java.util.stream.Stream;

import dev.capylang.compiler.Result;
import dev.capylang.compiler.parser.*;

import static org.assertj.core.api.Assertions.assertThat;

class CapybaraParserTest {

    @ParameterizedTest(name = "{index}: should parse function {0}")
    @MethodSource
    void parse(String name, String code) {
        var module = parseSuccess(new RawModule("Test", "/parser", code));
        findFunction(name, module.functional());
        System.out.println(module);
    }

    private static Function findFunction(String name, Functional functional) {
        return functional.definitions()
                .stream()
                .filter(Function.class::isInstance)
                .map(Function.class::cast)
                .filter(f -> f.name().equals(name))
                .findAny()
                .orElseThrow(() -> new AssertionError("Function " + name + " not found"));
    }

    private static <T> T findDefinition(Class<T> type, String name, Functional functional) {
        return functional.definitions()
                .stream()
                .filter(type::isInstance)
                .map(type::cast)
                .filter(definition -> switch (definition) {
                    case DataDeclaration dataDeclaration -> dataDeclaration.name().equals(name);
                    case TypeDeclaration typeDeclaration -> typeDeclaration.name().equals(name);
                    default -> false;
                })
                .findAny()
                .orElseThrow(() -> new AssertionError(type.getSimpleName() + " " + name + " not found"));
    }

    private static dev.capylang.compiler.parser.Module parseSuccess(RawModule module) {
        var result = new CapybaraParser().parseModule(module);
        assertThat(result).isInstanceOf(Result.Success.class);
        return ((Result.Success<dev.capylang.compiler.parser.Module>) result).value();
    }

    static Stream<Arguments> parse() {
        return Stream.of(
                Arguments.of(
                        "test_if",
                        """
                                fun test_if(x: int): string =
                                    if x > 0 then "positive"
                                    else if x < 0 then "negative"
                                    else "zero"
                                """),
                Arguments.of("list_identity", "fun list_identity(l: list[int]): list[int] = l"),
                Arguments.of("pipe_map", "fun pipe_map(l: list[int]): list[int] = l | x => x * 2"),
                Arguments.of("map", "fun map(l: list[int]) = l | :double\nfun double(x: int) = x * x"),
                Arguments.of("pipe_filter_out", "fun pipe_filter_out(l: list[int]): list[int] = l |- x => x > 0"),
                Arguments.of("pipe_reduce", "fun pipe_reduce(l: list[int]): int = l |> 0, (a, b) => a + b"),
                Arguments.of("pipe_reduce_dict", "fun pipe_reduce_dict(d: dict[int]): string = d |> \"\", (a, k, v) => a + k + v"),
                Arguments.of("pipe_map_unused", "fun pipe_map_unused(l: list[int]): list[int] = l | _ => 1"),
                Arguments.of("pipe_reduce_unused", "fun pipe_reduce_unused(l: list[int]): int = l |> 0, (_, v) => v"),
                Arguments.of("pipe_reduce_dict_unused", "fun pipe_reduce_dict_unused(d: dict[int]): int = d |> 0, (_, _, v) => v"),
                Arguments.of("pipe_flat_map", "fun pipe_flat_map(l: list[int]): list[int] = l |* x => [x, x + 1]"),
                Arguments.of("single_quote_string", "fun single_quote_string(): string = 'hello'"),
                Arguments.of(
                        "chained_lets_without_semicolons",
                        """
                                fun chained_lets_without_semicolons(): int =
                                    let a = 1
                                    let b = a + 2
                                    b * 3
                                """
                ),
                Arguments.of(
                        "let_inside_match_branch",
                        """
                                fun let_inside_match_branch(v: int): int =
                                    match v with
                                    | _ ->
                                        let x = v + 1
                                        x * 2
                                """
                ),
                Arguments.of("const_usage", "const PI = 3.14\nfun const_usage(): double = PI"),
                Arguments.of(
                        "block_comment",
                        """
                                /*
                                 this is
                                 multiline comment
                                */
                                fun block_comment(): int = 1
                                """
                ),
                Arguments.of(
                        "invoke_multi",
                        "fun run(f: (int, int) => int): int = f(1, 2)\nfun invoke_multi(): int = run((a, b) => a + b)"
                ),
                Arguments.of(
                        "invoke_no_arg_lambda",
                        "fun run0(f: () => string): string = f()\nfun invoke_no_arg_lambda(): string = run0(() => \"Hello, Wrold\")"
                ),
                Arguments.of(
                        "seq_first_wildcard",
                        """
                                type Seq[T] = Cons[T] | End
                                data Cons[T] { value: T, rest: Seq[T] }
                                single End

                                fun seq_first_wildcard(s: Seq[int]): Option[int] =
                                    match s with
                                    | End -> None {}
                                    | Cons { value, _ } -> Some { value }
                                """
                ),
                Arguments.of(
                        "seq_first_wildcard_fq_singleton",
                        """
                                type Seq[T] = Cons[T] | End
                                data Cons[T] { value: T, rest: Seq[T] }
                                single End

                                fun seq_first_wildcard_fq_singleton(s: Seq[int]): /capy/lang/Option[int] =
                                    match s with
                                    | End -> /capy/lang/Option.None {}
                                    | Cons { value, _ } -> Some { value }
                                """
                ));
    }

    @Test
    @DisplayName("should parse const declaration as zero-arg function")
    void parseConstDeclaration() {
        var module = parseSuccess(new RawModule("Test", "/parser", "const E: double = 2."));
        var function = findFunction("E", module.functional());
        assertThat(function.parameters()).isEmpty();
        assertThat(function.returnType()).contains(PrimitiveType.DOUBLE);
    }

    @Test
    @DisplayName("should parse list type")
    void parseListType() {
        // given
        var name = "list_identity";
        var code = "fun list_identity(l: list[int]): list[int] = l";

        // when
        var module = parseSuccess(new RawModule("Test", "/parser", code));

        // then
        var function = findFunction(name, module.functional());
        var parameters = function.parameters();
        assertThat(parameters).hasSize(1);
        var parameter = parameters.get(0);
        assertThat(parameter.name()).isEqualTo("l");
        var listOfIntsType = new CollectionType.ListType(PrimitiveType.INT);
        assertThat(parameter.type()).isEqualTo(listOfIntsType);
        assertThat(function.returnType()).hasValue(listOfIntsType);
    }

    @Test
    @DisplayName("should parse doc comments for data and type declarations")
    void parseDataAndTypeComments() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                /// See: [Complete JUnit XML example](https://github.com/testmoapp/junitxml?tab=readme-ov-file#complete-junit-xml-example)
                data JUnitReport { suites: list[JUnitTestSuite] }

                /// Represents a single suite
                type JUnitNode = JUnitTestSuite
                """));

        var data = findDefinition(DataDeclaration.class, "JUnitReport", module.functional());
        var type = findDefinition(TypeDeclaration.class, "JUnitNode", module.functional());

        assertThat(data.comments()).containsExactly("See: [Complete JUnit XML example](https://github.com/testmoapp/junitxml?tab=readme-ov-file#complete-junit-xml-example)");
        assertThat(type.comments()).containsExactly("Represents a single suite");
    }

    @Test
    @DisplayName("should parse doc comments for local function declarations")
    void parseLocalFunctionComments() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                fun accumulate(n: int): int =
                    /// Internal accumulate
                    fun __accumulate(n: int, acc: int): int =
                        if n <= 0 then acc else __accumulate(n-1, acc+n)
                    __accumulate(n, 0)
                """));

        var localFunction = findFunction("__accumulate__local_fun_0_accumulate", module.functional());
        assertThat(localFunction.comments()).containsExactly("Internal accumulate");
    }

    @Test
    @DisplayName("should parse bang over field access")
    void parseBangOverFieldAccess() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                data Box { failed: bool }
                fun test(box: Box): bool = !box.failed
                """));

        var function = findFunction("test", module.functional());
        assertThat(function.expression()).isInstanceOf(InfixExpression.class);
        var expression = (InfixExpression) function.expression();
        assertThat(expression.operator()).isEqualTo(InfixOperator.EQUAL);
        assertThat(expression.left()).isInstanceOf(FieldAccess.class);
        var fieldAccess = (FieldAccess) expression.left();
        assertThat(fieldAccess.field()).isEqualTo("failed");
        assertThat(fieldAccess.source()).isInstanceOf(Value.class);
        assertThat(((Value) fieldAccess.source()).name()).isEqualTo("box");
        assertThat(expression.right()).isInstanceOf(BooleanValue.class);
        assertThat(((BooleanValue) expression.right()).value()).isFalse();
    }

    @Test
    @DisplayName("should parse bang over method call")
    void parseBangOverMethodCall() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                data Box { failed: bool }
                fun Box.has_failed(): bool = this.failed
                fun test(box: Box): bool = !box.has_failed()
                """));

        var function = findFunction("test", module.functional());
        assertThat(function.expression()).isInstanceOf(InfixExpression.class);
        var expression = (InfixExpression) function.expression();
        assertThat(expression.operator()).isEqualTo(InfixOperator.EQUAL);
        assertThat(expression.left()).isInstanceOf(FunctionCall.class);
        var functionCall = (FunctionCall) expression.left();
        assertThat(functionCall.moduleName()).isEmpty();
        assertThat(functionCall.name()).isEqualTo("__invoke__has_failed");
        assertThat(functionCall.arguments()).singleElement().isInstanceOf(Value.class);
        assertThat(((Value) functionCall.arguments().getFirst()).name()).isEqualTo("box");
        assertThat(expression.right()).isInstanceOf(BooleanValue.class);
        assertThat(((BooleanValue) expression.right()).value()).isFalse();
    }

    @Test
    @DisplayName("should parse bang over nested field access")
    void parseBangOverNestedFieldAccess() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                data Parse { buffer: string }
                fun test(parse: Parse): bool = !parse.buffer.is_empty
                """));

        var function = findFunction("test", module.functional());
        assertThat(function.expression()).isInstanceOf(InfixExpression.class);
        var expression = (InfixExpression) function.expression();
        assertThat(expression.operator()).isEqualTo(InfixOperator.EQUAL);
        assertThat(expression.left()).isInstanceOf(FieldAccess.class);
        var outerFieldAccess = (FieldAccess) expression.left();
        assertThat(outerFieldAccess.field()).isEqualTo("is_empty");
        assertThat(outerFieldAccess.source()).isInstanceOf(FieldAccess.class);
        var innerFieldAccess = (FieldAccess) outerFieldAccess.source();
        assertThat(innerFieldAccess.field()).isEqualTo("buffer");
        assertThat(innerFieldAccess.source()).isInstanceOf(Value.class);
        assertThat(((Value) innerFieldAccess.source()).name()).isEqualTo("parse");
        assertThat(expression.right()).isInstanceOf(BooleanValue.class);
        assertThat(((BooleanValue) expression.right()).value()).isFalse();
    }

    @Test
    @DisplayName("should parse field access followed by index")
    void parseFieldAccessFollowedByIndex() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                data Parse { buffer: string }
                fun test(parse: Parse): string = parse.buffer[0]
                """));

        var function = findFunction("test", module.functional());
        assertThat(function.expression()).isInstanceOf(IndexExpression.class);
        var indexExpression = (IndexExpression) function.expression();
        assertThat(indexExpression.source()).isInstanceOf(FieldAccess.class);
        var fieldAccess = (FieldAccess) indexExpression.source();
        assertThat(fieldAccess.field()).isEqualTo("buffer");
        assertThat(fieldAccess.source()).isInstanceOf(Value.class);
        assertThat(((Value) fieldAccess.source()).name()).isEqualTo("parse");
        assertThat(indexExpression.index()).isInstanceOf(IntValue.class);
        assertThat(((IntValue) indexExpression.index()).intValue()).isEqualTo("0");
    }

    @Test
    @DisplayName("should reject chained postfix after unparenthesized pipe lambda")
    void rejectChainedPostfixAfterUnparenthesizedPipeLambda() {
        var result = new CapybaraParser().parseModule(new RawModule("Test", "/parser", """
                fun test(): /capy/lang/Option[string] =
                    to_seq([() => "ok", () => "failed", () => "later"])
                        | supplier => supplier()
                        .drop_until(value => value == "failed")
                        .first()
                """));

        assertThat(result).isInstanceOf(Result.Error.class);
        assertThat(((Result.Error<dev.capylang.compiler.parser.Module>) result).errors())
                .singleElement()
                .satisfies(error -> {
                    assertThat(error.file()).isEqualTo("/parser/Test.cfun");
                    assertThat(error.line()).isEqualTo(4);
                    assertThat(error.column()).isEqualTo(8);
                    assertThat(error.message()).contains("Parenthesize lambda before chaining postfix operations");
                });
    }

    @Test
    @DisplayName("should parse chained postfix after parenthesized pipe lambda")
    void parseChainedPostfixAfterParenthesizedPipeLambda() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                fun test(): /capy/lang/Option[string] =
                    to_seq([() => "ok", () => "failed", () => "later"])
                        | (supplier => supplier())
                        .drop_until(value => value == "failed")
                        .first()
                """));

        var function = findFunction("test", module.functional());
        assertThat(function.expression()).isInstanceOf(FunctionCall.class);
        var firstCall = (FunctionCall) function.expression();
        assertThat(firstCall.name()).isEqualTo("__invoke__first");
        assertThat(firstCall.arguments()).hasSize(1);
        assertThat(firstCall.arguments().getFirst()).isInstanceOf(FunctionCall.class);

        var dropUntilCall = (FunctionCall) firstCall.arguments().getFirst();
        assertThat(dropUntilCall.name()).isEqualTo("__invoke__drop_until");
        assertThat(dropUntilCall.arguments()).hasSize(2);
        assertThat(dropUntilCall.arguments().getFirst()).isInstanceOf(InfixExpression.class);

        var pipeExpression = (InfixExpression) dropUntilCall.arguments().getFirst();
        assertThat(pipeExpression.operator()).isEqualTo(InfixOperator.PIPE);
        assertThat(pipeExpression.right()).isInstanceOf(LambdaExpression.class);
    }

    @Test
    @DisplayName("should allow multiline postfix formatting inside unparenthesized pipe lambda body")
    void allowMultilinePostfixInsideUnparenthesizedPipeLambdaBody() {
        var result = new CapybaraParser().parseModule(new RawModule("Test", "/parser", """
                fun test(): string =
                    " x "
                        | value =>
                            value
                                .trim()
                """));

        assertThat(result).isInstanceOf(Result.Success.class);
    }

    @Test
    @DisplayName("should parse with expression with named assignments")
    void parseWithExpression() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                data Foo { a: int, b: string }
                fun test(foo: Foo): Foo = foo.with(a: foo.a + 1, b: "x")
                """));

        var function = findFunction("test", module.functional());
        assertThat(function.expression()).isInstanceOf(WithExpression.class);
        var withExpression = (WithExpression) function.expression();
        assertThat(withExpression.assignments()).hasSize(2);
        assertThat(withExpression.assignments().get(0).name()).isEqualTo("a");
        assertThat(withExpression.assignments().get(1).name()).isEqualTo("b");
    }

    @Test
    @DisplayName("should parse chained with expressions")
    void parseChainedWithExpression() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                data Foo { a: int, b: string }
                fun test(foo: Foo): Foo = foo.with(a: 1).with(b: "x")
                """));

        var function = findFunction("test", module.functional());
        assertThat(function.expression()).isInstanceOf(WithExpression.class);
        var outerWith = (WithExpression) function.expression();
        assertThat(outerWith.assignments()).singleElement().extracting(NewData.FieldAssignment::name).isEqualTo("b");
        assertThat(outerWith.source()).isInstanceOf(WithExpression.class);
        var innerWith = (WithExpression) outerWith.source();
        assertThat(innerWith.assignments()).singleElement().extracting(NewData.FieldAssignment::name).isEqualTo("a");
    }

    @Test
    @DisplayName("should parse match case when guard")
    void parseMatchCaseWhenGuard() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                from /capy/lang/Option import { * }
                fun test(option: Option[string]): string =
                    match option with
                    | Some { char } when char == "-" -> "minus"
                    | Some { char } -> "other"
                    | None -> "none"
                """));

        var function = findFunction("test", module.functional());
        assertThat(function.expression()).isInstanceOf(MatchExpression.class);
        var expression = (MatchExpression) function.expression();
        assertThat(expression.cases()).hasSize(3);
        assertThat(expression.cases().getFirst().guard()).isPresent();
        assertThat(expression.cases().get(1).guard()).isEmpty();
        assertThat(expression.cases().get(2).guard()).isEmpty();
    }
}




