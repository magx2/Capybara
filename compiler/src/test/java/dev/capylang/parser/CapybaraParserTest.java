package dev.capylang.parser;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.Optional;
import java.lang.reflect.Method;
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
                                    case _ ->
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
                                    case End -> None {}
                                    case Cons { value, _ } -> Some { value }
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
                                    case End -> /capy/lang/Option.None {}
                                    case Cons { value, _ } -> Some { value }
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
    @DisplayName("should parse lowercase private local const declaration")
    void parseLowercasePrivateLocalConstDeclaration() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                fun foo(x: string): bool =
                    const __white_space = { " ", "\\t", "\\n" }
                    __white_space ? x
                """));

        var localConst = findFunction("__foo__local_const_0_white_space", module.functional());
        assertThat(localConst.parameters()).isEmpty();
        assertThat(localConst.returnType()).contains(new CollectionType.SetType(PrimitiveType.STRING));

        var function = findFunction("foo", module.functional());
        assertThat(function.expression()).isInstanceOf(InfixExpression.class);
        var expression = (InfixExpression) function.expression();
        assertThat(expression.left()).isInstanceOf(FunctionCall.class);
        assertThat(((FunctionCall) expression.left()).name()).isEqualTo("__foo__local_const_0_white_space");
    }

    @Test
    @DisplayName("should reject local const name without private prefix")
    void rejectLocalConstNameWithoutPrivatePrefix() {
        var result = new CapybaraParser().parseModule(new RawModule("Test", "/parser", """
                fun foo(x: string): bool =
                    const white_space = { " ", "\\t", "\\n" }
                    x == ""
                """));

        assertThat(result).isInstanceOf(Result.Error.class);
        assertThat(((Result.Error<dev.capylang.compiler.parser.Module>) result).errors())
                .singleElement()
                .satisfies(error -> {
                    assertThat(error.file()).isEqualTo("/parser/Test.cfun");
                    assertThat(error.line()).isEqualTo(2);
                    assertThat(error.column()).isEqualTo(4);
                    assertThat(error.message()).contains("Local const name has to start with `__`");
                });
    }

    @Test
    @DisplayName("should reject duplicate local const names with locations")
    void rejectDuplicateLocalConstNames() {
        var result = new CapybaraParser().parseModule(new RawModule("Test", "/parser", """
                fun foo(): int =
                    const __white_space = 1
                    const __white_space = 2
                    __white_space
                """));

        assertThat(result).isInstanceOf(Result.Error.class);
        assertThat(((Result.Error<dev.capylang.compiler.parser.Module>) result).errors())
                .singleElement()
                .satisfies(error -> {
                    assertThat(error.file()).isEqualTo("/parser/Test.cfun");
                    assertThat(error.line()).isEqualTo(2);
                    assertThat(error.column()).isEqualTo(4);
                    assertThat(error.message()).contains("Duplicate local const name: __white_space. Declared at: 2:4, 3:4");
                });
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
    @DisplayName("should format duplicate local function fallback with first occurrence and all locations")
    void formatDuplicateLocalFunctionFallback() throws Exception {
        var parser = new CapybaraParser();
        Method method = CapybaraParser.class.getDeclaredMethod("formatParserError", RawModule.class, String.class, String.class);
        method.setAccessible(true);
        var module = new RawModule("SemVer", "/capy/util", """
                fun parse(): int =
                    fun __parse_positive_digit(value: int): int = value
                    fun __parse_positive_digit(value: int): int = value + 1
                    0
                """);

        var error = (Result.Error.SingleError) method.invoke(
                parser,
                module,
                module.input(),
                "Duplicate local function name: __parse_positive_digit"
        );

        assertThat(error.line()).isEqualTo(2);
        assertThat(error.column()).isEqualTo(4);
        assertThat(error.message()).contains("Duplicate local function name: __parse_positive_digit. Declared at: 2:4, 3:4");
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
                    case Some { char } when char == "-" -> "minus"
                    case Some { char } -> "other"
                    case None -> "none"
                """));

        var function = findFunction("test", module.functional());
        assertThat(function.expression()).isInstanceOf(MatchExpression.class);
        var expression = (MatchExpression) function.expression();
        assertThat(expression.cases()).hasSize(3);
        assertThat(expression.cases().getFirst().guard()).isPresent();
        assertThat(expression.cases().get(1).guard()).isEmpty();
        assertThat(expression.cases().get(2).guard()).isEmpty();
    }

    @Test
    @DisplayName("should parse match case with pattern alternatives")
    void parseMatchCaseWithPatternAlternatives() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                fun test(char: string): string =
                    match char with
                    case "1" | "2" | "3" -> "digit"
                    case _ -> "other"
                """));

        var function = findFunction("test", module.functional());
        assertThat(function.expression()).isInstanceOf(MatchExpression.class);
        var expression = (MatchExpression) function.expression();
        assertThat(expression.cases()).hasSize(4);
        assertThat(expression.cases().get(0).pattern()).isEqualTo(new MatchExpression.StringPattern("\"1\""));
        assertThat(expression.cases().get(1).pattern()).isEqualTo(new MatchExpression.StringPattern("\"2\""));
        assertThat(expression.cases().get(2).pattern()).isEqualTo(new MatchExpression.StringPattern("\"3\""));
        assertThat(expression.cases().get(3).pattern()).isEqualTo(MatchExpression.WildcardPattern.WILDCARD);
        assertThat(expression.cases().subList(0, 3))
                .extracting(MatchExpression.MatchCase::expression)
                .allSatisfy(caseExpression -> {
                    assertThat(caseExpression).isInstanceOf(StringValue.class);
                    assertThat(((StringValue) caseExpression).stringValue()).isEqualTo("\"digit\"");
                });
    }

    @Test
    @DisplayName("should parse grouped brace expression in pipe lambda body with nested match")
    void parseGroupedBraceExpressionInPipeLambdaBodyWithNestedMatch() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                from /capy/lang/Result import { * }
                fun test(value: int): Result[int] =
                    Success { value }
                    | parsed => {
                        match parsed with
                        case v when v > 0 -> Success { v + 1 }
                        case _ -> Error { "invalid" }
                    }
                """));

        var function = findFunction("test", module.functional());
        assertThat(function.expression()).isInstanceOf(InfixExpression.class);
        var pipeExpression = (InfixExpression) function.expression();
        assertThat(pipeExpression.operator()).isEqualTo(InfixOperator.PIPE);
        assertThat(pipeExpression.right()).isInstanceOf(LambdaExpression.class);
        var lambda = (LambdaExpression) pipeExpression.right();
        assertThat(lambda.expression()).isInstanceOf(MatchExpression.class);
    }

    @Test
    @DisplayName("should parse chained pipe with grouped brace expression in later lambda body")
    void parseChainedPipeWithGroupedBraceExpressionInLaterLambdaBody() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                from /capy/lang/Result import { * }
                fun test(value: int): Result[int] =
                    Success { value }
                    | parsed => Success { parsed + 1 }
                    | parse => {
                        match parse with
                        case v when v > 0 -> Success { v }
                        case _ -> Error { "invalid" }
                    }
                """));

        var function = findFunction("test", module.functional());
        assertThat(function.expression()).isInstanceOf(InfixExpression.class);
    }

    @Test
    @DisplayName("should parse semver style grouped branch with nested pipe and match")
    void parseSemVerStyleGroupedBranchWithNestedPipeAndMatch() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                from /capy/lang/Option import { * }
                from /capy/lang/Result import { * }

                data SemVer { pre_release: Option[string], build_metadata: Option[string] }

                fun test(version: string): Result[SemVer] =
                    data __Parse[T] { buffer: string, value: T }
                    fun __parse_pre_release(version: string): Result[__Parse[string]] = Success { __Parse { version, "rc1" } }
                    fun __parse_build(version: string): Result[__Parse[string]] = Success { __Parse { version, "001" } }
                    let raw_sem_ver = SemVer { None {}, None {} }
                    let parse_patch: __Parse[string] = __Parse { version, "1.2.3" }
                    match parse_patch.buffer[0] with
                    case Some { next_char } when next_char == "-" -> {
                        __parse_pre_release(parse_patch.buffer[1:])
                        | parse_pre_release => Success { __Parse {
                            buffer: parse_pre_release.buffer,
                            value: raw_sem_ver.with(pre_release: Some { parse_pre_release.value })
                        }}
                        | parse => {
                            match parse.buffer[0] with
                            case None -> Success { parse }
                            case Some { char } when char == "+" -> {
                                __parse_build(parse.buffer[1:])
                                | parse_build => Success { __Parse {
                                    buffer: parse_build.buffer,
                                    value: parse.value.with(build_metadata: Some { parse_build.value })
                                }}
                            }
                            case Some { char } -> Error { "bad" }
                        }
                    }
                    case _ -> Success { raw_sem_ver }
                """));

        var function = findFunction("test", module.functional());
        var expression = function.expression();
        while (expression instanceof LetExpression letExpression) {
            expression = letExpression.rest();
        }
        assertThat(expression).isInstanceOf(MatchExpression.class);
        var outerMatch = (MatchExpression) expression;
        assertThat(outerMatch.cases()).hasSize(2);
        assertThat(outerMatch.cases().getFirst().expression()).isInstanceOf(InfixExpression.class);
        var firstCasePipe = (InfixExpression) outerMatch.cases().getFirst().expression();
        assertThat(firstCasePipe.right()).isInstanceOf(LambdaExpression.class);
        var laterLambda = (LambdaExpression) firstCasePipe.right();
        assertThat(laterLambda.expression()).isInstanceOf(MatchExpression.class);
        var innerMatch = (MatchExpression) laterLambda.expression();
        assertThat(innerMatch.cases()).hasSize(3);
    }

    @Test
    @DisplayName("should parse grouped match branch with later nested match over piped parse value without leaking cases")
    void parseGroupedMatchBranchWithLaterNestedMatchOverPipedParseValueWithoutLeakingCases() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                from /capy/lang/Result import { * }

                fun test(input: string): string =
                    data __Parse[T] { buffer: string, value: T }
                    fun __parse_tail(buffer: string): Result[__Parse[string]] = Success { __Parse { buffer, "tail" } }
                    fun __parse_build(buffer: string): Result[__Parse[string]] = Success { __Parse { buffer, "build" } }
                    let parse_patch: __Parse[string] = __Parse { input, "base" }
                    match parse_patch.buffer[0] with
                    case Some { ch } when ch == "-" -> {
                        __parse_tail(parse_patch.buffer[1:])
                        | parse_tail => Success { __Parse {
                            buffer: parse_tail.buffer,
                            value: "pre:" + parse_tail.value
                        }}
                        | parse => {
                            match parse.buffer[0] with
                            case None -> parse.value
                            case Some { ch } when ch == "+" -> {
                                __parse_build(parse.buffer[1:])
                                | parse_build => "plus:" + parse.value + ":" + parse_build.value
                            }
                            case Some { ch } -> "bad:" + ch
                        }
                    }
                    case Some { ch } when ch == "+" ->
                        __parse_build(parse_patch.buffer[1:])
                        | parse_build => Success { "plus:" + parse_build.value }
                    case Some { ch } -> "other:" + ch
                    case None -> "none"
                """));

        var function = findFunction("test", module.functional());
        var expression = function.expression();
        while (expression instanceof LetExpression letExpression) {
            expression = letExpression.rest();
        }
        assertThat(expression).isInstanceOf(MatchExpression.class);
        var outerMatch = (MatchExpression) expression;
        assertThat(outerMatch.cases()).hasSize(4);
        assertThat(outerMatch.cases().getFirst().expression()).isInstanceOf(InfixExpression.class);
    }
}




