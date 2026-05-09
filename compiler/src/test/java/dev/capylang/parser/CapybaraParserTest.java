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
                Arguments.of(
                        "enum_type_pattern",
                        """
                                enum Status { READY, DONE }
                                fun enum_type_pattern(value: any): string =
                                    match value with
                                    case enum e -> e.name()
                                    case _ -> "other"
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
    @DisplayName("should parse placeholder arguments in function calls")
    void parsePlaceholderArgumentsInFunctionCalls() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                fun add(a: int, b: int): int = a + b
                fun test(): int => int = add(1, _)
                """));

        var function = findFunction("test", module.functional());
        assertThat(function.expression()).isInstanceOf(FunctionCall.class);
        var call = (FunctionCall) function.expression();
        assertThat(call.arguments()).hasSize(2);
        assertThat(call.arguments().get(1)).isInstanceOf(PlaceholderExpression.class);
    }

    @Test
    @DisplayName("should parse multiple placeholder arguments in order")
    void parseMultiplePlaceholderArgumentsInOrder() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                fun foo(a: int, b: string, c: double, d: int): string = b
                fun test(b: string): (int, double, int) => string = foo(_, b, _, _)
                """));

        var function = findFunction("test", module.functional());
        assertThat(function.expression()).isInstanceOf(FunctionCall.class);
        var call = (FunctionCall) function.expression();
        assertThat(call.arguments()).hasSize(4);
        assertThat(call.arguments().get(0)).isInstanceOf(PlaceholderExpression.class);
        assertThat(call.arguments().get(2)).isInstanceOf(PlaceholderExpression.class);
        assertThat(call.arguments().get(3)).isInstanceOf(PlaceholderExpression.class);
    }

    @Test
    @DisplayName("should parse standalone placeholder for compiler diagnostics")
    void parseStandalonePlaceholderForCompilerDiagnostics() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                fun test(): int => int = _
                """));

        var function = findFunction("test", module.functional());
        assertThat(function.expression()).isInstanceOf(PlaceholderExpression.class);
    }

    @Test
    @DisplayName("should parse data extension from multiple parent data declarations")
    void parseDataExtensionFromMultipleParents() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                data Foo { a: int, b: int }
                data Boo { b: int, c: int }
                data Bar { d: int, ...Foo, ...Boo }
                """));

        var data = findDefinition(DataDeclaration.class, "Bar", module.functional());
        assertThat(data.fields()).extracting(DataDeclaration.DataField::name).containsExactly("d");
        assertThat(data.extendsTypes()).containsExactly("Foo", "Boo");
    }

    @Test
    @DisplayName("should parse data constructor and constructor-local star data")
    void parseDataConstructor() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                data User { age: int } with constructor {
                    if age > 0 then * { age: age } else * { age: 1 }
                }
                """));

        var data = findDefinition(DataDeclaration.class, "User", module.functional());
        assertThat(data.constructor()).isPresent();
        assertThat(data.constructor().orElseThrow()).isInstanceOf(IfExpression.class);
        var constructor = (IfExpression) data.constructor().orElseThrow();
        assertThat(constructor.thenBranch()).isInstanceOf(ConstructorData.class);
        assertThat(constructor.elseBranch()).isInstanceOf(ConstructorData.class);
    }

    @Test
    @DisplayName("should parse deriver declarations and derive clauses")
    void parseDeriverAndDeriveClause() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                deriver Show {
                    fun show(): string =
                        let info: DataValueInfo = reflection(receiver)
                        let body: string = info.fields |> info.name + " { ", (acc, field) =>
                            acc + field.name + ": " + field.type.name
                        body + " }"
                }

                data User { name: string, age: int } derive Show
                type Named { id: string } = Person derive Show
                data Person { id: string, name: string }
                """));

        var deriver = module.functional().definitions().stream()
                .filter(DeriverDeclaration.class::isInstance)
                .map(DeriverDeclaration.class::cast)
                .findFirst()
                .orElseThrow();
        assertThat(deriver.name()).isEqualTo("Show");
        assertThat(deriver.methods()).extracting(DeriverDeclaration.DeriverMethod::name)
                .containsExactly("show");
        assertThat(deriver.methods().getFirst().parameters()).extracting(Parameter::name)
                .isEmpty();

        var data = findDefinition(DataDeclaration.class, "User", module.functional());
        assertThat(data.constructor()).isEmpty();
        assertThat(data.derives()).extracting(DeriveDirective::name)
                .containsExactly("Show");

        var type = findDefinition(TypeDeclaration.class, "Named", module.functional());
        assertThat(type.derives()).extracting(DeriveDirective::name)
                .containsExactly("Show");
    }

    @Test
    @DisplayName("should parse data constructor bypass")
    void parseDataConstructorBypass() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                data User { age: int } with constructor {
                    if age > 0 then Success { value: * { age: age } } else Error { "age" }
                }

                fun fallback(): User = User! { age: 1 }
                """));

        var function = findFunction("fallback", module.functional());
        assertThat(function.expression()).isInstanceOf(NewData.class);
        var newData = (NewData) function.expression();
        assertThat(newData.bypassConstructor()).isTrue();
    }

    @Test
    @DisplayName("should parse type constructor and constructor-local star data")
    void parseTypeConstructor() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                type User { age: int } with constructor {
                   if age > 0 then Success { * { age: age } } else Error { "age" }
                } = Adult | Child
                """));

        var type = findDefinition(TypeDeclaration.class, "User", module.functional());
        assertThat(type.constructor()).isPresent();
        assertThat(type.constructor().orElseThrow()).isInstanceOf(IfExpression.class);
        var constructor = (IfExpression) type.constructor().orElseThrow();
        assertThat(constructor.thenBranch()).isInstanceOf(NewData.class);
    }

    @Test
    @DisplayName("should parse result bind lets")
    void parseResultBindLets() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                from /capy/lang/Result import { * }

                fun users(name: string): Result[string] =
                    let user <- Success { value: name }
                    user
                """));

        var function = findFunction("users", module.functional());
        assertThat(function.expression()).isInstanceOf(LetExpression.class);
        var letExpression = (LetExpression) function.expression();
        assertThat(letExpression.kind()).isEqualTo(LetExpression.Kind.RESULT_BIND);
        assertThat(letExpression.name()).isEqualTo("user");
        assertThat(letExpression.rest()).isInstanceOf(Value.class);
    }

    @Test
    @DisplayName("should keep comparison with unary minus when there are no spaces")
    void parseComparisonWithUnaryMinusWithoutSpaces() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                fun negative_compare(a: int): bool = a<-1
                """));

        var function = findFunction("negative_compare", module.functional());
        assertThat(function.expression()).isInstanceOf(InfixExpression.class);
        var infixExpression = (InfixExpression) function.expression();
        assertThat(infixExpression.operator()).isEqualTo(InfixOperator.LT);
    }

    @Test
    @DisplayName("should reject data extension spread when it is not at the end")
    void rejectDataExtensionSpreadNotAtTheEnd() {
        var result = new CapybaraParser().parseModule(new RawModule("Test", "/parser", """
                data Foo { a: int }
                data Bar { ...Foo, b: int }
                """));

        assertThat(result).isInstanceOf(Result.Error.class);
        assertThat(((Result.Error<dev.capylang.compiler.parser.Module>) result).errors())
                .singleElement()
                .satisfies(error -> {
                    assertThat(error.file()).isEqualTo("/parser/Test.cfun");
                    assertThat(error.line()).isEqualTo(2);
                    assertThat(error.column()).isEqualTo(19);
                    assertThat(error.message()).contains("Spread data extension `...Type` must be declared at the end of data fields");
                });
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
    @DisplayName("should parse bare decimal literal as double")
    void parseBareDecimalLiteralAsDouble() {
        var module = parseSuccess(new RawModule("Test", "/parser", "fun pi(): double = 3.14"));
        var function = findFunction("pi", module.functional());
        assertThat(function.returnType()).contains(PrimitiveType.DOUBLE);
        assertThat(function.expression()).isInstanceOf(DoubleValue.class);
    }

    @Test
    @DisplayName("should parse suffixed float literal as float")
    void parseSuffixedFloatLiteralAsFloat() {
        var module = parseSuccess(new RawModule("Test", "/parser", "fun ratio(): float = 3.14f"));
        var function = findFunction("ratio", module.functional());
        assertThat(function.returnType()).contains(PrimitiveType.FLOAT);
        assertThat(function.expression()).isInstanceOf(FloatValue.class);
    }


    @Test
    @DisplayName("should parse underscored int and long literals")
    void parseUnderscoredIntAndLongLiterals() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                fun i(): int = 100_000
                fun l(): long = 9_738_771_718_7L
                """));

        var intFunction = findFunction("i", module.functional());
        assertThat(intFunction.expression()).isInstanceOf(IntValue.class);
        assertThat(((IntValue) intFunction.expression()).intValue()).isEqualTo("100_000");

        var longFunction = findFunction("l", module.functional());
        assertThat(longFunction.expression()).isInstanceOf(LongValue.class);
        assertThat(((LongValue) longFunction.expression()).longValue()).isEqualTo("9_738_771_718_7L");
    }

    @Test
    @DisplayName("should parse native expression literal")
    void parseNativeExpressionLiteral() {
        var module = parseSuccess(new RawModule("Test", "/parser", "fun generated(): int = <native>"));

        var function = findFunction("generated", module.functional());

        assertThat(function.expression()).isInstanceOf(NothingValue.class);
        assertThat(((NothingValue) function.expression()).literal()).isEqualTo("<native>");
    }

    @Test
    @DisplayName("should parse regex literal into runtime factory call")
    void parseRegexLiteral() {
        var module = parseSuccess(new RawModule("Test", "/parser", "fun digits() = regex/\\\\d+/im"));

        var function = findFunction("digits", module.functional());
        assertThat(function.expression()).isInstanceOf(FunctionCall.class);
        var call = (FunctionCall) function.expression();
        assertThat(call.moduleName()).contains("/capy/lang/Regex");
        assertThat(call.name()).isEqualTo("from_literal");
        assertThat(call.arguments()).hasSize(2);
        assertThat(call.arguments().get(0)).isInstanceOf(StringValue.class);
        assertThat(((StringValue) call.arguments().get(0)).stringValue()).isEqualTo("\"\\\\d+\"");
        assertThat(call.arguments().get(1)).isInstanceOf(StringValue.class);
        assertThat(((StringValue) call.arguments().get(1)).stringValue()).isEqualTo("\"im\"");
    }

    @Test
    @DisplayName("should parse regex symbolic operators")
    void parseRegexSymbolicOperators() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                fun check(v: string) = regex/\\\\d+/ ? v
                fun find(v: string) = regex/\\\\d+/ ~ v
                fun find_all(v: string) = regex/\\\\d+/ ~~ v
                fun redact() = regex/\\\\d+/ ~> "#"
                fun split(v: string) = regex/,/ /> v
                """));

        assertThat(((InfixExpression) findFunction("check", module.functional()).expression()).operator()).isEqualTo(InfixOperator.QUESTION);
        assertThat(((InfixExpression) findFunction("find", module.functional()).expression()).operator()).isEqualTo(InfixOperator.TILDE);
        assertThat(((InfixExpression) findFunction("find_all", module.functional()).expression()).operator()).isEqualTo(InfixOperator.TILDE_TILDE);
        assertThat(((InfixExpression) findFunction("redact", module.functional()).expression()).operator()).isEqualTo(InfixOperator.TILDE_GT);
        assertThat(((InfixExpression) findFunction("split", module.functional()).expression()).operator()).isEqualTo(InfixOperator.DIV_GT);
    }

    @Test
    @DisplayName("should parse lowercase private local const declaration")
    void parseLowercasePrivateLocalConstDeclaration() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                fun foo(x: string): bool =
                    const __white_space = { " ", "\\t", "\\n" }
                    ---
                    __white_space ? x
                """));

        var localConst = findFunction("__foo__scope_1_0__local_const_0_white_space", module.functional());
        assertThat(localConst.parameters()).isEmpty();
        assertThat(localConst.returnType()).contains(new CollectionType.SetType(PrimitiveType.STRING));

        var function = findFunction("foo", module.functional());
        assertThat(function.expression()).isInstanceOf(InfixExpression.class);
        var expression = (InfixExpression) function.expression();
        assertThat(expression.left()).isInstanceOf(FunctionCall.class);
        assertThat(((FunctionCall) expression.left()).name()).isEqualTo("__foo__scope_1_0__local_const_0_white_space");
    }

    @Test
    @DisplayName("should reject local const name without private prefix")
    void rejectLocalConstNameWithoutPrivatePrefix() {
        var result = new CapybaraParser().parseModule(new RawModule("Test", "/parser", """
                fun foo(x: string): bool =
                    const white_space = { " ", "\\t", "\\n" }
                    ---
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
                    ---
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
    @DisplayName("should parse line-wrapped type brackets")
    void parseLineWrappedTypeBrackets() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                data Box
                [T] { value: T }

                fun list_identity(xs: list
                [string]): list
                [string] = xs

                fun box_identity(box: Box
                [string]): Box
                [string] = box

                fun match_list(value: any): int =
                    match value with
                    case list
                    [string] _ -> 1
                    case _ -> 0
                """));

        findDefinition(DataDeclaration.class, "Box", module.functional());
        assertThat(findFunction("list_identity", module.functional()).returnType())
                .hasValue(new CollectionType.ListType(PrimitiveType.STRING));
        findFunction("box_identity", module.functional());
        findFunction("match_list", module.functional());
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
    @DisplayName("should parse trailing comma in data and type field declarations")
    void parseTrailingCommaInDataAndTypeFieldDeclarations() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                type T1 { a: int, b: string, c: int, } = D1 | D2
                data D1 { d: int, e: int, }
                data D2 { f: int, g: int, }
                """));

        var type = findDefinition(TypeDeclaration.class, "T1", module.functional());
        var d1 = findDefinition(DataDeclaration.class, "D1", module.functional());
        var d2 = findDefinition(DataDeclaration.class, "D2", module.functional());

        assertThat(type.fields()).hasSize(3);
        assertThat(d1.fields()).hasSize(2);
        assertThat(d2.fields()).hasSize(2);
    }

    @Test
    @DisplayName("should format duplicate local function fallback with first occurrence and all locations")
    void formatDuplicateLocalFunctionFallback() throws Exception {
        var parser = new CapybaraParser();
        Method method = CapybaraParser.class.getDeclaredMethod("formatParserError", RawModule.class, String.class, String.class);
        method.setAccessible(true);
                var module = new RawModule("SemVer", "/capy/util", """
                fun parse(): int =
                    fun rec __parse_positive_digit(value: int): int = __parse_positive_digit(value)
                    fun __parse_positive_digit(value: int): int = value + 1
                    ---
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
                    ---
                    __accumulate(n, 0)
                """));

        var localFunction = findFunction("__accumulate__scope_1_0__local_fun_0_accumulate", module.functional());
        assertThat(localFunction.comments()).containsExactly("Internal accumulate");
    }

    @Test
    @DisplayName("should parse tail recursive function marker")
    void parseTailRecursiveFunctionMarker() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                fun rec sum(n: int, acc: int): int =
                    if n <= 0 then acc else sum(n - 1, acc + n)
                """));

        var function = findFunction("sum", module.functional());
        assertThat(function.tailRecursive()).isTrue();
    }

    @Test
    @DisplayName("should parse tail recursive local function marker")
    void parseTailRecursiveLocalFunctionMarker() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                fun sum(n: int): int =
                    fun rec __sum(n: int, acc: int): int =
                        if n <= 0 then acc else __sum(n - 1, acc + n)
                    ---
                    __sum(n, 0)
                """));

        var localFunction = findFunction("__sum__scope_1_0__local_fun_0_sum", module.functional());
        assertThat(localFunction.tailRecursive()).isTrue();
    }

    @Test
    @DisplayName("should keep rec usable as a function name and call")
    void parseRecFunctionNameAndCall() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                fun rec(x: int): int = x
                fun call_rec(x: int): int = rec(x)
                """));

        assertThat(findFunction("rec", module.functional()).tailRecursive()).isFalse();
        var call = (FunctionCall) findFunction("call_rec", module.functional()).expression();
        assertThat(call.name()).isEqualTo("rec");
    }

    @Test
    @DisplayName("should parse doc comments for const declarations")
    void parseConstComments() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                /// Global threshold
                const THRESHOLD: int = 10

                fun config(): int =
                    /// Internal threshold
                    const __threshold: int = THRESHOLD
                    ---
                    __threshold
                """));

        var globalConst = findFunction("THRESHOLD", module.functional());
        var localConst = findFunction("__config__scope_4_0__local_const_0_threshold", module.functional());

        assertThat(globalConst.comments()).containsExactly("Global threshold");
        assertThat(localConst.comments()).containsExactly("Internal threshold");
    }

    @Test
    @DisplayName("should reject missing separator after local definitions")
    void rejectMissingSeparatorAfterLocalDefinitions() {
        var result = new CapybaraParser().parseModule(new RawModule("Test", "/parser", """
                fun foo(n: int): int =
                    fun __inc(x: int): int = x + 1
                    __inc(n)
                """));

        assertThat(result).isInstanceOf(Result.Error.class);
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
    @DisplayName("should parse index access with multiple arguments")
    void parseIndexAccessWithMultipleArguments() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                data Cell { name: string }
                data Matrix { cell: Cell }
                fun test(matrix: Matrix): string = matrix[1, 2].name
                """));

        var function = findFunction("test", module.functional());
        assertThat(function.expression()).isInstanceOf(FieldAccess.class);
        var fieldAccess = (FieldAccess) function.expression();
        assertThat(fieldAccess.field()).isEqualTo("name");
        assertThat(fieldAccess.source()).isInstanceOf(IndexExpression.class);
        var indexExpression = (IndexExpression) fieldAccess.source();
        assertThat(indexExpression.arguments()).hasSize(2);
        assertThat(indexExpression.arguments().get(0)).isInstanceOf(IntValue.class);
        assertThat(((IntValue) indexExpression.arguments().get(0)).intValue()).isEqualTo("1");
        assertThat(indexExpression.arguments().get(1)).isInstanceOf(IntValue.class);
        assertThat(((IntValue) indexExpression.arguments().get(1)).intValue()).isEqualTo("2");
    }

    @Test
    @DisplayName("should parse empty index access for compiler diagnostics")
    void parseEmptyIndexAccessForCompilerDiagnostics() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                data Box { value: string }
                fun test(box: Box): string = box[]
                """));

        var function = findFunction("test", module.functional());
        assertThat(function.expression()).isInstanceOf(IndexExpression.class);
        var indexExpression = (IndexExpression) function.expression();
        assertThat(indexExpression.arguments()).isEmpty();
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
    @DisplayName("should parse modulo and comparison before boolean and or")
    void parseModuloAndComparisonBeforeBooleanAndOr() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                fun leap_year(year: int): bool =
                    (year % 4 == 0 & year % 100 != 0) | year % 400 == 0
                """));

        var function = findFunction("leap_year", module.functional());
        assertThat(function.expression()).isInstanceOf(InfixExpression.class);

        var orExpression = (InfixExpression) function.expression();
        assertThat(orExpression.operator()).isEqualTo(InfixOperator.PIPE);

        assertThat(orExpression.left()).isInstanceOf(InfixExpression.class);
        var andExpression = (InfixExpression) orExpression.left();
        assertThat(andExpression.operator()).isEqualTo(InfixOperator.AND);

        assertThat(andExpression.left()).isInstanceOf(InfixExpression.class);
        assertThat(((InfixExpression) andExpression.left()).operator()).isEqualTo(InfixOperator.EQUAL);

        assertThat(andExpression.right()).isInstanceOf(InfixExpression.class);
        assertThat(((InfixExpression) andExpression.right()).operator()).isEqualTo(InfixOperator.NOTEQUAL);

        assertThat(orExpression.right()).isInstanceOf(InfixExpression.class);
        assertThat(((InfixExpression) orExpression.right()).operator()).isEqualTo(InfixOperator.EQUAL);
    }

    @Test
    @DisplayName("should parse arithmetic before comparison equality and boolean operators")
    void parseArithmeticBeforeComparisonEqualityAndBooleanOperators() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                fun precedence(): bool =
                    1 + 2 * 3 > 6 == true & 2 * 3 ^ 2 == 18 | false
                """));

        var function = findFunction("precedence", module.functional());
        assertThat(function.expression()).isInstanceOf(InfixExpression.class);

        var orExpression = (InfixExpression) function.expression();
        assertThat(orExpression.operator()).isEqualTo(InfixOperator.PIPE);
        assertThat(orExpression.right()).isInstanceOf(BooleanValue.class);

        assertThat(orExpression.left()).isInstanceOf(InfixExpression.class);
        var andExpression = (InfixExpression) orExpression.left();
        assertThat(andExpression.operator()).isEqualTo(InfixOperator.AND);

        assertThat(andExpression.left()).isInstanceOf(InfixExpression.class);
        var equalityLeft = (InfixExpression) andExpression.left();
        assertThat(equalityLeft.operator()).isEqualTo(InfixOperator.EQUAL);
        assertThat(equalityLeft.left()).isInstanceOf(InfixExpression.class);
        assertThat(((InfixExpression) equalityLeft.left()).operator()).isEqualTo(InfixOperator.GT);

        var greaterThan = (InfixExpression) equalityLeft.left();
        assertThat(greaterThan.left()).isInstanceOf(InfixExpression.class);
        assertThat(((InfixExpression) greaterThan.left()).operator()).isEqualTo(InfixOperator.PLUS);

        var plus = (InfixExpression) greaterThan.left();
        assertThat(plus.right()).isInstanceOf(InfixExpression.class);
        assertThat(((InfixExpression) plus.right()).operator()).isEqualTo(InfixOperator.MUL);

        assertThat(andExpression.right()).isInstanceOf(InfixExpression.class);
        var equalityRight = (InfixExpression) andExpression.right();
        assertThat(equalityRight.operator()).isEqualTo(InfixOperator.EQUAL);
        assertThat(equalityRight.left()).isInstanceOf(InfixExpression.class);
        assertThat(((InfixExpression) equalityRight.left()).operator()).isEqualTo(InfixOperator.MUL);

        var mul = (InfixExpression) equalityRight.left();
        assertThat(mul.right()).isInstanceOf(InfixExpression.class);
        assertThat(((InfixExpression) mul.right()).operator()).isEqualTo(InfixOperator.POWER);
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
    @DisplayName("should parse bare collection type patterns as any collections")
    void parseBareCollectionTypePatternsAsAnyCollections() {
        var module = parseSuccess(new RawModule("Test", "/parser", """
                fun test(value: any): int =
                    match value with
                    case list -> 1
                    case set s -> 2
                    case dict _ -> 3
                    case _ -> 0
                """));

        var function = findFunction("test", module.functional());
        assertThat(function.expression()).isInstanceOf(MatchExpression.class);
        var expression = (MatchExpression) function.expression();
        assertThat(expression.cases()).hasSize(4);
        assertThat(expression.cases().get(0).pattern())
                .isEqualTo(new MatchExpression.TypedPattern(new CollectionType.ListType(PrimitiveType.ANY), "__ignored"));
        assertThat(expression.cases().get(1).pattern())
                .isEqualTo(new MatchExpression.TypedPattern(new CollectionType.SetType(PrimitiveType.ANY), "s"));
        assertThat(expression.cases().get(2).pattern())
                .isEqualTo(new MatchExpression.TypedPattern(new CollectionType.DictType(PrimitiveType.ANY), "__ignored"));
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
                    ---
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
                    ---
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
