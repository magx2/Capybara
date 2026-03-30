package dev.capylang.parser;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.Optional;
import java.util.stream.Stream;

import dev.capylang.compiler.parser.*;

import static org.assertj.core.api.Assertions.assertThat;

class CapybaraParserTest {

    @ParameterizedTest(name = "{index}: should parse function {0}")
    @MethodSource
    void parse(String name, String code) {
        var module = new CapybaraParser().parseModule(new RawModule("Test", "/parser", code));
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
        var module = new CapybaraParser().parseModule(new RawModule("Test", "/parser", "const E: double = 2."));
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
        var module = new CapybaraParser().parseModule(new RawModule("Test", "/parser", code));

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
        var module = new CapybaraParser().parseModule(new RawModule("Test", "/parser", """
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
    @DisplayName("should parse bang over field access")
    void parseBangOverFieldAccess() {
        var module = new CapybaraParser().parseModule(new RawModule("Test", "/parser", """
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
        var module = new CapybaraParser().parseModule(new RawModule("Test", "/parser", """
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
}



