package dev.capylang.generator.java;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.api.Test;
import dev.capylang.compiler.*;
import dev.capylang.compiler.parser.RawModule;
import dev.capylang.generator.JavaGenerator;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Stream;

import static java.util.stream.Collectors.joining;
import static org.assertj.core.api.Assertions.assertThat;
import static dev.capylang.compiler.CompiledExpressionPrinter.printExpression;
import static dev.capylang.compiler.PrimitiveLinkedType.ANY;

class JavaExpressionEvaluatorTest {
    static {
        var rootLogger = Logger.getLogger("");
        rootLogger.setLevel(Level.FINE);
        for (Handler handler : rootLogger.getHandlers()) {
            handler.setLevel(Level.FINE);
        }
    }

    @ParameterizedTest(name = "{index}: should compile method {0} to return {2}")
    @MethodSource
    void returnType(String name, String fun, CompiledType expectedReturnType) {
        var program = compileProgram(fun);
        var returnType = findFunction(name, program).map(CompiledFunction::returnType);
        assertThat(returnType).contains(expectedReturnType);
    }

    static Stream<Arguments> returnType() {
        return Stream.of(
                Arguments.of(
                        "list_of_obj",
                        "fun list_of_obj() = []",
                        new CollectionLinkedType.CompiledList(ANY)),
                Arguments.of(
                        "set_of_obj",
                        "fun set_of_obj() = {}",
                        new CollectionLinkedType.CompiledSet(ANY)),
                Arguments.of(
                        "dict_of_obj",
                        "fun dict_of_obj() = { \"one\": 1, }",
                        new CollectionLinkedType.CompiledDict(PrimitiveLinkedType.INT))
        );
    }

    @ParameterizedTest(name = "{index}: should `{0}`")
    @MethodSource
    void wild(String name, String fun, String expected) {
        var program = compileProgram(fun);
        var expression = findFunction(name, program)
                .map(CompiledFunction::expression)
                .orElseThrow();
        printExpression(expression);

        var evaluated = JavaExpressionEvaluator.evaluateExpression(expression);

        assertThat(evaluated).isEqualToNormalizingNewlines(expected);
    }

    private static CompiledProgram compileProgram(String fun) {
        return compileProgram("test", "/foo/boo", fun);
    }

    private static CompiledProgram compileProgram(String name, String path, String source) {
        var programResult = CapybaraCompiler.INSTANCE.compile(List.of(new RawModule(name, path, source)), new java.util.TreeSet<>());
        if (programResult instanceof Result.Error<CompiledProgram> er) {
            throw new AssertionError(er.errors()
                    .stream()
                    .map(Result.Error.SingleError::message)
                    .collect(joining(", ")));
        }
        return ((Result.Success<CompiledProgram>) programResult).value();
    }

    private static Optional<CompiledFunction> findFunction(String name, CompiledProgram program) {
        return program.modules()
                .stream()
                .map(CompiledModule::functions)
                .flatMap(Collection::stream)
                .filter(f -> f.name().equals(name))
                .findAny();
    }

    @Test
    void dictKeysMustBeStrings() {
        var programResult = CapybaraCompiler.INSTANCE.compile(List.of(new RawModule("test", "/foo/boo", """
                fun invalid_dict() = {
                    1: 1
                }
                """)), new java.util.TreeSet<>());
        assertThat(programResult).isInstanceOf(Result.Error.class);
        var error = (Result.Error<CompiledProgram>) programResult;
        assertThat(error.errors().stream().map(Result.Error.SingleError::message).collect(joining(",")))
                .contains("dict keys must be of type `STRING`");
    }

    @Test
    void shouldGenerateCommentsForDataAndTypeDeclarations() {
        var program = compileProgram("""
                /// Complete report
                data JUnitTestSuite { name: string }

                /// Complete report
                data JUnitReport { suites: list[JUnitTestSuite] }

                /// Node type
                type JUnitNode = JUnitReport
                """);

        var generated = new JavaGenerator().generate(program).modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated).contains("Complete report");
        assertThat(generated).contains("JUnitReport");
        assertThat(generated).contains("Node type");
        assertThat(generated).contains("JUnitNode");
    }

    @Test
    void shouldGenerateTimedCapyTestMethodForCapyTestModule() {
        var program = compileProgram("CapyTest", "/capy/test", """
                data Assertion { result: bool, message: string, type: string }
                data StringAssert { value: string, assertions: list[Assertion] }
                type Assert { assertions: list[Assertion] } = StringAssert
                single Passed
                type TestResult = Passed
                data TestCase { name: string, result: TestResult, assertions_count: int, execution_time: long }

                fun _execute(assertions: list[Assertion]): TestResult = Passed
                fun test(name: string, assert_: Assert): TestCase =
                    TestCase {
                        name: name,
                        result: _execute(assert_.assertions),
                        assertions_count: assert_.assertions.size,
                        execution_time: -1
                    }
                """);

        var generated = new JavaGenerator().generate(program).modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated).contains("var start = System.currentTimeMillis();");
        assertThat(generated).contains("var result = _execute((assert_).assertions());");
        assertThat(generated).contains("var delta = System.currentTimeMillis() - start;");
        assertThat(generated).contains("return new TestCase(name, result, ((assert_).assertions()).size(), (delta/1000));");
    }

    @Test
    void shouldNotGenerateTimedCapyTestMethodOutsideCapyTestModule() {
        var program = compileProgram("NotCapyTest", "/foo/bar", """
                data Assertion { result: bool, message: string, type: string }
                data StringAssert { value: string, assertions: list[Assertion] }
                type Assert { assertions: list[Assertion] } = StringAssert
                single Passed
                type TestResult = Passed
                data TestCase { name: string, result: TestResult, assertions_count: int, execution_time: long }

                fun _execute(assertions: list[Assertion]): TestResult = Passed
                fun test(name: string, assert_: Assert): TestCase =
                    TestCase {
                        name: name,
                        result: _execute(assert_.assertions),
                        assertions_count: assert_.assertions.size,
                        execution_time: -1
                    }
                """);

        var generated = new JavaGenerator().generate(program).modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated).doesNotContain("var start = System.currentTimeMillis();");
        assertThat(generated).contains("return new TestCase(name, _execute((assert_).assertions()), ((assert_).assertions()).size(), (0-1));");
    }

    @Test
    void shouldNotGenerateDuplicateRecordToStringWhenCapybaraDefinesToString() {
        var program = compileProgram("Pretty", "/foo/bar", """
                data Pretty { value: string }
                fun Pretty.to_string(): string = "pretty:" + this.value
                """);

        var generated = new JavaGenerator().generate(program).modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated).contains("public java.lang.String toString()");
        assertThat(generated).doesNotContain("@Override public java.lang.String toString() { return \"Pretty { ");
    }

    @Test
    void shouldImportOptionTypesAsClassesInsteadOfStaticMembers() {
        var program = compileProgram("LocalOption", "/foo/bar", """
                from /capy/lang/Option import { Option, Some, None }
                from /capy/lang/Result import { * }

                fun parse(value: int): Result[int] =
                    data __Parse[T] { buffer: string, value: T }
                    fun __unwrap(parse: __Parse[Option[int]]): Result[__Parse[int]] =
                        match parse.value with
                        case Some { inner } -> Success { __Parse { buffer: parse.buffer, value: inner } }
                        case None -> Error { "missing" }
                    __unwrap(__Parse { buffer: "", value: Some { value } }) | parsed => Success { parsed.value }
                """);

        var generated = new JavaGenerator().generate(program).modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated).contains("import capy.lang.Option;");
        assertThat(generated).contains("import capy.lang.Option.Some;");
        assertThat(generated).doesNotContain("import static capy.lang.Option.Option;");
    }

    @Test
    void shouldGenerateRecordWithMethod() {
        var program = compileProgram("With", "/foo/bar", """
                data Foo { a: int, b: string, c: double }
                fun update(foo: Foo): Foo = foo.with(a: foo.a + 1, b: \"x\")
                """);

        var generated = new JavaGenerator().generate(program).modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated).contains("public Foo with(int a, java.lang.String b, double c)");
        assertThat(generated).contains("return new Foo(a, b, c);");
    }

    @Test
    void shouldGenerateChainedWithCalls() {
        var evaluated = JavaExpressionEvaluator.evaluateExpression(findFunction("update", compileProgram("With", "/foo/bar", """
                data Foo { a: int, b: string, c: double }
                fun update(foo: Foo): Foo = foo.with(a: foo.a + 1).with(b: \"x\")
                """)).orElseThrow().expression());

        assertThat(evaluated).contains(".with(");
        assertThat(evaluated).contains(").with(");
    }

    static Stream<Arguments> wild() {
        return Stream.of(
                Arguments.of(
                        "wild_if",
                        """
                                fun wild_if(x: int): string =
                                    let a = "unsued?"
                                    if ({
                                        let a = x * 2
                                        a > 2
                                    }) then {
                                        let a = x * x
                                        "I'm happy " + a
                                       } else {
                                        let a = x / 2
                                        "I'm not happy " + a
                                        }
                                """,
                        """
                                var a = "unsued?";
                                var a_j1 = (x*2);
                                var a_j2 = (x*x);
                                var a_j3 = (x/2);
                                return ((a_j1>2)) ? (("I'm happy "+java.lang.String.valueOf(a_j2))) : (("I'm not happy "+java.lang.String.valueOf(a_j3)));"""
                ),
                Arguments.of(
                        "wild_infix",
                        """
                                fun wild_infix(a: int, b: int): int =
                                    {
                                        let x = a * 2
                                        x - 1
                                    } / {
                                        let x = if(b!=0) then b else 1
                                        x*2
                                    }
                                """,
                        """
                                var x = (a*2);
                                var x_j1 = ((b!=0)) ? (b) : (1);
                                return ((x-1)/(x_j1*2));"""
                ),
                Arguments.of(
                        "wild_if_nested",
                        """
                                fun wild_if_nested(a: int, b: int): int =
                                    if (a > 0) then {
                                        {
                                            let x = a * 2
                                            x + 1
                                        }
                                    } else {
                                        {
                                            let x = b * 3
                                            x - 1
                                        }
                                    }
                                """,
                        """
                                var x = (a*2);
                                var x_j1 = (b*3);
                                return ((a>0)) ? ((x+1)) : ((x_j1-1));"""
                ),
                Arguments.of(
                        "wild_infix_single_side_let",
                        """
                                fun wild_infix_single_side_let(a: int): int =
                                    a + {
                                        let x = a * 2
                                        x + 1
                                    }
                                """,
                        """
                                var x = (a*2);
                                return (a+(x+1));"""
                ),
                Arguments.of(
                        "list_of_obj",
                        "fun list_of_obj() = []",
                        "return java.util.List.of();"
                ),
                Arguments.of(
                        "dict_of_obj",
                        """
                                fun dict_of_obj() = {
                                    "one": 1,
                                    "two": 2,
                                    "three": 3,
                                }
                                """,
                        "return java.util.stream.Stream.of(java.util.Map.entry(\"one\", 1), java.util.Map.entry(\"two\", 2), java.util.Map.entry(\"three\", 3)).collect(java.util.stream.Collectors.toMap(java.util.Map.Entry::getKey, java.util.Map.Entry::getValue, (oldValue, newValue) -> newValue, java.util.LinkedHashMap::new));"
                ),
                Arguments.of(
                        "single_quote_string",
                        "fun single_quote_string(): string = 'hello'",
                        "return \"hello\";"
                ),
                Arguments.of(
                        "summon_tom",
                        """
                                type Knight = EnglishKnight | Tom
                                data EnglishKnight { power: float }
                                single Tom
                                fun summon_tom(): Knight = Tom
                                """,
                        "return Tom.INSTANCE;"
                ),
                Arguments.of(
                        "pipe_map",
                        """
                                fun pipe_map(l: list[int]): list[int] =
                                    l | x => x * 2
                                """,
                        "return l.stream().map(x -> ((x*2))).toList();"
                ),
                Arguments.of(
                        "map",
                        """
                                fun map(l: list[int]) = l | :double

                                fun double(x: int) = x * x
                                """,
                        "return l.stream().map(it -> (double_(it))).toList();"
                ),
                Arguments.of(
                        "pipe_filter_out",
                        """
                                fun pipe_filter_out(l: list[int]): list[int] =
                                    l |- x => x > 2
                                """,
                        "return l.stream().filter(x -> !((x>2))).toList();"
                ),
                Arguments.of(
                        "pipe_reduce",
                        """
                                fun pipe_reduce(l: list[int]): int =
                                    l |> 0, (a, b) => a + b
                                """,
                        "return l.stream().reduce((a, b) -> ((a+b))).orElse(0);"
                ),
                Arguments.of(
                        "pipe_flat_map",
                        """
                                fun pipe_flat_map(l: list[int]): list[int] =
                                    l |* x => [x, x + 1]
                                """,
                        "return l.stream().flatMap(x -> (java.util.List.of(x, (x+1))).stream()).toList();"
                )
        );
    }
}

