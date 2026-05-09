package dev.capylang.generator.java;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import dev.capylang.compiler.*;
import dev.capylang.compiler.expression.CompiledLambdaExpression;
import dev.capylang.compiler.parser.RawModule;
import dev.capylang.generator.JavaGenerator;

import javax.tools.DiagnosticCollector;
import javax.tools.JavaFileObject;
import javax.tools.ToolProvider;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.Optional;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.logging.Logger;
import java.util.stream.Stream;

import static java.util.stream.Collectors.joining;
import static org.assertj.core.api.Assertions.assertThat;
import static dev.capylang.compiler.CompiledExpressionPrinter.printExpression;
import static dev.capylang.compiler.PrimitiveLinkedType.ANY;

class JavaExpressionEvaluatorTest {
    private static final Object STANDARD_LIBRARY_LOCK = new Object();
    private static Path standardLibraryClassesDir;

    @TempDir
    Path tempDir;

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
        return compileProgram(List.of(new RawModule(name, path, source)));
    }

    private static CompiledProgram compileProgram(List<RawModule> modules) {
        var programResult = CapybaraCompiler.INSTANCE.compile(modules, new java.util.TreeSet<>());
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
    void shouldPreferQualifierWhenFallingBackToOverrideBySimpleName() {
        var overrides = new LinkedHashMap<String, String>();
        overrides.put("other.Module.choose|CompiledList[elementType=INT]", "choose_other");
        overrides.put("target.Module.choose|CompiledList[elementType=INT]", "choose_target");
        JavaExpressionEvaluator.setFunctionNameOverrides(Map.copyOf(overrides));

        var resolved = JavaExpressionEvaluator.findOverrideBySimpleName(
                "target.Module.choose",
                "CompiledList[elementType=INT]");

        assertThat(resolved).contains("choose_target");
    }

    @Test
    void shouldFallBackToUnqualifiedOverrideWhenQualifiedEntryIsMissing() {
        var overrides = new LinkedHashMap<String, String>();
        overrides.put("choose|CompiledList[elementType=INT]", "choose_target");
        overrides.put("other.Module.choose|CompiledList[elementType=INT]", "choose_other");
        JavaExpressionEvaluator.setFunctionNameOverrides(Map.copyOf(overrides));

        var resolved = JavaExpressionEvaluator.findOverrideBySimpleName(
                "target.Module.choose",
                "CompiledList[elementType=INT]");

        assertThat(resolved).contains("choose_target");
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
    void shouldLowerPartialFunctionBindingToLambda() {
        var program = compileProgram("""
                fun add(a: int, b: int): int = a + b
                fun add_10(): int => int = add(10, _)
                """);

        var expression = findFunction("add_10", program)
                .map(CompiledFunction::expression)
                .orElseThrow();

        assertThat(expression).isInstanceOf(CompiledLambdaExpression.class);
        var lambda = (CompiledLambdaExpression) expression;
        assertThat(lambda.argumentName()).isEqualTo("$capybaraPartial1");
        assertThat(lambda.functionType()).isEqualTo(new CompiledFunctionType(PrimitiveLinkedType.INT, PrimitiveLinkedType.INT));
    }

    @Test
    void shouldLowerMultiplePartialFunctionBindingPlaceholdersInOrder() {
        var program = compileProgram("""
                fun foo(a: int, b: string, c: double, d: int): string = b
                fun bind_b(b: string): (int, double, int) => string = foo(_, b, _, _)
                """);

        var expression = findFunction("bind_b", program)
                .map(CompiledFunction::expression)
                .orElseThrow();

        assertThat(expression).isInstanceOf(CompiledLambdaExpression.class);
        var first = (CompiledLambdaExpression) expression;
        assertThat(first.argumentName()).isEqualTo("$capybaraPartial1");
        assertThat(first.functionType().argumentType()).isEqualTo(PrimitiveLinkedType.INT);
        assertThat(first.expression()).isInstanceOf(CompiledLambdaExpression.class);
        var second = (CompiledLambdaExpression) first.expression();
        assertThat(second.argumentName()).isEqualTo("$capybaraPartial2");
        assertThat(second.functionType().argumentType()).isEqualTo(PrimitiveLinkedType.DOUBLE);
        assertThat(second.expression()).isInstanceOf(CompiledLambdaExpression.class);
        var third = (CompiledLambdaExpression) second.expression();
        assertThat(third.argumentName()).isEqualTo("$capybaraPartial3");
        assertThat(third.functionType().argumentType()).isEqualTo(PrimitiveLinkedType.INT);
    }

    @Test
    void shouldKeepQualifiedOverrideOwnershipPerModule() {
        var program = compileProgram(List.of(
                new RawModule("Left", "/alpha", """
                        fun choose(values: list[int]): int = 100 + values.size
                        fun choose(values: list[long]): int = 200 + values.size
                        """),
                new RawModule("Right", "/beta", """
                        fun choose(values: list[int]): int = 300 + values.size
                        fun choose(values: list[long]): int = 400 + values.size
                        """),
                new RawModule("Main", "/app", """
                        fun ints(): list[int] = [1, 2, 3]
                        fun longs(): list[long] = [1L, 2L, 3L]
                        fun left_choice(): int = /alpha/Left.choose(ints())
                        fun left_choice_long(): int = /alpha/Left.choose(longs())
                        fun right_choice(): int = /beta/Right.choose(ints())
                        fun right_choice_long(): int = /beta/Right.choose(longs())
                        """)
        ));

        var generated = new JavaGenerator().generate(program).modules().stream()
                .filter(module -> module.relativePath().equals(java.nio.file.Path.of("app", "Main.java")))
                .map(dev.capylang.generator.GeneratedModule::code)
                .findFirst()
                .orElseThrow();

        assertThat(generated).contains("return alpha.Left.choose__compiledlist_elementtype_int(app.Main.ints());");
        assertThat(generated).contains("return alpha.Left.choose__compiledlist_elementtype_long(app.Main.longs());");
        assertThat(generated).contains("return beta.Right.choose__compiledlist_elementtype_int(app.Main.ints());");
        assertThat(generated).contains("return beta.Right.choose__compiledlist_elementtype_long(app.Main.longs());");
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
        assertThat(generated).contains("/// Complete report");
        assertThat(generated).contains("/// Node type");
    }

    @Test
    void shouldGenerateCommentsForTopLevelAndLocalConsts() {
        var program = compileProgram("""
                /// Global threshold
                const THRESHOLD: int = 10

                fun config(): int =
                    /// Internal threshold
                    const __threshold: int = THRESHOLD
                    ---
                    __threshold
                """);

        var generated = new JavaGenerator().generate(program).modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated).contains("/// Global threshold");
        assertThat(generated).contains("public static final int");
        assertThat(generated).contains("/// Internal threshold");
        assertThat(generated).contains("__configScope").contains("LocalConst0Threshold");
        assertThat(generated).doesNotContain(" * Global threshold");
    }

    @Test
    void shouldGenerateCapyTestMethodFromSourceForCapyTestModule() {
        var program = compileProgram("CapyTest", "/capy/test", """
                data Assertion { result: bool, message: string, type: string }
                data StringAssert { value: string, assertions: list[Assertion] }
                type Assert { assertions: list[Assertion] } = StringAssert
                single Passed
                type TestResult = Passed
                data TestCase { name: string, result: TestResult, assertions_count: int, execution_time: long }

                private fun execute(assertions: list[Assertion]): TestResult = Passed {}
                fun test(name: string, assert_: Assert): TestCase =
                    TestCase {
                        name: name,
                        result: execute(assert_.assertions),
                        assertions_count: assert_.assertions.size,
                        execution_time: -1
                    }
                """);

        var generated = new JavaGenerator().generate(program).modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated).doesNotContain("System.currentTimeMillis()");
        assertThat(generated).contains("execute((assert_).assertions())");
        assertThat(generated).contains("(((long) 0)-((long) 1))");
    }

    @Test
    void shouldGenerateCapyTestNamedMethodFromSourceOutsideCapyTestModule() {
        var program = compileProgram("NotCapyTest", "/foo/bar", """
                data Assertion { result: bool, message: string, type: string }
                data StringAssert { value: string, assertions: list[Assertion] }
                type Assert { assertions: list[Assertion] } = StringAssert
                single Passed
                type TestResult = Passed
                data TestCase { name: string, result: TestResult, assertions_count: int, execution_time: long }

                private fun execute(assertions: list[Assertion]): TestResult = Passed {}
                fun test(name: string, assert_: Assert): TestCase =
                    TestCase {
                        name: name,
                        result: execute(assert_.assertions),
                        assertions_count: assert_.assertions.size,
                        execution_time: -1
                    }
                """);

        var generated = new JavaGenerator().generate(program).modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated).doesNotContain("var start = System.currentTimeMillis();");
        assertThat(generated).contains("return new TestCase(name, foo.bar.NotCapyTest.execute((assert_).assertions()), ((assert_).assertions()).size(), (((long) 0)-((long) 1)));");
    }

    @Test
    void shouldGenerateRuntimeDataValueCarrierForRecords() {
        var generatedProgram = new JavaGenerator().generate(compileProgram("Consumer", "/foo/app", """
                data User { name: string, age: int }
                """));
        var generated = generatedProgram.modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated).contains("record User(java.lang.String name, int age) implements dev.capylang.CapybaraDataValue");
        assertThat(generated).contains("public java.lang.Object capybaraDataValueInfo()");
        assertThat(generated).contains("new capy.metaProg.Reflection.DataValueInfo(\"User\"");
        assertThat(generated).contains("new capy.metaProg.Reflection.FieldValueInfo(\"name\"");
    }

    @Test
    void shouldGenerateRuntimeDataValueCarrierForSingletonAndEnumValues() {
        var generatedProgram = new JavaGenerator().generate(compileProgram("Consumer", "/foo/app", """
                single Lonely
                enum Color { RED, BLUE }
                """));
        var generated = generatedProgram.modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated).contains("enum Lonely implements dev.capylang.CapybaraDataValue {INSTANCE;");
        assertThat(generated).contains("enum Color implements dev.capylang.CapybaraDataValue {RED, BLUE;");
        assertThat(generated).contains("new capy.metaProg.Reflection.DataValueInfo(\"Lonely\"");
        assertThat(generated).contains("case RED -> new capy.metaProg.Reflection.DataValueInfo(\"RED\"");
    }

    @Test
    void shouldQualifyConstructedEnumValuesInGeneratedJava() {
        var generatedProgram = new JavaGenerator().generate(compileProgram("Consumer", "/foo/app", """
                enum PathRoot { RELATIVE, ABSOLUTE }
                fun root(): PathRoot = PathRoot.ABSOLUTE {}
                fun inferred_root() = PathRoot.RELATIVE {}
                """));
        var generated = generatedProgram.modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated)
                .contains("public static PathRoot inferredRoot()")
                .contains("return PathRoot.ABSOLUTE;")
                .contains("return PathRoot.RELATIVE;");
        assertGeneratedJavaCompiles(generatedProgram);
    }

    @Test
    void shouldCompileEnumValuesWithoutBraces() {
        var generatedProgram = new JavaGenerator().generate(compileProgram("Consumer", "/foo/app", """
                enum PathRoot { RELATIVE, ABSOLUTE }
                fun root(): PathRoot = PathRoot.ABSOLUTE
                fun inferred_root() = RELATIVE
                fun root_name(): string = ABSOLUTE.name
                """));
        var generated = generatedProgram.modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated)
                .contains("return PathRoot.ABSOLUTE;")
                .contains("return PathRoot.RELATIVE;");
        assertGeneratedJavaCompiles(generatedProgram);
    }

    @Test
    void shouldStillRequireBracesForSingleValues() {
        var programResult = CapybaraCompiler.INSTANCE.compile(List.of(new RawModule("test", "/foo/boo", """
                single Done
                fun done(): Done = Done
                """)), new java.util.TreeSet<>());
        assertThat(programResult).isInstanceOf(Result.Error.class);
        var error = (Result.Error<CompiledProgram>) programResult;
        assertThat(error.errors().stream().map(Result.Error.SingleError::message).collect(joining(",")))
                .contains("Singleton data value `Done` must be constructed with `Done {}`");
    }

    @Test
    void shouldGenerateRuntimeCarrierCallForReflectionValueOnGenericData() {
        var generatedProgram = new JavaGenerator().generate(compileProgram(List.of(
                reflectionMetadataModule(),
                new RawModule("Consumer", "/foo/app", """
                        from /capy/meta_prog/Reflection import { DataValueInfo, reflection }

                        fun reflect_data(obj: data): DataValueInfo = reflection(obj)
                        """)
        )));
        var generated = generatedProgram.modules().stream()
                .filter(module -> module.relativePath().equals(Path.of("foo", "app", "Consumer.java")))
                .map(dev.capylang.generator.GeneratedModule::code)
                .findFirst()
                .orElseThrow();

        assertThat(generated).contains("instanceof dev.capylang.CapybaraDataValue");
        assertThat(generated).contains("reflection expects a Capybara data value");
        assertThat(generated).contains(".capybaraDataValueInfo()");
    }

    @Test
    void shouldKeepConcreteReflectionValueGeneratingStaticMetadata() {
        var generatedProgram = new JavaGenerator().generate(compileProgram(List.of(
                reflectionMetadataModule(),
                new RawModule("Consumer", "/foo/app", """
                        from /capy/meta_prog/Reflection import { DataValueInfo, reflection }

                        data User { name: string }

                        fun reflect_user(user: User): DataValueInfo = reflection(user)
                        """)
        )));
        var generated = generatedProgram.modules().stream()
                .filter(module -> module.relativePath().equals(Path.of("foo", "app", "Consumer.java")))
                .map(dev.capylang.generator.GeneratedModule::code)
                .findFirst()
                .orElseThrow();

        assertThat(generated).contains("new capy.metaProg.Reflection.DataValueInfo(\"User\"");
        assertThat(generated).doesNotContain("reflection expects a Capybara data value");
    }

    @Test
    void shouldCastCollectionTypedPatternBindingsBeforeUse() {
        var generatedProgram = new JavaGenerator().generate(compileProgram("CollectionTypedMatch", "/foo/bar", """
                fun stringify(value: any): list[string] =
                    match value with
                    case list[any] items -> items | item => "" + item
                    case set[any] items -> items |> [], (acc, item) => acc + ("" + item)
                    case dict[any] items -> items |> [], (acc, key, item) => acc + (key + ":" + item)
                    case _ -> []
                """));
        var generated = generatedProgram.modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated).contains("((java.util.List<java.lang.Object>)");
        assertThat(generated).contains("((java.util.Set<java.lang.Object>)");
        assertThat(generated).contains("((java.util.Map<java.lang.String, java.lang.Object>)");
        assertGeneratedJavaCompiles(generatedProgram);
    }

    @Test
    void shouldGuardDataTypedPatternWithCapybaraDataValue() {
        var generatedProgram = new JavaGenerator().generate(compileProgram("DataTypedMatch", "/foo/bar", """
                fun is_data(value: any): bool =
                    match value with
                    case data d -> true
                    case _ -> false
                """));
        var generated = generatedProgram.modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated).contains("instanceof dev.capylang.CapybaraDataValue");
        assertThat(generated).doesNotContain("instanceof java.util.List");
        assertGeneratedJavaCompiles(generatedProgram);
    }

    @Test
    void shouldGenerateNativeRandomSeedMethod() {
        var program = compileProgram("Random", "/capy/lang", """
                data Seed { value: long }

                fun seed(): Seed = <native>
                """);

        var generated = new JavaGenerator().generate(program).modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated).contains("return new Seed(java.util.concurrent.ThreadLocalRandom.current().nextLong());");
        assertThat(generated).doesNotContain("__capybaraUnsupported");
    }

    @Test
    void shouldPreserveSourceRandomSeedMethodWhenNotNative() {
        var program = compileProgram("Random", "/capy/lang", """
                data Seed { value: long }

                fun seed(): Seed = Seed { value: 1L }
                """);

        var generated = new JavaGenerator().generate(program).modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated).contains("return new Seed(1L);");
        assertThat(generated).doesNotContain("ThreadLocalRandom");
    }

    @Test
    void shouldGenerateStaticImportsForUpperSnakeConstants() {
        var program = compileProgram(List.of(
                new RawModule("Bounds", "/foo", """
                        const FLOAT_BOUND: long = 16777216L
                        """),
                new RawModule("Consumer", "/bar", """
                        from /foo/Bounds import { FLOAT_BOUND }

                        fun value(): long = FLOAT_BOUND
                        """)
        ));

        var generatedProgram = new JavaGenerator().generate(program);
        var generated = generatedProgram.modules().stream()
                .filter(module -> module.relativePath().equals(Path.of("bar", "Consumer.java")))
                .map(dev.capylang.generator.GeneratedModule::code)
                .findFirst()
                .orElseThrow();

        assertThat(generated).contains("import static foo.Bounds.fLOATBOUND;");
        assertThat(generated).doesNotContain("import foo.Bounds.FLOATBOUND;");
        assertGeneratedJavaCompiles(generatedProgram);
    }

    @Test
    void shouldGeneratePrimitiveTypedLetDeclarations() {
        var program = compileProgram("""
                fun copy_double(value: double): double =
                    let copy: double = value
                    copy

                fun copy_float(value: float): float =
                    let copy: float = value
                    copy
                """);

        var generated = new JavaGenerator().generate(program).modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated).contains("double copy = value;");
        assertThat(generated).contains("float copy = value;");
        assertThat(generated).doesNotContain("java.lang.Double copy = value;");
        assertThat(generated).doesNotContain("java.lang.Float copy = value;");
    }

    @Test
    void shouldCoercePrimitiveFunctionCallArgumentsBeforeJavaOverloadResolution() {
        var program = compileProgram("""
                data Box[T] { value: T }

                fun accept(value: double): string = "double"
                fun accept(value: data): string = "data"

                fun use_box(box: Box[double]): string =
                    accept(box.value)
                """);

        var generated = new JavaGenerator().generate(program).modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated).contains("accept(((double) ((java.lang.Double) ((box).value()))))");
    }

    @Test
    void shouldFullyQualifyResultReturnTypeInTopLevelRecordMethods() {
        var program = compileProgram("LocalDateModule", "/foo/bar", """
                from /capy/lang/Result import {*}

                data LocalDate { day: int }
                fun LocalDate.first_day_of_month(): Result[LocalDate] = Success { LocalDate { day: 1 } }
                """);

        var generatedDate = new JavaGenerator().generate(program).modules().stream()
                .filter(module -> module.relativePath().equals(java.nio.file.Path.of("foo", "bar", "LocalDateModule.java")))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(generatedDate).contains("public capy.lang.Result<LocalDate> firstDayOfMonth()");
        assertThat(generatedDate).doesNotContain("public Result<LocalDate> firstDayOfMonth()");
    }

    @Test
    void shouldRewriteQualifiedTailRecursiveCallWhenStaticMethodsUseModuleSuffix() {
        var generatedProgram = new JavaGenerator().generate(compileProgram("Counter", "/foo/bar", """
                data Counter { value: int }

                fun rec sum(n: int, acc: int): int =
                    if n <= 0 then acc else Counter.sum(n - 1, acc + n)
                """));

        var generated = generatedProgram.modules().stream()
                .filter(module -> module.relativePath().equals(Path.of("foo", "bar", "CounterModule.java")))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(generated).contains("while (true)");
        assertThat(generated).contains("continue;");
        assertThat(generated).doesNotContain("return foo.bar.CounterModule.sum(");
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
    void shouldUseCapybaraToStringUtilForGeneratedRecordToString() {
        var program = compileProgram("Pretty", "/foo/bar", """
                data Pretty { value: string }
                """);

        var generated = new JavaGenerator().generate(program).modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated).contains("dev.capylang.CapybaraToStringUtil.toStringValue(value)");
        assertThat(generated).doesNotContain("private static java.lang.String __capybaraToStringValue");
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
                    ---
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

    @Test
    void shouldGeneratePresentOptionCaseForBareSomePattern() {
        var generated = new JavaGenerator().generate(compileProgram("OptionMatch", "/foo/bar", """
                from /capy/lang/Option import { Option, Some, None }

                fun read(value: Option[string]): string =
                    match value with
                    case None -> "none"
                    case Some -> "some"
                """)).modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated).contains(".isPresent()");
        assertThat(generated).doesNotContain("default -> (((java.lang.String) (\"some\")))");
    }

    @Test
    void shouldGenerateBooleanLiteralMatchCasesUsingBooleanPatternGuards() {
        var generated = new JavaGenerator().generate(compileProgram("BoolMatch", "/foo/bar", """
                fun mood(value: bool): string =
                    match value with
                    case true -> "happy"
                    case false -> "sad"
                """)).modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated).contains("case java.lang.Boolean");
        assertThat(generated).contains("java.util.Objects.equals");
        assertThat(generated).contains("default -> throw new java.lang.IllegalStateException(\"Unexpected bool value:");
        assertThat(generated).doesNotContain("case true ->");
        assertThat(generated).doesNotContain("case false ->");
    }

    @Test
    void shouldInferSharedParentForConcatenatedSubtypeLists() {
        var program = compileProgram("EmptyLiteralInference", "/foo/bar", """
                type Outcome = ParseSucceeded | ParseFailed
                data ParseSucceeded { source: string }
                data ParseFailed { source: string }

                fun merged_outcomes(values: list[string]) =
                    let succeeded = values |> [], (acc, source) => acc + ParseSucceeded { source }
                    let failed = values |> [], (acc, source) => acc + ParseFailed { source }
                    succeeded + failed
                """);

        var returnType = findFunction("merged_outcomes", program).orElseThrow().returnType();

        assertThat(returnType).isInstanceOf(CollectionLinkedType.CompiledList.class);
        var elementType = ((CollectionLinkedType.CompiledList) returnType).elementType();
        assertThat(elementType).isInstanceOf(CompiledDataParentType.class);
        assertThat(((CompiledDataParentType) elementType).name()).isEqualTo("Outcome");
    }

    @Test
    void shouldGenerateTypedConcatForSubtypeListsWithSharedParent() {
        var program = compileProgram("EmptyLiteralInference", "/foo/bar", """
                type Outcome = ParseSucceeded | ParseFailed
                data ParseSucceeded { source: string }
                data ParseFailed { source: string }
                data OutcomeBatch { outcomes: list[Outcome] }

                fun batch_outcomes(outcomes: list[Outcome]): OutcomeBatch =
                    OutcomeBatch { outcomes }

                fun concat_inferred_parent_subtype_lists(values: list[string]): OutcomeBatch =
                    let succeeded = values |> [], (acc, source) => acc + ParseSucceeded { source }
                    let failed = values |> [], (acc, source) => acc + ParseFailed { source }
                    batch_outcomes(succeeded + failed)
                """);

        var generated = new JavaGenerator().generate(program).modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated).contains("java.util.stream.Stream.<Outcome>concat");
    }

    @Test
    void shouldQualifyBareResultErrorPattern() {
        var generated = new JavaGenerator().generate(compileProgram("ResultMatch", "/foo/bar", """
                from /capy/lang/Result import { * }

                fun recover(value: Result[string]): string =
                    match value with
                    case Error -> "bad"
                    case Success { ok } -> ok
                """)).modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated).contains("case");
        assertThat(generated).contains("Result.Error __ignored");
        assertThat(generated).doesNotContain("case Error __ignored");
    }

    @Test
    void shouldQualifyResultErrorMatchCaseCast() {
        var generated = new JavaGenerator().generate(compileProgram("ResultErrorCast", "/foo/bar", """
                from /capy/lang/Result import { * }

                fun fail(message: string): Result[int] = Error { message }

                fun nested(value: string): Result[int] =
                    match value with
                    case "x" -> fail(value)
                    case _ v ->
                        match v with
                        case "y" -> Error { "bad" }
                        case _ -> Error { "worse" }
                """)).modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated).contains("new capy.lang.Result.Error");
        assertThat(generated).doesNotContain("((Error)");
    }

    @Test
    void shouldQualifyNestedMatchReturningOnlyResultError() {
        var generated = new JavaGenerator().generate(compileProgram("NestedResultErrorCast", "/foo/bar", """
                from /capy/lang/Result import { * }
                from /capy/lang/Option import { * }

                fun nested(value: string): Result[int] =
                    match value with
                    case "" -> Error { "empty" }
                    case _ v ->
                        match v[0] with
                        case None -> Error { "missing" }
                        case Some { next } -> Error { next }
                """)).modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated).contains("new capy.lang.Result.Error");
        assertThat(generated).doesNotContain("((Error)");
        assertThat(generated).contains("((Result)");
    }

    @Test
    void shouldCastGenericConstructorPatternBindingsBeforeFunctionApplication() {
        var generated = new JavaGenerator().generate(compileProgram("GenericResultBinding", "/foo/bar", """
                from /capy/lang/Result import { * }

                type Assert { assertions: list[() => string] } = StringAssert
                data StringAssert { assertions: list[() => string] }
                data ResultAssert[T] { value: Result[T] }

                fun ResultAssert[T].succeeds(assert_: T => Assert): list[() => string] =
                    match this.value with
                    case Error e -> [() => e.message]
                    case Success { value } -> assert_(value).assertions
                """)).modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated).contains("assert_.apply(((T) __capybaraMatchBinding");
    }

    @Test
    void shouldPreserveTypedLambdaArgumentsForResultAssertChains() {
        var generatedProgram = new JavaGenerator().generate(compileProgram("DateTestLike", "/foo/bar", """
                from /capy/lang/Result import { * }
                from /capy/test/Assert import { * }

                data LocalDate { day: int }

                fun test(value: Result[LocalDate]): Assert = assert_that(value).succeeds(LocalDate { day: 1 })
                """));
        var generated = generatedProgram.modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated).contains(".succeeds(new LocalDate(1))");
        assertGeneratedJavaCompiles(generatedProgram);
    }

    @Test
    void shouldSanitizeJavaKeywordsUsedAsLocalNames() {
        var generatedProgram = new JavaGenerator().generate(compileProgram("KeywordLocalNames", "/foo/bar", """
                data Date { day: int }

                fun test_keyword_let(is_valid: bool): bool =
                    let assert: bool = is_valid
                    assert

                fun test_keyword_lambda(values: list[Date]): list[int] =
                    values | assert => assert.day

                fun test_keyword_reduce(values: list[int]): int =
                    let acc = 1
                    values |> 0, (acc, count) => acc + count
                """));
        var generated = generatedProgram.modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated).contains("boolean assert_ =");
        assertThat(generated).contains("return assert_;");
        assertThat(generated).contains(".stream().map(assert_ ->");
        assertGeneratedJavaCompiles(generatedProgram);
    }

    @Test
    void shouldAvoidJavaNameCollisionsAfterKeywordSanitization() {
        var generatedProgram = new JavaGenerator().generate(compileProgram("KeywordSanitizedCollision", "/foo/bar", """
                fun test(assert_: bool): bool =
                    let assert: bool = !assert_
                    assert
                """));
        var generated = generatedProgram.modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated).contains("boolean assert_j1 =");
        assertThat(generated).contains("return assert_j1;");
        assertGeneratedJavaCompiles(generatedProgram);
    }

    @Test
    void shouldKeepPipeReduceMatchStatementsInsideReducerLambda() {
        var generatedProgram = new JavaGenerator().generate(compileProgram("ReduceMatchLambda", "/foo/bar", """
                type Item = Text | Count
                data Text { value: string }
                data Count { value: int }

                fun summarize(items: list[Item]): string =
                    items |> "", (acc, item) =>
                        acc + match item with
                        case Text { text } -> text
                        case Count { count } -> count + ""
                """));
        var generated = generatedProgram.modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated).contains("-> { var __matchValue");
        assertGeneratedJavaCompiles(generatedProgram);
    }

    @Test
    void shouldAvoidPipeReduceLambdaArgumentCollisionWithLetName() {
        var generatedProgram = new JavaGenerator().generate(compileProgram("ReduceLetNameCollision", "/foo/bar", """
                fun flatten(asserts: list[list[int]]): list[int] =
                    let assertions: list[int] = asserts |> [], (acc, assertions) => acc + assertions
                    assertions
                """));
        var generated = generatedProgram.modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated).contains("assertions_j1");
        assertGeneratedJavaCompiles(generatedProgram);
    }

    @Test
    void shouldParenthesizeIfExpressionsInsideStringConcatenation() {
        var generated = new JavaGenerator().generate(compileProgram("StringIfConcat", "/foo/bar", """
                fun label(is_valid: bool): string =
                    "Date should be " + (if is_valid then 'valid' else 'invalid')
                """)).modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated).contains("((\"Date should be \")+(((is_valid) ? (\"valid\") : (\"invalid\"))))");
    }

    @Test
    void shouldNotGenerateUnsupportedHelperWhenUnused() {
        var generated = new JavaGenerator().generate(compileProgram("NoUnsupported", "/foo/bar", """
                fun answer(): int = 42
                """)).modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated).doesNotContain("private static <T> T __capybaraUnsupported(String message)");
    }

    @Test
    void shouldGenerateUnsupportedHelperWhenUsed() {
        var generated = new JavaGenerator().generate(compileProgram("NeedsUnsupported", "/foo/bar", """
                fun not_supported(): int = ???
                """)).modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated).contains("private static <T> T __capybaraUnsupported(String message)");
    }

    @Test
    void shouldNotGenerateUnsupportedHelperWhenNameAppearsInStringLiteral() {
        var generated = new JavaGenerator().generate(compileProgram("LiteralUnsupportedName", "/foo/bar", """
                fun show(): string = "__capybaraUnsupported("
                """)).modules().stream()
                .map(dev.capylang.generator.GeneratedModule::code)
                .collect(joining("\n"));

        assertThat(generated).doesNotContain("private static <T> T __capybaraUnsupported(String message)");
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
                                return (((a_j1>2)) ? ((("I'm happy ")+(java.lang.String.valueOf(a_j2)))) : ((("I'm not happy ")+(java.lang.String.valueOf(a_j3)))));"""
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
                                var x_j1 = (((b!=0)) ? (b) : (1));
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
                                return (((a>0)) ? ((x+1)) : ((x_j1-1)));"""
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
                                fun summon_tom(): Knight = Tom {}
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
                        "return l.stream().map(it -> (double_(((int) it)))).toList();"
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

    private static RawModule reflectionMetadataModule() {
        return new RawModule("Reflection", "/capy/meta_prog", """
                type AnyInfo { name: string, pkg: PackageInfo } =
                    DataInfo
                    | InterfaceInfo
                    | ObjectInfo
                    | TraitInfo
                    | ListInfo
                    | SetInfo
                    | DictInfo
                    | TupleInfo
                    | FunctionTypeInfo

                data DataInfo { name: string, pkg: PackageInfo }

                data InterfaceInfo { methods: list[MethodInfo], parents: set[AnyInfo] }
                data ObjectInfo { open: bool, fields: list[FieldInfo], methods: list[MethodInfo], parents: set[AnyInfo] }
                data TraitInfo { methods: list[MethodInfo], parents: set[AnyInfo] }
                data MethodInfo { name: string, pkg: PackageInfo, params: list[FieldInfo], return_type: AnyInfo }

                data ListInfo { element_type: AnyInfo }
                data SetInfo { element_type: AnyInfo }
                data DictInfo { value_type: AnyInfo }

                data TupleInfo { elements: list[AnyInfo] }
                data FunctionTypeInfo { params: list[AnyInfo], return_type: AnyInfo }

                data PackageInfo { name: string, path: string }
                data FieldInfo { name: string, type: AnyInfo }
                data FieldValueInfo { name: string, type: AnyInfo, value: any }
                data DataValueInfo { name: string, pkg: PackageInfo, fields: list[FieldValueInfo] }

                fun reflection(obj: data): DataValueInfo = <native>
                """);
    }

    private void assertGeneratedJavaCompiles(dev.capylang.generator.GeneratedProgram generatedProgram) {
        var sourceDir = tempDir.resolve("generated");
        var classesDir = tempDir.resolve("classes");

        generatedProgram.modules().forEach(module -> {
            try {
                var path = sourceDir.resolve(module.relativePath());
                Files.createDirectories(path.getParent());
                Files.writeString(path, module.code(), StandardCharsets.UTF_8);
            } catch (Exception e) {
                throw new AssertionError("Unable to write generated source", e);
            }
        });

        var compiler = ToolProvider.getSystemJavaCompiler();
        assertThat(compiler).as("system Java compiler").isNotNull();

        try {
            Files.createDirectories(classesDir);
            var javaFiles = new java.util.ArrayList<Path>();
            try (var files = Files.walk(sourceDir)) {
                javaFiles.addAll(files.filter(Files::isRegularFile)
                        .filter(path -> path.getFileName().toString().endsWith(".java"))
                        .toList());
            }
            var dependencyRoots = List.of(
                    resolveRepositoryPath("lib/capybara-lib/src/main/java"),
                    resolveRepositoryPath("lib/capybara-lib/build/generated/sources/capybara/java/main")
            );
            for (var dependencyRoot : dependencyRoots) {
                if (!Files.exists(dependencyRoot)) {
                    continue;
                }
                try (var files = Files.walk(dependencyRoot)) {
                    javaFiles.addAll(files.filter(Files::isRegularFile)
                            .filter(path -> path.getFileName().toString().endsWith(".java"))
                            .toList());
                }
            }
            var diagnostics = new DiagnosticCollector<JavaFileObject>();
            var output = new StringWriter();
            try (var fileManager = compiler.getStandardFileManager(diagnostics, Locale.ROOT, StandardCharsets.UTF_8)) {
                var compilationUnits = fileManager.getJavaFileObjectsFromFiles(javaFiles.stream().map(Path::toFile).toList());
                var classpathEntries = Stream.concat(
                                Stream.of(System.getProperty("java.class.path")),
                                Stream.of(
                                                resolveRepositoryPath("lib/capybara-lib/build/classes/java/main"),
                                                ensureStandardLibraryClasses()
                                        )
                                        .filter(Objects::nonNull)
                                        .filter(Files::exists)
                                        .map(Path::toString))
                        .collect(joining(System.getProperty("path.separator")));
                var options = List.of(
                        "--release", "21",
                        "-classpath", classpathEntries,
                        "-d", classesDir.toString()
                );
                var success = compiler.getTask(new PrintWriter(output), fileManager, diagnostics, options, null, compilationUnits).call();
                assertThat(success)
                        .as(diagnostics.getDiagnostics().stream().map(Objects::toString).collect(joining(System.lineSeparator())))
                        .isTrue();
            }
        } catch (Exception e) {
            throw new AssertionError("Unable to compile generated Java", e);
        }
    }

    private Path resolveRepositoryPath(String relativePath) {
        var current = Path.of("").toAbsolutePath().normalize();
        while (current != null) {
            var candidate = current.resolve(relativePath).normalize();
            if (Files.exists(candidate)) {
                return candidate;
            }
            current = current.getParent();
        }
        return Paths.get(relativePath).toAbsolutePath().normalize();
    }

    private Path ensureStandardLibraryClasses() {
        synchronized (STANDARD_LIBRARY_LOCK) {
            if (standardLibraryClassesDir != null && Files.exists(standardLibraryClassesDir)) {
                return standardLibraryClassesDir;
            }
            try {
                var workDir = Files.createTempDirectory("capybara-lib-java-test-");
                var generatedSourceDir = workDir.resolve("generated");
                var classesDir = workDir.resolve("classes");
                Files.createDirectories(generatedSourceDir);
                Files.createDirectories(classesDir);

                var sourceRoot = resolveRepositoryPath("lib/capybara-lib/src/main/capybara");
                var rawModules = loadRawModules(sourceRoot);
                var programResult = CapybaraCompiler.INSTANCE.compile(rawModules, new java.util.TreeSet<>());
                if (programResult instanceof Result.Error<CompiledProgram> error) {
                    throw new AssertionError(error.errors().stream()
                            .map(Result.Error.SingleError::toString)
                            .collect(joining(System.lineSeparator())));
                }
                var generatedProgram = new JavaGenerator().generate(((Result.Success<CompiledProgram>) programResult).value());
                generatedProgram.modules().forEach(module -> {
                    try {
                        var path = generatedSourceDir.resolve(module.relativePath());
                        Files.createDirectories(path.getParent());
                        Files.writeString(path, module.code(), StandardCharsets.UTF_8);
                    } catch (Exception e) {
                        throw new AssertionError("Unable to write standard library generated source", e);
                    }
                });

                var compiler = ToolProvider.getSystemJavaCompiler();
                assertThat(compiler).as("system Java compiler").isNotNull();
                var javaFiles = new java.util.ArrayList<Path>();
                try (var files = Files.walk(generatedSourceDir)) {
                    javaFiles.addAll(files.filter(Files::isRegularFile)
                            .filter(path -> path.getFileName().toString().endsWith(".java"))
                            .toList());
                }
                var javaSourceRoot = resolveRepositoryPath("lib/capybara-lib/src/main/java");
                if (Files.exists(javaSourceRoot)) {
                    try (var files = Files.walk(javaSourceRoot)) {
                        javaFiles.addAll(files.filter(Files::isRegularFile)
                                .filter(path -> path.getFileName().toString().endsWith(".java"))
                                .toList());
                    }
                }

                var diagnostics = new DiagnosticCollector<JavaFileObject>();
                try (var fileManager = compiler.getStandardFileManager(diagnostics, Locale.ROOT, StandardCharsets.UTF_8)) {
                    var compilationUnits = fileManager.getJavaFileObjectsFromFiles(javaFiles.stream().map(Path::toFile).toList());
                    var options = List.of(
                            "--release", "21",
                            "-classpath", System.getProperty("java.class.path"),
                            "-d", classesDir.toString()
                    );
                    var success = compiler.getTask(new PrintWriter(new StringWriter()), fileManager, diagnostics, options, null, compilationUnits).call();
                    assertThat(success)
                            .as(diagnostics.getDiagnostics().stream().map(Objects::toString).collect(joining(System.lineSeparator())))
                            .isTrue();
                }

                standardLibraryClassesDir = classesDir;
                return classesDir;
            } catch (Exception e) {
                throw new AssertionError("Unable to prepare Capybara library classes", e);
            }
        }
    }

    private List<RawModule> loadRawModules(Path sourceRoot) throws Exception {
        try (var files = Files.walk(sourceRoot)) {
            return files.filter(Files::isRegularFile)
                    .filter(path -> path.getFileName().toString().endsWith(".cfun"))
                    .sorted()
                    .map(path -> toRawModule(sourceRoot, path))
                    .toList();
        }
    }

    private RawModule toRawModule(Path sourceRoot, Path sourceFile) {
        try {
            var relativePath = sourceRoot.relativize(sourceFile);
            var moduleName = stripExtension(relativePath.getFileName().toString());
            var moduleParent = relativePath.getParent() == null
                    ? "/"
                    : "/" + relativePath.getParent().toString().replace('\\', '/');
            return new RawModule(moduleName, moduleParent, Files.readString(sourceFile, StandardCharsets.UTF_8));
        } catch (Exception e) {
            throw new AssertionError("Unable to load raw module from " + sourceFile, e);
        }
    }

    private String stripExtension(String fileName) {
        var extensionIndex = fileName.lastIndexOf('.');
        return extensionIndex >= 0 ? fileName.substring(0, extensionIndex) : fileName;
    }
}
