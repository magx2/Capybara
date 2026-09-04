package dev.capylang;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import dev.capylang.compiler.OutputType;
import dev.capylang.generator.Generator;
import dev.capylang.generator.JavaGenerator;
import dev.capylang.generator.JavaScriptGenerator;
import dev.capylang.generator.PythonGenerator;
import dev.capylang.generator.internal.GeneratedJavaGenerator;
import dev.capylang.compiler.CapybaraCompiler;
import dev.capylang.compiler.CompiledModule;
import dev.capylang.compiler.CompiledProgram;
import dev.capylang.compiler.LinkedJsonCodec;
import dev.capylang.compiler.NativeProviderManifest;
import dev.capylang.compiler.parser.ParserException;
import dev.capylang.compiler.parser.RawModule;
import dev.capylang.compiler.parser.SourceKind;
import capy.lang.Either;

import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.nio.charset.StandardCharsets;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.assertj.core.api.Assertions.fail;

class CompilationTest {
    @Test
    void shouldRejectExtensionMethodCallWithWrongArity() {
        var result = CapybaraCompiler.compile(
                List.of(rawModule("Game", "paper_soccer", """
                        data Game {}
                        data Point {}
                        data Player {}

                        fun Game.ball_position(game: Game): Point = Point {}
                        fun Player.ball_position(): Point = Point {}

                        fun move(game: Game): Point = game.ball_position()
                        """)),
                new LinkedHashSet<>(),
                emptyNativeProviders(),
                emptyNativeProviders()
        ).unsafeRun();

        assertThat(result).isInstanceOf(Either.Right.class);
        assertThat(((Either.Right<?, ?>) result).value().toString())
                .contains("Method `ball_position` on receiver type `Game` expects 1 argument, but received 0.");
    }

    @Test
    void shouldRejectExtensionMethodCalledOnWrappedReceiverDuringCompilation() {
        var result = CapybaraCompiler.compile(
                extensionMethodReceiverModules("""
                        const K1234: Result[Kaprekar] = Success { Kaprekar { 1234 } }

                        private fun test(name: String, body: () => any): int = 0

                        fun broken(): int = test('Kaprekar.diff()', () => K1234.diff())
                        """),
                new LinkedHashSet<>(),
                emptyNativeProviders(),
                emptyNativeProviders()
        ).unsafeRun();

        assertThat(result).isInstanceOf(Either.Right.class);
        assertThat(((Either.Right<?, ?>) result).value().toString())
                .contains("Method `diff` requires receiver type `Kaprekar`, but the receiver has type "
                        + "`Result[Kaprekar]`; extract a `Kaprekar` value before calling the method.");
    }

    @Test
    void shouldAllowExtensionMethodCalledOnItsDeclaredReceiver() {
        compileProgram(extensionMethodReceiverModules("""
                fun valid(value: Kaprekar): Kaprekar = value.diff()
                """));
    }

    @Test
    void shouldGenerateMappedExtensionCallOnWrappedConstantForEveryBackend() {
        var program = compileProgram(extensionMethodReceiverModules("""
                const K1234: Result[Kaprekar] = Success { Kaprekar { 1234 } }

                fun mapped(): Result[Kaprekar] = K1234.map(k => k.diff())
                """));

        var javaCode = String.join("\n", JavaGenerator.javaGenerator(program).modules().stream()
                .map(module -> module.code())
                .toList());
        assertThat(javaCode)
                .contains("Kaprekar_diff__")
                .doesNotContain("k.diff(");

        var javaScriptCode = String.join("\n", JavaScriptGenerator.javaScriptGenerator(program).modules().stream()
                .map(module -> module.code())
                .toList());
        assertThat(javaScriptCode)
                .contains("__capy_result_map(K1234")
                .contains("Kaprekar_diff__")
                .doesNotContain("k.diff(");

        var pythonCode = String.join("\n", PythonGenerator.pythonGenerator(program).modules().stream()
                .map(module -> module.code())
                .toList());
        assertThat(pythonCode)
                .contains("__capy_result_map(K1234")
                .contains("Kaprekar_diff__")
                .doesNotContain("k.diff(");
    }

    @Test
    void shouldRejectUndefinedMethodOnMappedResultDuringCompilation() {
        var result = CapybaraCompiler.compile(
                extensionMethodReceiverModules("""
                        const K1234: Result[Kaprekar] = Success { Kaprekar { 1234 } }

                        fun broken(): bool = K1234.map(k => k.to_string()).contains("1234")
                        """),
                new LinkedHashSet<>(),
                emptyNativeProviders(),
                emptyNativeProviders()
        ).unsafeRun();

        assertThat(result).isInstanceOf(Either.Right.class);
        assertThat(((Either.Right<?, ?>) result).value().toString())
                .contains("Method `contains` is not defined for receiver type `Result[String]`.");
    }

    @Test
    void shouldKeepPrimitiveConstructionWithoutValidatorUnwrapped() {
        compileProgram(List.of(rawModule("TimeUnit", "", """
                type second -> long

                fun second.same(): second = this

                fun duration(): second = second { 1L }.same()
                """)));
    }

    private static List<RawModule> extensionMethodReceiverModules(String consumerSource) {
        return List.of(
                rawModule("Kaprekar", "", """
                        data Kaprekar { value: int }

                        fun Kaprekar.diff(): Kaprekar = this
                        fun Kaprekar.to_string(): String = "1234"
                        """),
                rawModule("Kaprekar.test", "", """
                        from /Kaprekar import { Kaprekar }

                        data Error { kind: String, message: String }
                        data Success[T] { value: T }
                        union Result[T] = Success[T] | Error

                        %s
                        """.formatted(consumerSource))
        );
    }

    @Test
    void shouldRejectPrivateTypesInPublicFunctionSignatures() {
        var result = CapybaraCompiler.compile(
                List.of(rawModule("Visibility", "", """
                        private data Hidden { value: int }

                        fun accepts_hidden(values: List[Hidden]): int = 0

                        fun returns_hidden(): Result[Hidden] = Success { Hidden { 1 } }

                        fun infers_hidden() = Hidden { 2 }
                        """)),
                new LinkedHashSet<>(),
                emptyNativeProviders(),
                emptyNativeProviders()
        ).unsafeRun();

        assertThat(result).isInstanceOf(Either.Right.class);
        assertThat(((Either.Right<?, ?>) result).value().toString())
                .contains("Public function `accepts_hidden` exposes private type `Hidden` in its parameter `values`.")
                .contains("Public function `returns_hidden` exposes private type `Hidden` in its return type.")
                .contains("Public function `infers_hidden` exposes private type `Hidden` in its return type.");
    }

    @Test
    void shouldAllowPrivateTypesInPrivateFunctionSignatures() {
        compileProgram(List.of(rawModule("Visibility", "", """
                private data Hidden { value: int }

                private fun accepts_hidden(values: List[Hidden]): int = 0

                private fun returns_hidden(): Result[Hidden] = Success { Hidden { 1 } }

                private fun infers_hidden() = Hidden { 2 }
                """)));
    }

    @Test
    void shouldRejectFunctionCallAsPipeRightOperandDuringCompilation() {
        var result = CapybaraCompiler.compile(
                List.of(rawModule("main", "", """
                        fun broken(value: Result[int]): bool = value | diff()

                        private fun diff(): int = 1
                        """)),
                new LinkedHashSet<>(),
                emptyNativeProviders(),
                emptyNativeProviders()
        ).unsafeRun();

        assertThat(result).isInstanceOf(Either.Right.class);
        assertThat(((Either.Right<?, ?>) result).value().toString())
                .contains("Operator `|` requires a lambda or function reference on its right-hand side; "
                        + "found a function call.");
    }

    @Test
    void shouldContinueToAllowBooleanOrWithoutCallableRightOperand() {
        compileProgram(List.of(rawModule("main", "", """
                fun either(left: bool, right: bool): bool = left | right
                """)));
    }

    @Test
    void shouldCompileAndGenerateRootModulesWithAbsoluteImports() {
        var program = compileProgram(List.of(
                rawModule("Support", "", """
                        const ANSWER: int = 41
                        fun increment(value: int): int = value + 1
                        """),
                rawModule("main", "", """
                        from /Support import { ANSWER, increment }

                        fun answer(): int = increment(ANSWER)
                        """)
        ));

        var javaModules = JavaGenerator.javaGenerator(program).modules();
        assertThat(javaModules)
                .extracting(module -> module.relativePath())
                .contains("Support.java", "main.java");
        assertThat(javaModules.stream()
                .filter(module -> module.relativePath().equals("main.java"))
                .findFirst()
                .orElseThrow()
                .code())
                .doesNotContain("package ;")
                .contains("Support.increment")
                .contains("Support.ANSWER");

        var javaScriptModules = JavaScriptGenerator.javaScriptGenerator(program).modules();
        assertThat(javaScriptModules)
                .extracting(module -> module.relativePath())
                .contains("Support.js", "main.js");
        assertThat(javaScriptModules.stream()
                .filter(module -> module.relativePath().equals("main.js"))
                .findFirst()
                .orElseThrow()
                .code())
                .contains("require(\"./Support\")");

        var pythonModules = PythonGenerator.pythonGenerator(program).modules();
        assertThat(pythonModules)
                .extracting(module -> module.relativePath())
                .contains("Support.py", "main.py");
        assertThat(pythonModules.stream()
                .filter(module -> module.relativePath().equals("main.py"))
                .findFirst()
                .orElseThrow()
                .code())
                .contains("__import__(\"Support\"");
    }

    @Test
    void shouldSanitizeJavaPackageSegments() {
        var program = compileProgram(List.of(
                rawModule("Field", "/paper-soccer", """
                        fun width(): int = 8
                        """),
                rawModule("Main", "/paper-soccer", """
                        from Field import { width }

                        fun field_width(): int = width()
                        """)
        ));

        var generatedModules = JavaGenerator.javaGenerator(program).modules();
        var field = generatedModules.stream()
                .filter(module -> module.relativePath().equals("paper_soccer/Field.java"))
                .findFirst()
                .orElseThrow();
        var main = generatedModules.stream()
                .filter(module -> module.relativePath().equals("paper_soccer/Main.java"))
                .findFirst()
                .orElseThrow();

        assertThat(field.code())
                .startsWith("package paper_soccer;")
                .doesNotContain("package paper-soccer;");
        assertThat(main.code())
                .contains("import static paper_soccer.Field.width__")
                .contains("width__");
    }

    @Test
    void shouldGenerateTopLevelInterfaceParentsFromLegacyModuleOnlyPrograms() {
        var program = compileProgram(List.of(rawModule("UIContract", "/paper-soccer/ui", """
                interface UI {
                    def draw_field(game_field: String): String
                }

                class ConsoleUI: UI {
                    override def draw_field(game_field: String): String = game_field
                }
                """, SourceKind.OBJECT_ORIENTED)));
        var module = program.modules().getFirst();
        assertThat(module.functions().stream()
                .filter(function -> function.name().equals("__capy_oo_parent|ConsoleUI|0")))
                .hasSize(1);
        var legacyFunctions = module.functions().stream()
                .map(function -> function.name().equals("__capy_oo_parent|ConsoleUI|0")
                        ? function.with(
                                function.name(),
                                function.visibility(),
                                function.documentation(),
                                function.parameters(),
                                function.returnType(),
                                new dev.capylang.compiler.CompiledExpression.CompiledStringLiteral(
                                        "UI",
                                        "\"UI\"",
                                        function.location()
                                ),
                                function.location()
                        )
                        : function)
                .toList();
        var legacyModule = module.with(
                module.name(),
                module.path(),
                module.types(),
                module.visiblePrimitiveBackedTypes(),
                legacyFunctions,
                module.imports(),
                module.derivers(),
                module.annotations(),
                module.staticImports()
        );
        var legacyProgram = new CompiledProgram(
                List.of(legacyModule),
                List.of(),
                program.nativeProviders(),
                program.nativeProviderCatalog()
        );

        var generated = JavaGenerator.javaGenerator(legacyProgram);
        var contract = generated.modules().stream()
                .filter(generatedModule -> generatedModule.relativePath().equals("paper_soccer/ui/UIContract.java"))
                .findFirst()
                .orElseThrow();

        assertThat(generated.modules())
                .extracting(generatedModule -> generatedModule.relativePath())
                .contains("paper_soccer/ui/UI.java");
        assertThat(contract.code())
                .contains("class ConsoleUI implements paper_soccer.ui.UI")
                .doesNotContain("UIContract.UI");
    }

    @Test
    void shouldSanitizeLinkedLibraryStaticImportOwners() {
        var libraries = compileProgram(List.of(rawModule("Field", "/paper-soccer", """
                fun width(): int = 8
                """)));
        var compilation = CapybaraCompiler.compile(
                List.of(rawModule("Main", "/paper-soccer", """
                        from Field import { width }

                        fun field_width(): int = width()
                        """)),
                new LinkedHashSet<>(libraries.modules()),
                emptyNativeProviders(),
                emptyNativeProviders()
        ).unsafeRun();
        assertThat(compilation).isInstanceOf(Either.Left.class);
        var program = (CompiledProgram) ((Either.Left<?, ?>) compilation).value();
        var main = JavaGenerator.javaGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("paper_soccer/Main.java"))
                .findFirst()
                .orElseThrow();

        assertThat(main.code())
                .contains("import static paper_soccer.Field.width__")
                .doesNotContain("paper-soccer.Field");
    }

    @Test
    void shouldUseBundledModulesForLookupWithoutGeneratingThem() {
        var program = compileProgram(List.of(rawModule("BundledLookup", "/sample/app", """
                from /capy/collection/List import { * }
                from /capy/lang/Option import { * }
                from /capy/lang/Result import { * }
                from /capy/test/Assert import { * }

                fun first_or(values: List[int], fallback: int): int =
                    match values[0] with
                    case Some { value } -> value
                    case None -> fallback

                fun result_or(result: Result[int], fallback: int): int =
                    result.reduce_left(value => value, _ => fallback)

                fun failed(message: String): Result[int] =
                    error(message)

                fun failed_kind(kind: String, message: String): Result[int] =
                    fail_kind(kind, message)

                fun detailed_error(kind: String, message: String, details: data): Error =
                    error_with(kind, message, details)

                fun check(value: String): Assert =
                    assert_that(value).is_equal_to("ok")
                """)));

        var javaModules = JavaGenerator.javaGenerator(program).modules();
        assertThat(javaModules)
                .extracting(module -> module.relativePath())
                .containsExactly("sample/app/BundledLookup.java");
        assertThat(javaModules.getFirst().code())
                .contains("java.util.List<java.lang.Integer> values")
                .contains("java.util.Optional<java.lang.Integer>")
                .contains("java.lang.Object result")
                .contains("__capy_result_is_success")
                .contains("__capy_error(kind, message)")
                .contains("__capy_error_with(kind, message, details)")
                .contains("__capy_assert_equal(");

        var pythonModules = PythonGenerator.pythonGenerator(program).modules();
        assertThat(pythonModules)
                .extracting(module -> module.relativePath())
                .contains("sample/app/BundledLookup.py", "capy/test/Assert.py", "capy/test/CapyTestRuntime.py", "dev/capylang/capybara.py")
                .doesNotContain("capy/collection/List.py", "capy/lang/Option.py", "capy/lang/Result.py");
        assertThat(pythonModules.stream()
                .filter(module -> module.relativePath().equals("sample/app/BundledLookup.py"))
                .findFirst()
                .orElseThrow()
                .code())
                .contains("__capy_index(values, 0)")
                .contains("__capy_result_reduce(result")
                .contains("__capy_error(message)")
                .contains("__capy_error_kind(kind, message)")
                .contains("__capy_error_with(kind, message, details)")
                .contains("__import__(\"capy.test.Assert\"");

        var javaScriptModules = JavaScriptGenerator.javaScriptGenerator(program).modules();
        assertThat(javaScriptModules)
                .extracting(module -> module.relativePath())
                .contains("sample/app/BundledLookup.js", "capy/test/Assert.js", "capy/test/CapyTestRuntime.js")
                .doesNotContain("capy/collection/List.js", "capy/lang/Option.js", "capy/lang/Result.js");
        assertThat(javaScriptModules.stream()
                .filter(module -> module.relativePath().equals("sample/app/BundledLookup.js"))
                .findFirst()
                .orElseThrow()
                .code())
                .contains("__capy_index(values, 0, \"List\")")
                .contains("__capy_result_reduce(result")
                .contains("__capy_error_kind(\"capy.error\", message)")
                .contains("__capy_error_kind(kind, message)")
                .contains("__capy_error_with(kind, message, details)")
                .contains("capy/test/Assert");
    }

    @Test
    void shouldSanitizeDottedModuleNamesForJavaClasses() {
        var program = compileProgram(List.of(rawModule(
                "Kaprekar.test",
                "",
                "fun tests(): int = 1"
        )));

        var generated = JavaGenerator.javaGenerator(program);
        assertThat(generated.modules())
                .extracting(module -> module.relativePath())
                .contains("Kaprekar_test.java");
        assertThat(generated.modules().stream()
                .filter(module -> module.relativePath().equals("Kaprekar_test.java"))
                .findFirst()
                .orElseThrow()
                .code())
                .contains("public final class Kaprekar_test")
                .contains("private Kaprekar_test()");
        assertThat(generated.modules().stream()
                .filter(module -> module.relativePath().equals("capy/test/CapyTestRuntime.java"))
                .findFirst()
                .orElseThrow()
                .code())
                .contains("invokeRootTests(\"Kaprekar_test\")")
                .doesNotContain("Kaprekar_test.tests()");
    }

    @Test
    void shouldSanitizeJavaPackageDirectories() {
        var program = compileProgram(List.of(
                rawModule("Main", "/paper-soccer/shared-code", "fun result(): int = 42")
        ));

        var generated = JavaGenerator.javaGenerator(program);
        assertThat(generated.modules())
                .extracting(module -> module.relativePath())
                .contains("paper_soccer/shared_code/Main.java");
        assertThat(generated.modules().stream()
                .filter(module -> module.relativePath().equals("paper_soccer/shared_code/Main.java"))
                .findFirst()
                .orElseThrow()
                .code())
                .contains("package paper_soccer.shared_code;")
                .doesNotContain("paper-soccer", "shared-code");
    }

    @Test
    void shouldSanitizeCrossModuleFunctionBindingOwners() throws ReflectiveOperationException {
        var ownerMethod = Arrays.stream(GeneratedJavaGenerator.class.getDeclaredMethods())
                .filter(method -> method.getName().startsWith("java_function_binding_class_name__"))
                .findFirst()
                .orElseThrow();
        ownerMethod.setAccessible(true);

        assertThat(ownerMethod.invoke(null, Map.of(
                "modulePath", "paper-soccer/shared-code",
                "moduleName", "Support"
        ))).isEqualTo("paper_soccer.shared_code.Support");
    }

    @Test
    void shouldRejectAbsoluteImportWithoutRootModuleName() {
        assertThatThrownBy(() -> CapybaraCompiler.compile(
                List.of(rawModule("main", "", """
                        from / import { increment }

                        fun answer(): int = increment(41)
                        """)),
                new LinkedHashSet<>(),
                emptyNativeProviders(),
                emptyNativeProviders()
        ).unsafeRun())
                .isInstanceOf(ParserException.class)
                .hasMessageContaining("main.cfun:1:0: ParserError");
    }

    @Test
    void shouldRejectUnknownExplicitStandardLibraryImports() throws Exception {
        var libraries = compileProgram(List.of(rawModule("Primitives", "/capy/lang", """
                const ZERO_DIGIT: int = 0
                const CURRENT_BUILD_ONLY: int = 1
                """)));
        var result = CapybaraCompiler.compile(
                List.of(rawModule("main", "", """
                        from /capy/lang/Primitives import { ZERO_DIGIT, CURRENT_BUILD_ONLY, ONE }

                        fun total(): int = ZERO_DIGIT + CURRENT_BUILD_ONLY
                        """)),
                new LinkedHashSet<>(libraries.modules()),
                emptyNativeProviders(),
                emptyNativeProviders()
        ).unsafeRun();

        assertThat(result).isInstanceOf(Either.Right.class);
        assertThat(((Either.Right<?, ?>) result).value().toString())
                .contains("Module `/capy/lang/Primitives` does not export `ONE`.")
                .doesNotContain("does not export `ZERO_DIGIT`")
                .doesNotContain("does not export `CURRENT_BUILD_ONLY`");

        var resource = CompilationTest.class.getResourceAsStream("/capy/lang/Primitives.json");
        if (resource == null) {
            fail("Missing bundled /capy/lang/Primitives.json");
        }
        var bundledPrimitives = LinkedJsonCodec.read(
                new String(resource.readAllBytes(), StandardCharsets.UTF_8),
                CompiledModule.class
        );
        var bundledResult = CapybaraCompiler.compile(
                List.of(rawModule("bundled", "", """
                        from /capy/lang/Primitives import { digit }

                        fun identity(value: digit): digit = value
                        """)),
                new LinkedHashSet<>(List.of(bundledPrimitives)),
                emptyNativeProviders(),
                emptyNativeProviders()
        ).unsafeRun();

        assertThat(bundledResult).isInstanceOf(Either.Left.class);
    }

    @Test
    void shouldValidateWildcardExclusionsAgainstCurrentStandardLibrarySources() {
        var libraries = compileProgram(List.of(rawModule("CurrentAssert", "/capy/test", """
                fun fail(message: String): String = message
                fun pass(message: String): String = message
                """)));

        var result = CapybaraCompiler.compile(
                List.of(rawModule("main", "", """
                        from /capy/test/CurrentAssert import { * } except { fail }

                        fun success(): String = pass("ok")
                        """)),
                new LinkedHashSet<>(libraries.modules()),
                emptyNativeProviders(),
                emptyNativeProviders()
        ).unsafeRun();

        assertThat(result).isInstanceOf(Either.Left.class);
    }

    @Test
    void shouldGenerateImportedPrimitiveConstantsForEveryBackend() throws Exception {
        var resource = CompilationTest.class.getResourceAsStream("/capy/lang/Primitives.json");
        if (resource == null) {
            fail("Missing bundled /capy/lang/Primitives.json");
        }
        var primitives = LinkedJsonCodec.read(
                new String(resource.readAllBytes(), StandardCharsets.UTF_8),
                CompiledModule.class
        );
        assertThat(primitives.visiblePrimitiveBackedTypes()).containsKey("digit");
        var compilation = CapybaraCompiler.compile(
                List.of(rawModule("main", "", """
                from /capy/lang/Primitives import { digit, ONE_DIGIT, NINE_DIGIT, MAX_INT_VALUE, MAX_LONG_VALUE }

                fun digits(): List[digit] = [ONE_DIGIT, NINE_DIGIT]
                fun render(value: digit): String = value.to_string()
                fun max_int(): int = MAX_INT_VALUE
                fun max_long(): long = MAX_LONG_VALUE
                """)),
                new LinkedHashSet<>(List.of(primitives)),
                emptyNativeProviders(),
                emptyNativeProviders()
        ).unsafeRun();
        if (compilation instanceof Either.Right<?, ?> error) {
            fail(error.value().toString());
        }
        var program = (CompiledProgram) ((Either.Left<?, ?>) compilation).value();

        var javaCode = JavaGenerator.javaGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("main.java"))
                .findFirst()
                .orElseThrow()
                .code();
        assertThat(javaCode)
                .contains("return java.util.List.of(1, 9);")
                .contains("java.lang.String.valueOf(value)")
                .contains("return 2147483647;")
                .contains("return 9223372036854775807L;")
                .doesNotContain("java.util.List.of(ONE_DIGIT, NINE_DIGIT)");

        var javaScriptCode = JavaScriptGenerator.javaScriptGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("main.js"))
                .findFirst()
                .orElseThrow()
                .code();
        assertThat(javaScriptCode)
                .contains("return [1, 9];")
                .contains("return __capy_to_string(value);")
                .contains("return 2147483647;")
                .contains("return 9223372036854775807n;")
                .doesNotContain("__capy_import_capy_lang_Primitives")
                .doesNotContain("ONE_DIGIT__")
                .doesNotContain("NINE_DIGIT__")
                .doesNotContain("value.to_string()");

        var pythonCode = PythonGenerator.pythonGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("main.py"))
                .findFirst()
                .orElseThrow()
                .code();
        assertThat(pythonCode)
                .contains("return [1, 9]")
                .contains("return __capy_to_string(value)")
                .contains("return 2147483647")
                .contains("return 9223372036854775807")
                .doesNotContain("__import__(\"capy.lang.Primitives\"")
                .doesNotContain(".ONE_DIGIT")
                .doesNotContain(".NINE_DIGIT")
                .doesNotContain("value.to_string()");
    }

    @Test
    void shouldRejectNestedGenericMismatchInEffectBinding() {
        var asyncSource = """
                data Async[T] { <native> }

                fun all(tasks: Seq[Async[T]]): Effect[List[Result[T]]] = <native>
                """;
        var mainSource = """
                from /capy/lang/Async import { Async, all }

                fun broken(tasks: Seq[Async[String]]): Effect[List[String]] =
                    let strings: List[String] <- all(tasks)
                    Effect.pure(strings)
                """;

        var result = CapybaraCompiler.compile(
                List.of(
                        rawModule("Async", "/capy/lang", asyncSource),
                        rawModule("main", "", mainSource)
                ),
                new LinkedHashSet<>(),
                emptyNativeProviders(),
                emptyNativeProviders()
        ).unsafeRun();

        assertThat(result).isInstanceOf(Either.Right.class);
        var errors = (List<?>) ((Either.Right<?, ?>) result).value();
        assertThat(errors.toString())
                .contains("Binding `strings` has type `List[Result[T]]`, but declares `List[String]`.");
    }

    @Test
    void shouldRejectNonCallableDataFieldDuringCompilation() {
        var result = CapybaraCompiler.compile(
                List.of(rawModule("Main", "", """
                        union Seq[T] = Cons | End
                        data Cons[T] { value: T, rest: () => Seq[T] }
                        data End {}

                        fun broken(value: int): Seq[int] =
                            Cons { value: value, rest: End {} }
                        """)),
                new LinkedHashSet<>(),
                emptyNativeProviders(),
                emptyNativeProviders()
        ).unsafeRun();

        assertThat(result).isInstanceOf(Either.Right.class);
        var errors = (List<?>) ((Either.Right<?, ?>) result).value();
        assertThat(errors.toString())
                .contains("Field `rest` of data `Cons` requires callable type `()=>Seq[T]`.");
    }

    @Test
    void shouldAcceptCallableDataFieldDuringCompilation() {
        compileProgram(List.of(rawModule("Main", "", """
                union Seq[T] = Cons | End
                data Cons[T] { value: T, rest: () => Seq[T] }
                data End {}

                fun valid(value: int): Seq[int] =
                    Cons { value: value, rest: () => End {} }
                """)));
    }

    @Test
    void shouldRejectBroaderLocalFunctionReturnTypeDuringCompilation() {
        var result = CapybaraCompiler.compile(
                List.of(rawModule("Main", "", """
                        union Seq[T] = Cons[T] | End
                        data Cons[T] { value: T, rest: () => Seq[T] }
                        data End {}
                        data Kaprekar {}

                        fun find_kaprekar_sequence(start: Kaprekar): Cons[Kaprekar] =
                            fun find_kaprekar_sequence_full(next: Kaprekar): Seq[Kaprekar] =
                                Cons { value: next, rest: () => End {} }
                            ---
                            find_kaprekar_sequence_full(start)
                        """)),
                new LinkedHashSet<>(),
                emptyNativeProviders(),
                emptyNativeProviders()
        ).unsafeRun();

        assertThat(result).isInstanceOf(Either.Right.class);
        assertThat(((Either.Right<?, ?>) result).value().toString())
                .contains("Function `find_kaprekar_sequence` returns `Seq[Kaprekar]`, "
                        + "but declares `Cons[Kaprekar]`.");
    }

    @Test
    void shouldAcceptNarrowerLocalFunctionReturnTypeForUnionDuringCompilation() {
        compileProgram(List.of(rawModule("Main", "", """
                union Seq[T] = Cons[T] | End
                data Cons[T] { value: T, rest: () => Seq[T] }
                data End {}

                fun valid(start: int): Seq[int] =
                    fun singleton(next: int): Cons[int] =
                        Cons { value: next, rest: () => End {} }
                    ---
                    singleton(start)
                """)));
    }

    @ParameterizedTest(name = "{index}: should {0}")
    @MethodSource
    void test(String code) {
        var rawModules = List.of(rawModule("Main", "/capybara", code));
        System.out.println(" === PARSING === ");
        System.out.println(rawModules);

        var link = CapybaraCompiler.compile(rawModules, new LinkedHashSet<>(), emptyNativeProviders(), emptyNativeProviders()).unsafeRun();
        if (link instanceof Either.Right<?, ?> error) {
            var errors = (List<?>) error.value();
            throw new RuntimeException("Linking failed with " + errors.size() + " error(s): " + errors);
        }
        System.out.println("\n === LINKING === ");
        System.out.println(link);

        System.out.println("\n === GENERATION === ");
        Arrays.stream(OutputType.values())
                .parallel()
                .map(type -> {
                    var linkedProgram = (CompiledProgram) ((Either.Left<?, ?>) link).value();
                    var compiled = Generator.generate(linkedProgram, generatorOutputType(type));
                    return "\t === " + type + " === \n" + compiled;
                })
                .forEach(System.out::println);

    }

    @Test
    void shouldGenerateObjectOrientedCatchBranchesByErrorKind() {
        var resultSource = """
                data Error { kind: String, message: String }
                fun error_kind(kind: String, message: String): Error = Error { kind: kind, message: message }
                """;
        var objectSource = """
                from /capy/lang/Result import { * }

                class CatchByKind {
                    def recover(): String {
                        try {
                            throw error_kind("capy.test.alpha", "alpha")
                        } catch "capy.test.alpha" error {
                            return "alpha"
                        } catch "capy.test.beta" error {
                            return "beta"
                        } catch error {
                            return "fallback"
                        }
                    }
                }
                """;
        var program = compileProgram(List.of(
                rawModule("Result", "/capy/lang", resultSource, SourceKind.FUNCTIONAL),
                rawModule("CatchByKind", "/sample/app", objectSource, SourceKind.OBJECT_ORIENTED)
        ));

        var generated = JavaGenerator.javaGenerator(program);
        var code = generated.modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/CatchByKind.java"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(code).contains("__capy_error_kind_equals");
        var alphaBranch = code.indexOf("\"capy.test.alpha\"");
        var betaBranch = code.indexOf("\"capy.test.beta\"");
        var fallbackBranch = code.indexOf("\"fallback\"");
        assertThat(alphaBranch).isGreaterThanOrEqualTo(0);
        assertThat(betaBranch).isGreaterThan(alphaBranch);
        assertThat(fallbackBranch).isGreaterThan(betaBranch);
        assertThat(code).contains("__capy_thrown_error");

        var javaScriptCode = JavaScriptGenerator.javaScriptGenerator(program).modules().stream()
                .map(module -> module.code())
                .reduce("", String::concat);
        assertThat(javaScriptCode)
                .contains("__capy_throw")
                .contains("__capy_try")
                .contains("__capy_enrich_thrown_error")
                .contains("function __capy_error(message) { return __capy_error_kind('capy.error', message); }")
                .contains("backend: 'javascript'");

        var pythonCode = PythonGenerator.pythonGenerator(program).modules().stream()
                .map(module -> module.code())
                .reduce("", String::concat);
        assertThat(pythonCode)
                .contains("__capy_throw")
                .contains("__capy_try")
                .contains("__capy_enrich_thrown_error")
                .contains("def __capy_error(message):\n    return __capy_error_kind('capy.error', message)")
                .contains("backend='python'");
    }

    @Test
    void shouldGenerateResultReducerErrorCallbacksWithStructuredError() {
        var resultSource = """
                data Error { kind: String, message: String }
                data Success[T] { value: T }
                union Result[T] = Success[T] | Error
                fun fail_kind(kind: String, message: String): Result[String] = Error { kind: kind, message: message }
                """;
        var consumerSource = """
                from /sample/lib/Result import { * }

                fun reducer_error_kind(): String =
                    fail_kind("capy.test.result.boom", "boom").reduce(_ => "success", error => error.kind)
                """;
        var program = compileProgram(List.of(
                rawModule("Result", "/sample/lib", resultSource, SourceKind.FUNCTIONAL),
                rawModule("UseResult", "/sample/app", consumerSource, SourceKind.FUNCTIONAL)
        ));

        var code = JavaGenerator.javaGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/UseResult.java"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(code).contains("__capy_result_error_value");
        assertThat(code).contains("__capy_data_field(error, \"kind\")");
        assertThat(code).doesNotContain("java.lang.String error = java.lang.String.valueOf(__capy_result_error_value");
    }

    @Test
    void shouldGenerateListFoldFollowedByOptionFallback() {
        var source = """
                data Some[T] { value: T }
                data None {}
                union Option[T] = Some[T] | None

                fun render(values: List[String]): String =
                    values
                        .fold((acc, value) => acc + "," + value)
                        .or_else("empty")
                """;
        var program = compileProgram(List.of(rawModule("Fold", "/sample/app", source, SourceKind.FUNCTIONAL)));

        var code = JavaGenerator.javaGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/Fold.java"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(code)
                .contains(".stream().reduce((acc, value) ->")
                .contains(".orElse(\"empty\")")
                .doesNotContain("throw new UnsupportedOperationException(\"Unsupported CFUN expression at");

        var pythonCode = PythonGenerator.pythonGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/Fold.py"))
                .findFirst()
                .orElseThrow()
                .code();
        assertThat(pythonCode)
                .contains("__capy_option_or_else(__capy_fold(values, lambda acc, value:")
                .doesNotContain(".fold(");

        var javaScriptCode = JavaScriptGenerator.javaScriptGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/Fold.js"))
                .findFirst()
                .orElseThrow()
                .code();
        assertThat(javaScriptCode)
                .contains("__capy_fold(values, (acc, value) =>")
                .doesNotContain(".fold(");
    }

    @Test
    void shouldPreserveWholeOptionMatchBindingType() {
        var source = """
                data Some[T] { value: T }
                data None {}
                union Option[T] = Some[T] | None

                fun keep(value: Option[String]): Option[String] =
                    match value with
                    case None -> None {}
                    case Some some -> some
                """;
        var program = compileProgram(List.of(rawModule("OptionMatch", "/sample/app", source, SourceKind.FUNCTIONAL)));

        var code = JavaGenerator.javaGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/OptionMatch.java"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(code)
                .contains("java.util.Optional<java.lang.String> some__match_")
                .contains("return some__match_")
                .doesNotContain("java.lang.Object some__match_");
    }

    @Test
    void shouldLowerStringCharacterParsingAndSequencePatternsAcrossBackends() {
        var source = """
                import /capy/collection/Seq
                import /capy/lang/String
                from /capy/lang/Primitives import { to_int }
                from /capy/lang/Result import { Result, Success }

                fun parse(value: String): Seq[Result[int]] =
                    value | char => char.to_int()

                fun second(values: Seq[int]): int =
                    match values with
                    case Cons first ->
                        match first.rest() with
                        case Cons second -> second.value
                        case End -> 0
                    case End -> 0

                fun collect(values: Seq[int]): Result[Seq[int]] =
                    values
                    |> Success { Seq.to_seq([]) }, (acc, value) => acc.map(items => items + value)

                fun from_list(values: List[int]): Seq[int] = Seq.to_seq(values)

                fun list_value(value: int): List[int] = [value]

                fun map_list(values: Seq[Result[int]]): Seq[Result[Seq[int]]] =
                    values | result => result.map(value => Seq.to_seq(list_value(value)))

                data Item { label: String }

                fun Item.render(): String = this.label

                fun render_first(values: Seq[Item]): String =
                    match values with
                    case Cons cons -> cons.value.render()
                    case End -> ""
                """;
        var program = compileProgram(List.of(rawModule("SequencePatterns", "/sample/app", source, SourceKind.FUNCTIONAL)));
        var javaCode = JavaGenerator.javaGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/SequencePatterns.java"))
                .findFirst()
                .orElseThrow()
                .code();
        var pythonCode = PythonGenerator.pythonGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/SequencePatterns.py"))
                .findFirst()
                .orElseThrow()
                .code();
        var javaScriptCode = JavaScriptGenerator.javaScriptGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/SequencePatterns.js"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(javaCode)
                .contains("__capy_parse_int(_char)")
                .contains("__capy_seq_rest(")
                .contains("private static <T> capy.collection.Seq<T> __capy_seq_rest")
                .contains("capy.collection.Seq.toSeq(java.util.List.of())")
                .contains("java.lang.Object items")
                .contains("__capy_seq_append(((capy.collection.Seq<java.lang.Object>) items), value)")
                .contains("private static <T> capy.collection.Seq<T> __capy_seq_append")
                .contains("capy.collection.Seq.toSeq(values)")
                .contains("capy.collection.Seq.toSeq(list_value__")
                .contains(".first().orElse(null)");
        assertThat(pythonCode)
                .contains("__capy_parse_int(char)")
                .contains("__capy_seq_rest(first)")
                .contains("__capy_seq_first_value(second)")
                .contains("Item_render__")
                .doesNotContain("__capy_seq_first_value(cons).render()");
        assertThat(javaScriptCode)
                .contains("__capy_parse_int(char)")
                .contains("__capy_seq_rest(first)")
                .contains("__capy_seq_first_value(second)")
                .contains("Item_render__")
                .doesNotContain("__capy_seq_first_value(cons).render()");
    }

    @Test
    void shouldResolvePythonOverloadUsingGenericMatchPayloadType() {
        var source = """
                from /capy/collection/Seq import { Cons }
                from /capy/lang/Result import { Result, Success, Error }

                fun render(result: Result[Cons[int]]): String =
                    match result with
                    case Success { value } -> render(value)
                    case Error { kind, message } -> message

                fun render(result: Cons[int]): String = "values"
                """;
        var program = compileProgram(List.of(rawModule("GenericOverload", "/sample/app", source, SourceKind.FUNCTIONAL)));
        var pythonCode = PythonGenerator.pythonGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/GenericOverload.py"))
                .findFirst()
                .orElseThrow()
                .code();
        var javaScriptCode = JavaScriptGenerator.javaScriptGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/GenericOverload.js"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(pythonCode)
                .contains("def render__4_0(result):")
                .contains("lambda value: render__9_0(value)")
                .doesNotContain("lambda value: render__4_0(value)");
        assertThat(javaScriptCode)
                .contains("function render__4_0(result)")
                .contains("((value) => render__9_0(value))")
                .doesNotContain("((value) => render(value))");
    }

    @Test
    void shouldUseStaticImportWhenLambdaParameterShadowsPackageName() {
        var modelSource = """
                data Widget {}
                fun Widget.render(): String = "widget"
                """;
        var appSource = """
                from /sample/model/Widget import { Widget }

                fun render_all(values: Seq[Widget]): Seq[String] =
                    values.map(sample => sample.render())
                """;
        var program = compileProgram(List.of(
                rawModule("Widget", "/sample/model", modelSource, SourceKind.FUNCTIONAL),
                rawModule("App", "/sample/app", appSource, SourceKind.FUNCTIONAL)
        ));

        var code = JavaGenerator.javaGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/App.java"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(code)
                .contains("(sample) -> Widget_render__2_0(sample)")
                .doesNotContain("sample.model.Widget.Widget_render__2_0(sample)");
    }

    @Test
    void shouldUseDeclaredFunctionalLambdaParameterTypes() {
        var source = """
                fun typed_map(values: List[String]): Seq[String] =
                    values | value: String => value + "!"

                fun typed_pair(values: Dict[int]): Dict[String] =
                    values | (key: String, value: int) => key + value

                fun entries(values: Dict[int]): List[Tuple[String, int]] =
                    values.entries()
                """;
        var program = compileProgram(List.of(rawModule("TypedLambda", "/sample/app", source, SourceKind.FUNCTIONAL)));

        var code = JavaGenerator.javaGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/TypedLambda.java"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(code).doesNotContain("throw new UnsupportedOperationException(\"Unsupported CFUN expression at");
        assertThat(code).contains("value + \"!\"");
        assertThat(code).contains("java.lang.String key");
        assertThat(code).contains("int value");
        assertThat(code).contains(".entrySet().stream().map(__capy_entry ->");

        var pythonCode = PythonGenerator.pythonGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/TypedLambda.py"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(pythonCode)
                .contains("return list(values.items())")
                .doesNotContain("values.entries()");
    }

    @Test
    void shouldResolveLocalFunctionsInsidePythonExtensionMethodsBeforeQualifiedCalls() {
        var source = """
                data Box { value: int }

                fun Box.count_down(n: int): int =
                    fun loop(current: int): int =
                        if current <= 0 then this.value else loop(current - 1)
                    ---
                    loop(n)

                fun Box.total(list: List[int]): int = this.value + list.size()
                """;
        var program = compileProgram(List.of(rawModule("LocalExtension", "/sample/app", source, SourceKind.FUNCTIONAL)));

        var code = PythonGenerator.pythonGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/LocalExtension.py"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(code)
                .contains("def Box_count_down__local__loop__4_4(")
                .contains("Box_count_down__local__loop__4_4(__capy_sub(current, 1))")
                .contains("return Box_count_down__local__loop__4_4(n)")
                .contains("def Box_total__9_0(this, _capy_list):")
                .contains("__capy_size(_capy_list)")
                .doesNotContain("Box_count_down__local__loop__4_4__4_4");
    }

    @Test
    void shouldGenerateStandardLibraryExtensionCallsForPython() {
        var program = compileProgram(List.of(
                rawModule("TimeUnit", "/capy/lang", """
                        from /capy/lang/Option import { * }
                        from /capy/lang/Result import { * }

                        type nano_second -> long
                        type normalized -> String with constructor { Success { value.to_upper_case() } }
                        const NORMALIZED: normalized = normalized! { "d" }

                        data Time { hour: int }
                        data ValueAssert { value: any }
                        data TimeAssert { value: Time }
                        data ResultAssert[T] { value: Result[T] }
                        data OptionAssert[T] { value: Option[T] }
                        data Sized {}
                        enum Ordering { EQUAL }

                        fun nano_second.to_nano_seconds(): nano_second = this
                        fun assert_that(value: any): ValueAssert = ValueAssert { value }
                        fun assert_that(value: Time): TimeAssert = TimeAssert { value }
                        fun assert_that(value: Result[T]): ResultAssert[T] = ResultAssert { value }
                        fun assert_that(value: Option[T]): OptionAssert[T] = OptionAssert { value }
                        fun TimeAssert.has_hour(hour: int): TimeAssert = this
                        fun OptionAssert[T].contains(other: T): OptionAssert[T] = this
                        fun ValueAssert.is_equal_to(other: any): ValueAssert = this
                        fun Sized.size(): Time = Time { 1 }
                        fun load(): Result[Time] = Success { Time { 1 } }
                        fun optional(): Option[String] = Some { "value" }
                        fun preferred(): Result[Time] = load().or({ Error { "fallback" } })
                        fun fallback_chain(): Result[Time] = Error { "first" }.or({
                            let loaded <- load()
                            Success { loaded }
                        })
                        fun selected(value: Option[Time]): Result[Time] =
                            let fallback <- load()
                            match value with
                            case Some { selected } -> selected
                            case None -> Error { "missing" }
                        """),
                rawModule("TimeUnitTest", "/sample/app", """
                        from /capy/lang/TimeUnit import { * }

                        fun convert(): nano_second = nano_second { 2L }.to_nano_seconds()
                        fun assert_time(value: Time): TimeAssert = assert_that(value).has_hour(1)
                        fun assert_time_result(): ResultAssert[Time] = assert_that(load()).succeeds(time => assert_that(time).has_hour(1))
                        fun assert_option(): OptionAssert[String] = assert_that(optional()).contains("value")
                        fun assert_enum(value: Ordering): ValueAssert = assert_that(value).is_equal_to(value)
                        fun equal(): Ordering = Ordering.EQUAL
                        fun sized(value: Sized): Time = value.size()
                        fun upper(value: String): String = value.to_upper_case()
                        fun compare_strings(left: String, right: String): Ordering = left.compare(right)
                        fun unsafe_normalized(): normalized = normalized! { "d" }
                        fun windows_path(): String = "D:\\\\repos"
                        fun json_literal(): String = "{\\\"foo\\\":\\\"boo\\\"}"
                        fun unicode_null(): String = '\\u0000'
                        fun nested(left: Option[String], right: Option[String]): int =
                            match left with
                            case None ->
                                match right with
                                case None -> 0
                                case Some -> 1
                            case Some ->
                                match right with
                                case None -> 2
                                case Some -> 3
                        fun classify(value: Option[String]): int =
                            match value with
                            case Some { "0" } -> 0
                            case Some -> 1
                            case None -> 2
                        fun retain_ordering(value: Ordering): Ordering =
                            match value with
                            case EQUAL -> EQUAL
                            case ordering -> ordering
                        """)
        ));

        var pythonCode = PythonGenerator.pythonGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/TimeUnitTest.py"))
                .findFirst()
                .orElseThrow()
                .code();
        var pythonLibraryCode = PythonGenerator.pythonGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("capy/lang/TimeUnit.py"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(pythonLibraryCode)
                .contains("__capy_result_from_value(")
                .contains("__capy_result_or(")
                .contains(", lambda: __capy_data(\"Error\"")
                .contains("__capy_block_bind(load__");
        assertThat(pythonLibraryCode.indexOf("def __capy_constructor_normalized__"))
                .isLessThan(pythonLibraryCode.indexOf("NORMALIZED = __capy_bind_value("));

        assertThat(pythonCode)
                .contains("__import__(\"capy.lang.TimeUnit\", fromlist=['*']).nano_second_to_nano_seconds__")
                .contains("__import__(\"capy.lang.TimeUnit\", fromlist=['*']).TimeAssert_has_hour__")
                .contains("__import__(\"capy.lang.TimeUnit\", fromlist=['*']).OptionAssert_T__contains__")
                .contains("__import__(\"capy.lang.TimeUnit\", fromlist=['*']).ValueAssert_is_equal_to__")
                .contains("__import__(\"capy.lang.TimeUnit\", fromlist=['*']).Sized_size__")
                .contains("return __import__(\"capy.lang.TimeUnit\", fromlist=['*']).EQUAL")
                .contains("__capy_deep_equal(__capy_pattern_literal_0, \"0\")")
                .contains("str(value).translate(str.maketrans('abcdefghijklmnopqrstuvwxyz', 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'))")
                .contains("str(left) < str(right)")
                .contains("__capy_bind_value(__import__(\"capy.lang.TimeUnit\", fromlist=['*']).__capy_constructor_normalized__")
                .contains("return \"D:\\\\repos\"")
                .contains("return \"{\\\"foo\\\":\\\"boo\\\"}\"")
                .contains("return '\\u0000'")
                .contains("else (3 if __capy_type_matches(__capy_match_value, \"Some\")")
                .doesNotContain("2.to_nano_seconds()")
                .doesNotContain("StringAssert_is_equal_to__")
                .doesNotContain("String_compare__")
                .doesNotContain("__capy_size(value)")
                .doesNotContain("__capy_unsupported('match')")
                .doesNotContain("assert_that(value).has_hour(1)")
                .doesNotContain("lambda time: assert_that(time).has_hour(1)");
    }

    @Test
    void shouldGenerateImportedUnqualifiedEnumValuesInJava() {
        var orderingSource = """
                enum Ordering { LESS, EQUAL, GREATER }
                """;
        var seqSource = """
                fun Seq[T].drop_until(pred: T => bool): Seq[T] = this
                """;
        var pathSource = """
                data Path { value: String }
                fun Path.`/`(segment: String): Path = this
                fun Path.`/`(other: Path): Path = this
                fun Path.normalize(): Path = this
                """;
        var dateTimeSource = """
                data DateTime {}
                fun from_timestamp(value: long): DateTime = DateTime {}
                fun DateTime.to_iso_8601(): String = "timestamp"
                """;
        var comparisonSource = """
                from /sample/lib/Ordering import { Ordering, EQUAL, GREATER }
                from /capy/collection/Seq import { * }
                from /capy/io/Path import { * }
                from /capy/date_time/DateTime import { * }
                import /capy/lang/Effect

                data Version { value: int }
                data Error { message: String }
                data Success[T] { value: T }
                union Result[T] = Success[T] | Error
                data Assertion { result: bool }
                data Check { assertions: List[() => Assertion] }
                data TestOutput { path: Path, content: String }

                fun compare_core(left: Version, right: Version): Ordering = GREATER

                fun compare_details(left: Version, right: Version): Ordering = GREATER

                fun compare_strings(left: String, right: String): Ordering = left.compare(right)

                fun pipe_then_or(result: Result[String]): Result[String] =
                    result
                    | (value => value)
                    .or(Success { "fallback" })

                fun unwrap_effects(effects: List[Effect[String]]): Effect[List[String]] =
                    match effects[0] with
                    case None -> Effect.pure([])
                    case Some { effect } -> {
                        let value <- effect
                        let rest <- unwrap_effects(effects[1:])
                        [value] + rest
                    }

                fun invoke_suppliers(suppliers: List[() => Assertion]): Option[Assertion] =
                    to_seq(suppliers)
                    | (supplier => supplier())
                    .drop_until(assertion => !assertion.result)
                    .first()

                fun create_parent(path: Path): Effect[Result[Path]] =
                    Effect.pure(Success { path })

                fun write_changed(path: Path, content: String): Effect[Result[String]] =
                    Effect.pure(Success { content })

                fun write_output(output_dir: Path, test_output: TestOutput): Effect[Result[Path]] =
                    let relative_path = test_output.path.normalize()
                    let output_path = output_dir / relative_path
                    create_parent(output_path).flat_map(parent_result =>
                        match parent_result with
                        case Error e -> Effect.pure(e)
                        case Success _ ->
                            write_changed(output_path, test_output.content).map(write_result =>
                                match write_result with
                                case Error e -> e
                                case Success _ -> Success { relative_path }
                            )
                    )

                fun normalize_paths(result: Result[List[Path]]): Result[List[Path]] =
                    match result with
                    case Error e -> e
                    case Success { paths } ->
                        let normalized = (paths | path => path.normalize()).as_list()
                        Success { normalized }

                fun timestamp(value: long): String = from_timestamp(value).to_iso_8601()

                fun check_error(result: Result[String], check: Error => Check): List[() => Assertion] =
                    match result with
                    case Error e -> check(e).assertions
                    case Success _ -> []

                fun join_values(values: List[String]): String =
                    (values |> "", (acc, item) => if acc == "" then item else acc + "," + item)

                fun contains_exponent(value: String): bool = value == "" | value ? "E" | value ? "e"

                fun escaped_controls(): String = "\\n\\r\\t"

                fun Version.compare(other: Version): Ordering =
                    match compare_core(this, other) with
                    case EQUAL -> compare_details(this, other)
                    case order -> order

                fun Version.`>`(other: Version): bool = this.compare(other) == GREATER
                """;
        var program = compileProgram(List.of(
                rawModule("Ordering", "/sample/lib", orderingSource, SourceKind.FUNCTIONAL),
                rawModule("Seq", "/capy/collection", seqSource, SourceKind.FUNCTIONAL),
                rawModule("Path", "/capy/io", pathSource, SourceKind.FUNCTIONAL),
                rawModule("DateTime", "/capy/date_time", dateTimeSource, SourceKind.FUNCTIONAL),
                rawModule("Comparison", "/sample/app", comparisonSource, SourceKind.FUNCTIONAL)
        ));

        var code = JavaGenerator.javaGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/Comparison.java"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(code)
                .contains("sample.lib.Ordering.GREATER")
                .contains("import static sample.lib.Ordering.GREATER;")
                .contains(".compareTo(")
                .contains("capy.lang.Ordering.EQUAL")
                .contains("capy.collection.Seq.Seq_T__drop_until__1_0")
                .contains("__capy_result_or_")
                .contains("flatMap(")
                .contains("supplier.get()")
                .contains(".contains(\"E\")")
                .contains("return \"\\n\\r\\t\";")
                .doesNotContain("Ordering.GREATER__")
                .doesNotContain("throw new UnsupportedOperationException(\"Unsupported CFUN expression at");
    }

    @Test
    void shouldGenerateJavaProgramMainEntrypoint() {
        var source = """
                from /capy/lang/Effect import { Effect, pure }
                from /capy/lang/Program import { Program, Success }

                fun main(args: List[String]): Effect[Program] =
                    pure(Success {})
                """;
        var program = compileProgram(List.of(rawModule("Main", "/sample/app", source, SourceKind.FUNCTIONAL)));

        var code = JavaGenerator.javaGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/Main.java"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(code)
                .contains("public static capy.lang.Effect<capy.lang.Program> main(java.util.List<java.lang.String> args)")
                .containsSubsequence(
                        "public static final void main(java.lang.String... args)",
                        "var __capybaraArgsList = java.util.List.of(args);",
                        "capy.lang.Program __capybaraProgram = main(__capybaraArgsList).unsafeRun();",
                        "if (__capybaraProgram instanceof capy.lang.Program.Failed __capybaraFailed)",
                        "java.lang.System.exit(__capybaraFailed.exit_code());"
                );
    }

    @Test
    void shouldRejectDirectProgramValueFromEffectMainDuringCompilation() {
        var result = CapybaraCompiler.compile(
                List.of(rawModule("Main", "/paper-soccer", """
                        from /capy/lang/Effect import { Effect }
                        from /capy/lang/Program import { Program, Success }

                        fun main(args: List[String]): Effect[Program] =
                            Success {}
                        """)),
                new LinkedHashSet<>(),
                emptyNativeProviders(),
                emptyNativeProviders()
        ).unsafeRun();

        assertThat(result).isInstanceOf(Either.Right.class);
        assertThat(((Either.Right<?, ?>) result).value().toString())
                .contains("Function `main` returns `Success`, but declares `Effect[Program]`.");
    }

    @Test
    void shouldGenerateJavaProgramMainEntrypointForSeqArguments() {
        var source = """
                import /capy/collection/Seq
                from /capy/lang/Effect import { Effect, pure }
                from /capy/lang/Program import { Program, Success }

                fun main(args: Seq[String]): Effect[Program] =
                    pure(Success {})
                """;
        var program = compileProgram(List.of(rawModule("Main", "/sample/app", source, SourceKind.FUNCTIONAL)));

        var code = JavaGenerator.javaGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/Main.java"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(code)
                .contains("public static final void main(java.lang.String... args)")
                .contains("main(capy.collection.Seq.<java.lang.String>toSeq(__capybaraArgsList)).unsafeRun()");
    }

    @Test
    void shouldGenerateQualifiedStandardLibraryCalls() {
        var source = """
                import /capy/lang/Effect
                import /capy/io/Console
                import /capy/lang/Primitives
                from /capy/collection/Seq import { Cons, Seq }
                from /capy/lang/Option import { Some, None }
                import /capy/lang/Async
                import /capy/lang/System
                import /capy/lang/Result
                from /capy/lang/Result import { Error, Success }
                import /capy/lang/Math
                import /capy/io/Path
                import /capy/io/IO
                import /capy/collection/Seq
                import /capy/collection/List

                private fun pure(value: String): String = value

                fun qualified_pure(value: String): Effect[String] =
                    Effect.pure(value)

                fun qualified_delay(value: String): Effect[String] =
                    Effect.delay(() => value)

                fun qualified_print(value: String): Effect[String] =
                    Console.println(value)

                fun qualified_parse(value: String): /capy/lang/Result[int] =
                    Primitives.to_int(value)

                fun qualified_compute(value: int): Async[int] =
                    Async.compute(() => {
                        let computed: int = value + 1
                        computed
                    })

                private fun render_result(result: Result[List[int]]): String = "done"

                fun qualified_compute_result(result: Result[Seq[int]]): Async[String] =
                    Async.compute(() => {
                        let r2: Result[List[int]] = result.flat_map(values => Success { values.as_list() })
                        render_result(r2)
                    })

                fun qualified_all(tasks: Seq[Async[int]]): Effect[List[Result[int]]] =
                    let results: List[Result[int]] <- Async.all(tasks)
                    Effect.pure(results)

                fun qualified_millis(): Effect[long] =
                    System.current_millis()

                fun qualified_property() =
                    System.system_property("java.version")

                fun qualified_digits(value: int): int =
                    Math.digits(value)

                fun qualified_error(value: String): Error =
                    Result.error(value)

                fun qualified_path(value: String): Path =
                    Path.from_string(value)

                fun qualified_exists(path: Path): Effect[bool] =
                    IO.exists(path)

                fun qualified_seq(values: List[int]): Seq[int] =
                    Seq.to_seq(values)
                """;
        var program = compileProgram(List.of(rawModule(
                "QualifiedEffect",
                "/sample/app",
                source,
                SourceKind.FUNCTIONAL
        )));

        var code = JavaGenerator.javaGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/QualifiedEffect.java"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(code)
                .contains("return capy.lang.Effect.pure(value);")
                .contains("return capy.lang.Effect.delay(")
                .contains("dev.capylang.ConsoleUtil.println")
                .contains("return __capy_parse_int(value);")
                .contains("return capy.lang.Async.start(capy.lang.Effect.delay(")
                .contains("capy.lang.Async.all(")
                .contains(").asList()).<java.util.List<java.lang.Object>>flatMap(")
                .contains("return capy.lang.System.currentMillis();")
                .contains("return capy.lang.System.systemProperty(\"java.version\");")
                .contains("java.lang.Integer.toString(value)")
                .contains("return __capy_error(value);")
                .contains("dev.capylang.PathUtil.fromString(value)")
                .contains("return capy.io.IO.exists__")
                .contains("return capy.collection.Seq.toSeq(values);")
                .doesNotContain("throw new UnsupportedOperationException(\"Unsupported CFUN expression at");

        var javaScriptCode = JavaScriptGenerator.javaScriptGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/QualifiedEffect.js"))
                .findFirst()
                .orElseThrow()
                .code();
        assertThat(javaScriptCode)
                .contains("__capy_async_start(delay(() =>")
                .contains("__capy_async_all(tasks)")
                .doesNotContain("return compute(")
                .doesNotContain("return all(tasks)");

        var pythonCode = PythonGenerator.pythonGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/QualifiedEffect.py"))
                .findFirst()
                .orElseThrow()
                .code();
        assertThat(pythonCode)
                .contains("__capy_async_start(delay(lambda :")
                .contains("__capy_async_all(tasks)")
                .doesNotContain("return compute(")
                .doesNotContain("return all(tasks)");
    }

    @Test
    void shouldFailJavaGenerationInsteadOfEmittingUnsupportedFunctionStubs() {
        var source = "fun broken(): int = missing()";
        var program = compileProgram(List.of(rawModule("Broken", "/sample/app", source, SourceKind.FUNCTIONAL)));

        assertThatThrownBy(() -> JavaGenerator.javaGenerator(program))
                .isInstanceOf(IllegalStateException.class)
                .hasMessage("Java generation failed for `sample/app/Broken.cfun` at 1:20 in function `broken`: "
                        + "unresolved function call `missing`. No Java source was written for this module.");
    }

    @Test
    void shouldGeneratePrimitiveBackedOperatorsAndStandardLibraryCalls() {
        var source = """
                from /capy/lang/Async import { Async, compute }
                from /capy/lang/Primitives import { to_int }
                from /capy/lang/Result import { Result }

                type digit -> int

                fun add(left: digit, right: digit): int = left + right
                fun compare(left: digit, right: digit): bool = left > right
                fun render(value: digit): String = value.to_string()
                fun parse(value: String): Result[int] = to_int(value)
                fun background(value: digit): Async[int] = compute(() => value + 1)
                """;
        var program = compileProgram(List.of(rawModule("BackendCoverage", "/sample/app", source, SourceKind.FUNCTIONAL)));

        var code = JavaGenerator.javaGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/BackendCoverage.java"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(code).doesNotContain("throw new UnsupportedOperationException(\"Unsupported CFUN expression at");
        assertThat(code).contains("__capy_parse_int(value)");
        assertThat(code).contains("capy.lang.Async.start(capy.lang.Effect.delay(");
        assertThat(code).contains("java.lang.String.valueOf(");
        assertThat(code)
                .contains("public static int add__")
                .contains("(int left, int right)")
                .contains("return (left + right);")
                .doesNotContain("__capy_data_field(left, \"value\")");
    }

    @Test
    void shouldGeneratePrimitiveBackedDataFieldsAndConstructorConditions() {
        var source = """
                data Error { kind: String, message: String }
                data Success[T] { value: T }
                union Result[T] = Success[T] | Error

                type digit -> int with constructor {
                    if value >= 0 & value <= 9
                    then Success { value }
                    else Error { kind: "invalid", message: "invalid digit" }
                }

                data Digits {
                    first: digit,
                    second: digit,
                    third: digit,
                    fourth: digit,
                } with constructor {
                    if first == second & second == third & third == fourth
                    then Error { kind: "invalid", message: "all digits match" }
                    else Success { * { first, second, third, fourth } }
                }

                fun total(value: Digits): int =
                    value.first * 1000 + value.second * 100 + value.third * 10 + value.fourth

                fun same(left: Digits, right: Digits): bool =
                    left.first == right.first & left.second == right.second

                fun render(value: Digits): String =
                    value.first.to_string() + value.second.to_string()

                fun from_list(values: List[digit]): Digits =
                    Digits! { values[0], values[1], values[2], values[3] }

                fun Digits.to_int(): int =
                    this.first * 1000 + this.second * 100 + this.third * 10 + this.fourth

                fun Digits.diff(): Digits =
                    let ordered = this
                    let diff: int = ordered.to_int() - ordered.to_int()
                    let a: digit = digit! { diff / 1000 }
                    let b: digit = digit! { (diff / 100) % 10 }
                    let c: digit = digit! { (diff / 10) % 10 }
                    let d: digit = digit! { diff % 10 }
                    Digits! { a, b, c, d }
                """;
        var program = compileProgram(List.of(rawModule("PrimitiveData", "/sample/app", source, SourceKind.FUNCTIONAL)));

        var code = JavaGenerator.javaGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/PrimitiveData.java"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(code).doesNotContain("throw new UnsupportedOperationException(\"Unsupported CFUN expression at");
        assertThat(code).contains("__capy_data_field(value, \"first\")");
        assertThat(code).contains("__capy_list_get_optional(values, 0)");

        var pythonCode = PythonGenerator.pythonGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/PrimitiveData.py"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(pythonCode)
                .contains("diff = __capy_sub(Digits_to_int__")
                .doesNotContain("diff = __capy_sub(__capy_parse_int(ordered)");
    }

    @Test
    void shouldRejectPrimitiveBackingValuesPassedToImportedDataFields() {
        var result = CapybaraCompiler.compile(
                primitiveBackedDataModules("""
                        from /support/Result import { Result }
                        from /Kaprekar import { Kaprekar }

                        const K1234: Result[Kaprekar] = Kaprekar { 1, 2, 3, 4 }
                        """),
                new LinkedHashSet<>(),
                emptyNativeProviders(),
                emptyNativeProviders()
        ).unsafeRun();

        assertThat(result).isInstanceOf(Either.Right.class);
        var errors = ((Either.Right<?, ?>) result).value().toString();
        assertThat(errors)
                .contains("Field `first` of data `Kaprekar` has type `int`, but requires primitive-backed type `digit`")
                .contains("Field `second` of data `Kaprekar` has type `int`, but requires primitive-backed type `digit`")
                .contains("construct `digit` explicitly and unwrap its `Result` before constructing `Kaprekar`");
    }

    @Test
    void shouldRejectPrimitiveBackingValuesPassedToUnsafeDataFields() {
        var result = CapybaraCompiler.compile(
                primitiveBackedDataModules("""
                        from /Kaprekar import { Kaprekar }

                        const KAPREKAR: Kaprekar = Kaprekar! { 6, 1, 7, 4 }
                        """),
                new LinkedHashSet<>(),
                emptyNativeProviders(),
                emptyNativeProviders()
        ).unsafeRun();

        assertThat(result).isInstanceOf(Either.Right.class);
        assertThat(((Either.Right<?, ?>) result).value().toString())
                .contains("Field `first` of data `Kaprekar` has type `int`, but requires primitive-backed type `digit`")
                .contains("Field `fourth` of data `Kaprekar` has type `int`, but requires primitive-backed type `digit`")
                .contains("construct `digit` explicitly and unwrap its `Result` before constructing `Kaprekar`");
    }

    @Test
    void shouldRejectPrimitiveBackingValuesPassedToDataFieldsFromCompiledLibraries() {
        var libraries = compileProgram(primitiveBackedLibraryModules());
        var result = CapybaraCompiler.compile(
                List.of(rawModule("Kaprekar.test", "", """
                        from /support/Result import { Result }
                        from /Kaprekar import { Kaprekar }

                        const K1234: Result[Kaprekar] = Kaprekar { 1, 2, 3, 4 }
                        """)),
                new LinkedHashSet<>(libraries.modules()),
                emptyNativeProviders(),
                emptyNativeProviders()
        ).unsafeRun();

        assertThat(result).isInstanceOf(Either.Right.class);
        assertThat(((Either.Right<?, ?>) result).value().toString())
                .contains("Field `first` of data `Kaprekar` has type `int`, but requires primitive-backed type `digit`")
                .contains("Field `fourth` of data `Kaprekar` has type `int`, but requires primitive-backed type `digit`")
                .contains("construct `digit` explicitly and unwrap its `Result` before constructing `Kaprekar`");
    }

    @Test
    void shouldRejectPrimitiveBackingValuesPassedToUnsafeDataFieldsFromCompiledLibraries() {
        var libraries = compileProgram(primitiveBackedLibraryModules());
        var result = CapybaraCompiler.compile(
                List.of(rawModule("Kaprekar.test", "", """
                        from /Kaprekar import { Kaprekar }

                        const KAPREKAR: Kaprekar = Kaprekar! { 6, 1, 7, 4 }
                        """)),
                new LinkedHashSet<>(libraries.modules()),
                emptyNativeProviders(),
                emptyNativeProviders()
        ).unsafeRun();

        assertThat(result).isInstanceOf(Either.Right.class);
        assertThat(((Either.Right<?, ?>) result).value().toString())
                .contains("Field `first` of data `Kaprekar` has type `int`, but requires primitive-backed type `digit`")
                .contains("Field `fourth` of data `Kaprekar` has type `int`, but requires primitive-backed type `digit`")
                .contains("construct `digit` explicitly and unwrap its `Result` before constructing `Kaprekar`");
    }

    @Test
    void shouldAcceptConstructedPrimitiveBackedValuesInDataFields() {
        compileProgram(primitiveBackedDataModules("""
                from /support/Result import { Result }
                from /support/Digit import { digit }
                from /Kaprekar import { Kaprekar }

                fun k1234(): Result[Kaprekar] =
                    let first: digit <- digit { 1 }
                    let second: digit <- digit { 2 }
                    let third: digit <- digit { 3 }
                    let fourth: digit <- digit { 4 }
                    Kaprekar { first, second, third, fourth }
                """));
    }

    private static List<RawModule> primitiveBackedDataModules(String consumerSource) {
        var modules = new java.util.ArrayList<>(primitiveBackedLibraryModules());
        modules.add(rawModule("Kaprekar.test", "", consumerSource));
        return modules;
    }

    private static List<RawModule> primitiveBackedLibraryModules() {
        return List.of(
                rawModule("Result", "/support", """
                        data Error { kind: String, message: String }
                        data Success[T] { value: T }
                        union Result[T] = Success[T] | Error
                        """),
                rawModule("Digit", "/support", """
                        from /support/Result import { Success }

                        type digit -> int with constructor {
                            Success { value }
                        }
                        """),
                rawModule("Kaprekar", "", """
                        from /support/Result import { Success }
                        from /support/Digit import { digit }

                        data Kaprekar {
                            first: digit,
                            second: digit,
                            third: digit,
                            fourth: digit
                        } with constructor {
                            Success { * { first, second, third, fourth } }
                        }
                        """)
        );
    }

    @Test
    void shouldLowerDynamicBackendResultAndImportedPrimitiveOperations() {
        var resultSource = """
                data Error { kind: String, message: String }
                data Success[T] { value: T }
                union Result[T] = Success[T] | Error
                """;
        var digitSource = """
                from /capy/lang/Result import { Success }

                type digit -> int with constructor {
                    Success { value }
                }

                data Item { value: int }

                fun Item.to_string(): String = this.value.to_string()

                fun digit.render(): String = this.to_string()

                fun raw_digit(): digit = digit! { 1 }
                """;
        var consumerSource = """
                from /capy/lang/Result import { Result, Success }
                from /sample/Digit import { digit, Item }
                import /capy/lang/Effect
                import /capy/io/Console
                import /capy/lang/Primitives

                fun parse_digits(values: List[int]): Result[List[digit]] =
                    values
                    | value => Success { value }
                    |> Success { [] }, (acc, int_result) => {
                        acc.flat_map(acc_list => {
                            int_result.flat_map(int_value => {
                                digit { int_value }.map(digit => acc_list + digit)
                            })
                        })
                    }

                fun map_results(values: List[Result[int]]): List[Result[int]] =
                    (values | result => result.map(value => value)).as_list()

                fun flat_map_results(values: List[Result[int]]): List[Result[int]] =
                    (values | result => result.flat_map(value => Success { value })).as_list()

                fun qualified_pure(value: String): Effect[String] = Effect.pure(value)

                fun qualified_print(value: String): Effect[String] = Console.println(value)

                fun qualified_parse(value: String): Result[int] = Primitives.to_int(value)

                fun render_items(result: Result[List[Item]]): List[String] =
                    match result with
                    case Success { value } -> (value | item => item.to_string()).as_list()
                    case Error _ -> []

                fun render_cons(result: Cons[Item]): String =
                    result.value.to_string()

                fun cons_rest(result: Cons[Item]): Seq[Item] =
                    result.rest()

                fun first_or_zero(values: List[int]): int =
                    match values[0] with
                    case Some { value } -> value
                    case None -> 0

                fun join(values: List[String]): String =
                    values.reduce("", (acc, value) => if acc.is_empty() then value else acc + value)
                """;
        var program = compileProgram(List.of(
                rawModule("Result", "/capy/lang", resultSource, SourceKind.FUNCTIONAL),
                rawModule("Seq", "/capy/collection", """
                        data End {}
                        data Cons[T] { value: T, rest: () => Seq[T] }
                        union Seq[T] = End | Cons[T]
                        """, SourceKind.FUNCTIONAL),
                rawModule("Digit", "/sample", digitSource, SourceKind.FUNCTIONAL),
                rawModule("UseDigit", "/sample", consumerSource, SourceKind.FUNCTIONAL)
        ));

        var generated = PythonGenerator.pythonGenerator(program);
        var code = generated.modules().stream()
                .filter(module -> module.relativePath().equals("sample/UseDigit.py"))
                .findFirst()
                .orElseThrow()
                .code();
        var digitCode = generated.modules().stream()
                .filter(module -> module.relativePath().equals("sample/Digit.py"))
                .findFirst()
                .orElseThrow()
                .code();
        var runtimeCode = generated.modules().stream()
                .filter(module -> module.relativePath().equals("capy/test/CapyTestRuntime.py"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(code)
                .contains("__capy_result_flat_map(acc")
                .contains("__capy_result_flat_map(int_result")
                .contains("__capy_result_map(__capy_primitive_constructor_result(__import__(\"sample.Digit\", fromlist=['*'])")
                .contains("__capy_result_map(result")
                .contains("__capy_result_flat_map(result")
                .contains("return pure(value)")
                .contains("return println(value)")
                .contains("return __capy_parse_int(value)")
                .contains(" = sample.Digit.Item_to_string__")
                .contains("Item_to_string__")
                .contains("__capy_seq_first_value(result)")
                .contains("return __capy_seq_rest(result)")
                .contains("__capy_index(values, 0)")
                .doesNotContain("Effect_pure")
                .doesNotContain("Console_println")
                .doesNotContain("Primitives_to_int")
                .doesNotContain("__capy_dynamic_map(result")
                .doesNotContain("__capy_dynamic_flat_map(result")
                .doesNotContain("__capy_seq_flat_map(acc")
                .doesNotContain("__capy_seq_flat_map(int_result")
                .doesNotContain("int_value.map(");
        assertThat(digitCode).contains("return __capy_to_string(this)");
        assertThat(runtimeCode)
                .contains("end=chr(10) if newline else ''")
                .contains("def does_not_contain(self, other):")
                .contains("rest=lambda: __capy_seq_map(__capy_seq_rest(value), mapper)")
                .contains("return sum(1 for _ in __capy_as_iterable(value))")
                .contains("return lambda: value[1:]");

        var javaScriptGenerated = JavaScriptGenerator.javaScriptGenerator(program);
        var javaScriptCode = javaScriptGenerated.modules().stream()
                .filter(module -> module.relativePath().equals("sample/UseDigit.js"))
                .findFirst()
                .orElseThrow()
                .code();
        var javaScriptDigitCode = javaScriptGenerated.modules().stream()
                .filter(module -> module.relativePath().equals("sample/Digit.js"))
                .findFirst()
                .orElseThrow()
                .code();
        var javaScriptRuntimeCode = javaScriptGenerated.modules().stream()
                .filter(module -> module.relativePath().equals("capy/test/CapyTestRuntime.js"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(javaScriptCode)
                .contains("__capy_primitive_constructor_result(require(\"../sample/Digit\")[")
                .contains("return pure(value)")
                .contains("return println(value)")
                .contains("return __capy_parse_int(value)")
                .contains("Item_to_string__")
                .contains("__capy_seq_first_value(result)")
                .contains("return __capy_seq_rest(result)")
                .contains("__capy_dynamic_flat_map(int_result")
                .contains("__capy_result_map(__capy_primitive_constructor_result")
                .contains("(__capy_size(acc) === 0)")
                .doesNotContain("__capy_import_capy_lang_Effect")
                .doesNotContain("__capy_import_capy_io_Console")
                .doesNotContain("__capy_import_capy_lang_Primitives")
                .doesNotContain("Effect_pure")
                .doesNotContain("Console_println")
                .doesNotContain("Primitives_to_int")
                .doesNotContain("item.to_string(")
                .doesNotContain("result.rest(")
                .doesNotContain("acc.is_empty(");
        assertThat(javaScriptDigitCode)
                .contains("return __capy_to_string(this_)")
                .contains("return 1")
                .contains("\"__capy_constructor_digit");
        assertThat(javaScriptRuntimeCode)
                .contains("function __capy_primitive_constructor_result(value)")
                .contains("function __capy_dynamic_map(value, mapper)")
                .contains("function __capy_dynamic_flat_map(value, mapper)")
                .contains("String.fromCharCode(10)")
                .doesNotContain("split(/\\\n");
    }

    @Test
    void shouldLowerImportedStandardPrimitiveConstructorAsResultInDynamicBackends() {
        var source = """
                from /capy/lang/Primitives import { digit }
                from /capy/lang/Result import { Result }

                fun increment_digit(value: int): Result[int] =
                    digit { value }.map(digit => digit + 1)
                """;
        var program = compileProgram(List.of(rawModule(
                "StandardPrimitive",
                "/sample/app",
                source,
                SourceKind.FUNCTIONAL
        )));

        var javaScriptCode = JavaScriptGenerator.javaScriptGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/StandardPrimitive.js"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(javaScriptCode)
                .contains("__capy_result_map(__capy_digit(value)")
                .doesNotContain("capy/lang/Primitives")
                .doesNotContain("__capy_result_map(value,")
                .doesNotContain("__capy_get_field(value, 'value').map(");

        var pythonCode = PythonGenerator.pythonGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/StandardPrimitive.py"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(pythonCode)
                .contains("__capy_result_map(__capy_digit(value)")
                .doesNotContain("capy.lang.Primitives")
                .doesNotContain("__capy_result_map(value,");
    }

    @Test
    void shouldPreferCurrentStdlibSourceValuesOverPackagedFallbacks() {
        var primitivesSource = """
                from /capy/lang/Result import { Success }

                type digit -> int with constructor {
                    Success { value + 1 }
                }

                const ONE_DIGIT: digit = digit! { 40 }
                """;
        var consumerSource = """
                from /capy/lang/Primitives import { digit, ONE_DIGIT }
                from /capy/lang/Result import { Result }

                fun current_constant(): digit = ONE_DIGIT
                fun current_constructor(value: int): Result[digit] = digit { value }
                """;
        var program = compileProgram(List.of(
                rawModule("Primitives", "/capy/lang", primitivesSource, SourceKind.FUNCTIONAL),
                rawModule("Consumer", "/sample/app", consumerSource, SourceKind.FUNCTIONAL)
        ));

        var javaScriptCode = JavaScriptGenerator.javaScriptGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/Consumer.js"))
                .findFirst()
                .orElseThrow()
                .code();
        assertThat(javaScriptCode)
                .contains("__capy_import_capy_lang_Primitives[\"ONE_DIGIT")
                .contains("require(\"../../capy/lang/Primitives\")[\"__capy_constructor_digit")
                .doesNotContain("return 1;")
                .doesNotContain("__capy_digit(value)");

        var pythonCode = PythonGenerator.pythonGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/Consumer.py"))
                .findFirst()
                .orElseThrow()
                .code();
        assertThat(pythonCode)
                .contains("return __import__(\"capy.lang.Primitives\", fromlist=['*']).ONE_DIGIT")
                .contains("__import__(\"capy.lang.Primitives\", fromlist=['*']).__capy_constructor_digit")
                .doesNotContain("__capy_digit(value)");
    }

    @Test
    void shouldKeepIndexExpressionsBeforePipeLambdas() {
        var source = """
                from /capy/lang/Option import { Option, Some }

                fun second(values: List[int]): Option[int] =
                    values[1] | value => Some { value }
                """;
        var program = compileProgram(List.of(rawModule("IndexedPipe", "/sample/app", source, SourceKind.FUNCTIONAL)));

        var code = JavaGenerator.javaGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/IndexedPipe.java"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(code).doesNotContain("throw new UnsupportedOperationException(\"Unsupported CFUN expression at");
        assertThat(code).contains("__capy_list_get_optional(values, 1)");
    }

    @Test
    void shouldGenerateFlatPythonStringConcatenation() {
        var source = """
                fun pieces(a: String, b: String, c: String, d: String): String =
                    a + "-" + b + "-" + c + "-" + d
                """;
        var program = compileProgram(List.of(rawModule("StringConcat", "/sample/app", source, SourceKind.FUNCTIONAL)));

        var code = PythonGenerator.pythonGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/StringConcat.py"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(code).contains("return \"\".join([a, \"-\", b, \"-\", c, \"-\", d])");
    }

    @Test
    void shouldGeneratePythonCollectionReduceOperator() {
        var source = """
                fun joined(values: List[String]): String =
                    values |> "", (acc, value) => acc + value
                """;
        var program = compileProgram(List.of(rawModule("CollectionReduce", "/sample/app", source, SourceKind.FUNCTIONAL)));

        var code = PythonGenerator.pythonGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/CollectionReduce.py"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(code).contains("__capy_reduce(");
        assertThat(code).doesNotContain("|>");
    }

    @Test
    void shouldGenerateUnionParentFieldsInJavaDataDeclarations() {
        var source = """
                union A { a: String } = B | C

                data B { x: int }
                data C { y: String }
                """;
        var program = compileProgram(List.of(rawModule("Main", "/sample/app", source, SourceKind.FUNCTIONAL)));

        var code = JavaGenerator.javaGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/Main.java"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(code).contains("""
                    public sealed interface A {
                        public String a();
                    }
                """);
        assertThat(code).contains("    public record B(String a, int x) implements A {}");
        assertThat(code).contains("    public record C(String a, String y) implements A {}");
    }

    @Test
    void shouldAvoidSealingUnionWithExternalJavaVariants() {
        var ownerSource = """
                union A { a: String } = B

                data B { b: String }
                """;
        var extensionSource = """
                from /sample/owner/Owner import { A }

                data C { ... A, c: String }
                """;
        var program = compileProgram(List.of(
                rawModule("Owner", "/sample/owner", ownerSource, SourceKind.FUNCTIONAL),
                rawModule("Extension", "/sample/ext", extensionSource, SourceKind.FUNCTIONAL)
        ));

        var generated = JavaGenerator.javaGenerator(program);
        var ownerCode = generated.modules().stream()
                .filter(module -> module.relativePath().equals("sample/owner/Owner.java"))
                .findFirst()
                .orElseThrow()
                .code();
        var extensionCode = generated.modules().stream()
                .filter(module -> module.relativePath().equals("sample/ext/Extension.java"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(ownerCode).contains("    public interface A {");
        assertThat(ownerCode).doesNotContain("public sealed interface A");
        assertThat(ownerCode).contains("    public record B(String a, String b) implements A {}");
        assertThat(extensionCode).contains("    public record C(String a, String c) implements sample.owner.Owner.A {}");
    }

    @Test
    void shouldGenerateGrandparentUnionFieldsInJavaDataDeclarations() {
        var source = """
                union A { a: String } = B

                union B { b: String } = C

                data C { c: String }
                """;
        var program = compileProgram(List.of(rawModule("Main", "/sample/app", source, SourceKind.FUNCTIONAL)));

        var code = JavaGenerator.javaGenerator(program).modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/Main.java"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(code).contains("""
                    public sealed interface A {
                        public String a();
                    }
                """);
        assertThat(code).contains("    public sealed interface B extends A {");
        assertThat(code).contains("        public String b();");
        assertThat(code).contains("    public record C(String a, String b, String c) implements B {}");
    }

    @Test
    void shouldPreservePrimitiveBackedFieldTypesInJavaDataDeclarations() {
        var programSource = """
                union Program = Success | Failed

                data Success {}
                data Failed { exit_code: failed_exit_code }

                type failed_exit_code -> int
                """;
        var consumerSource = """
                from /capy/lang/Program import { * }

                fun fail(): Program =
                    let exit_code: failed_exit_code = failed_exit_code! { 1 }
                    Failed { exit_code }
                """;
        var program = compileProgram(List.of(
                rawModule("Program", "/capy/lang", programSource, SourceKind.FUNCTIONAL),
                rawModule("Main", "/sample/app", consumerSource, SourceKind.FUNCTIONAL)
        ));

        var generated = JavaGenerator.javaGenerator(program);
        var programCode = generated.modules().stream()
                .filter(module -> module.relativePath().equals("capy/lang/Program.java"))
                .findFirst()
                .orElseThrow()
                .code();
        var consumerCode = generated.modules().stream()
                .filter(module -> module.relativePath().equals("sample/app/Main.java"))
                .findFirst()
                .orElseThrow()
                .code();

        assertThat(programCode).contains("    public record Failed(int exit_code) {}");
        assertThat(programCode).doesNotContain("record failed_exit_code");
        assertThat(consumerCode)
                .contains("int exit_code = 1;")
                .contains("new capy.lang.Program.Failed(exit_code)");
    }

    @Test
    void shouldRejectObjectOrientedThrowingNonError() {
        var objectSource = """
                class BadThrow {
                    def fail(): String {
                        throw "boom"
                    }
                }
                """;
        var result = CapybaraCompiler.compile(
                List.of(rawModule("BadThrow", "/sample/app", objectSource, SourceKind.OBJECT_ORIENTED)),
                new LinkedHashSet<>(),
                emptyNativeProviders(),
                emptyNativeProviders()
        ).unsafeRun();

        assertThat(result).isInstanceOf(Either.Right.class);
        var errors = (List<?>) ((Either.Right<?, ?>) result).value();
        assertThat(errors.toString()).contains("OO throw expression must have type `/capy/lang/Result.Error`.");
    }

    @Test
    void shouldRejectObjectOrientedNonFinalIfThatFallsThrough() {
        var objectSource = """
                class BadIf {
                    def label(): String {
                        return "side"
                    }

                    def run(flag: bool): String {
                        if flag {
                            this.label()
                        }
                        return "ok"
                    }
                }
                """;
        var result = CapybaraCompiler.compile(
                List.of(rawModule("BadIf", "/sample/app", objectSource, SourceKind.OBJECT_ORIENTED)),
                new LinkedHashSet<>(),
                emptyNativeProviders(),
                emptyNativeProviders()
        ).unsafeRun();

        assertThat(result).isInstanceOf(Either.Right.class);
        var errors = (List<?>) ((Either.Right<?, ?>) result).value();
        assertThat(errors.toString()).contains("Unsupported object-oriented construct");
    }

    private static String generatorOutputType(OutputType outputType) {
        return switch (outputType) {
            case JAVA -> "java";
            case PYTHON -> "python";
            case JAVASCRIPT -> "javascript";
        };
    }

    private static RawModule rawModule(String name, String path, String input) {
        return rawModule(name, path, input, SourceKind.FUNCTIONAL);
    }

    private static RawModule rawModule(String name, String path, String input, SourceKind sourceKind) {
        return new RawModule(name, path, input, sourceKind);
    }

    private static CompiledProgram compileProgram(List<RawModule> rawModules) {
        var result = CapybaraCompiler.compile(rawModules, new LinkedHashSet<>(), emptyNativeProviders(), emptyNativeProviders()).unsafeRun();
        if (result instanceof Either.Right<?, ?> error) {
            fail(error.value().toString());
        }
        return (CompiledProgram) ((Either.Left<?, ?>) result).value();
    }

    private static NativeProviderManifest emptyNativeProviders() {
        return new NativeProviderManifest(List.of());
    }

    static Stream<Arguments> test() {
        return Stream.of(
                Arguments.of("""
                                // algebraic type
                                union Shape = Circle | Rectangle
                                data Circle { radius: double }
                                data Rectangle { width: double, height: double }
                                
                                fun area(shape: Shape): double =
                                    match shape with
                                    case Circle { radius } -> 3.14 * radius ^ 2
                                    case Rectangle { width, height } -> width * height
                                
                                fun da_vinci(shape: Shape): Shape =
                                    match shape with
                                    case Circle { radius } -> Rectangle { width : radius * 2, height : radius * 2 }
                                    case Rectangle { width, height } -> Circle { radius : (width + height) / 4 }
                                
                                // type with common value
                                union Person { name: String, age: int } = Student | Teacher
                                data Student { grade: int }
                                data Teacher { subject: String }
                                """,
                        """
                                fun classify(x: int): String =
                                    if x > 0 then "positive"
                                    else "non-positive"
                                fun always_true(): bool = true
                                
                                // algebraic type
                                union Shape = Circle | Rectangle
                                data Circle { radius: double }
                                data Rectangle { width: double, height: double }
                                
                                fun area(shape: Shape): double =
                                    match shape with
                                    case Circle { radius } -> 3.14 * radius * radius
                                    case Rectangle { width, height } -> width * height
                                
                                fun da_vinci(shape: Shape): Shape =
                                    match shape with
                                    case Circle { radius } -> Rectangle { width = radius * 2, height = radius * 2 }
                                    case Rectangle { width, height } -> Circle { radius = (width + height) / 4 }
                                """,
                        """
                                // algebraic type
                                union Shape = Circle | Rectangle
                                data Circle { radius: double }
                                data Rectangle { width: double, height: double }
                                
                                fun area(shape: Shape): double =
                                    match shape with
                                    case Circle { radius } -> 3.14 * radius ^ 2
                                    case Rectangle { width, height } -> width * height
                                
                                fun da_vinci(shape: Shape): Shape =
                                    match shape with
                                    case Circle { radius } -> Rectangle { width : radius * 2, height : radius * 2 }
                                    case Rectangle { width, height } -> Circle { radius : (width + height) / 4 }
                                """,
                        """
                                // type with common value
                                union Person { name: String, age: int } = Student | Teacher
                                data Student { grade: int }
                                data Teacher { subject: String }
                                """,
                        """
                                union Option[T] = Some[T] | None
                                data Some[T] { value: T }
                                data None {}
                                fun tuple(): Tuple[int, String, double] = (1, "foo", 5.0)
                                fun tuple2(): Tuple[int, Option[String], double] = (1, Some { value: "foo" }, 5.0)
                                fun tuple_index(): Option[String] = (1, "foo", 5.0)[1]
                                fun tuple_index_negative(): Option[String] = (1, "foo", 5.0)[-2]
                                fun tuple_slice(): Tuple[String, double] = (1, "foo", 5.0)[1:]
                                fun tuple_slice_negative(): Tuple[int, String] = (1, "foo", 5.0)[:-1]
                                fun tuple_if(x: int): Tuple[int, String, float, String] =
                                    (5, if x > 4 then "big" else "small", 5.1f, "foo")
                                """)
        );
    }
}
