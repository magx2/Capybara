package dev.capylang.generator;

import dev.capylang.compiler.CapybaraCompiler;
import dev.capylang.compiler.CompiledProgram;
import dev.capylang.compiler.Result;
import dev.capylang.compiler.parser.RawModule;
import org.junit.jupiter.api.Test;

import java.nio.file.Path;
import java.util.List;
import java.util.TreeSet;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.fail;

class PrimitiveBackedTypeGeneratorTest {
    @Test
    void shouldErasePrimitiveBackedTypesInJava() {
        var code = generatedCode(new JavaGenerator(), Path.of("foo", "Ids.java"));

        assertThat(code)
                .contains("public static @dev.capylang.PrimitiveType(cfunType = \"/foo/Ids.user_id\") int make(int value)")
                .contains("public static int unwrap(@dev.capylang.PrimitiveType(cfunType = \"/foo/Ids.user_id\") int value)")
                .contains("public static @dev.capylang.PrimitiveType(cfunType = \"/foo/Ids.user_id\") int plus__name_plus__user_id__user_id(@dev.capylang.PrimitiveType(cfunType = \"/foo/Ids.user_id\") int this_, @dev.capylang.PrimitiveType(cfunType = \"/foo/Ids.user_id\") int other)")
                .contains("public static @dev.capylang.PrimitiveType(cfunType = \"/foo/Ids.user_id\") int plus__user_id__user_id(@dev.capylang.PrimitiveType(cfunType = \"/foo/Ids.user_id\") int left, @dev.capylang.PrimitiveType(cfunType = \"/foo/Ids.user_id\") int right)")
                .contains("return foo.Ids.plus__name_plus__user_id__user_id(left, right);")
                .contains("return value;");
        assertThat(code)
                .doesNotContain("record UserId")
                .doesNotContain("class UserId")
                .doesNotContain("interface UserId")
                .doesNotContain("new UserId")
                .doesNotContain("compiledprimitive");
    }

    @Test
    void shouldKeepImportedPrimitiveBackedJavaAnnotationsFullyQualified() {
        var program = compileProgram(List.of(
                new RawModule("UserIds", "/foo/users", """
                        type user_id -> int
                        """),
                new RawModule("OrderIds", "/foo/orders", """
                        type user_id -> long
                        """),
                new RawModule("Consumer", "/foo/app", """
                        from /foo/users/UserIds import { user_id }

                        fun echo(id: user_id): user_id = id
                        """)
        ));

        var code = generatedCode(new JavaGenerator(), program, Path.of("foo", "app", "Consumer.java"));

        assertThat(code)
                .contains("public static @dev.capylang.PrimitiveType(cfunType = \"/foo/users/UserIds.user_id\") int echo(@dev.capylang.PrimitiveType(cfunType = \"/foo/users/UserIds.user_id\") int id)")
                .doesNotContain("cfunType = \"user_id\"");
    }

    @Test
    void shouldExposeGeneratedPrimitiveBackedConstructorHelpersInJava() {
        var program = compileProgram(List.of(new RawModule("Numbers", "/foo", """
                from /capy/lang/Result import { * }

                type count -> int with constructor {
                    if value >= 0
                    then Success { value }
                    else Error { "bad count" }
                }
                """)));

        var code = generatedCode(new JavaGenerator(), program, Path.of("foo", "Numbers.java"));

        assertThat(code)
                .contains("public static capy.lang.Result<java.lang.Integer> __constructorPrimitiveCount(int value)");
    }

    @Test
    void shouldErasePrimitiveBackedTypesInJavaScript() {
        var code = generatedCode(new JavaScriptGenerator(), Path.of("foo", "Ids.js"));

        assertThat(code)
                .contains("function make(value)")
                .contains("function unwrap(value)")
                .contains("const __capybaraPrimitiveTypes = Object.freeze({")
                .contains("user_id: Object.freeze({ cfunType: '/foo/Ids.user_id', backingType: 'int' })");
        assertThat(code)
                .doesNotContain("class UserId")
                .doesNotContain("class user_id")
                .doesNotContain("new UserId")
                .doesNotContain("new user_id")
                .doesNotContain("compiledprimitive");
    }

    @Test
    void shouldErasePrimitiveBackedTypesInPython() {
        var code = generatedCode(new PythonGenerator(), Path.of("foo", "Ids.py"));

        assertThat(code)
                .contains("def make(value):")
                .contains("def unwrap(value):")
                .contains("__capybaraPrimitiveTypes = {")
                .contains("'user_id': {\"cfunType\": '/foo/Ids.user_id', \"backingType\": 'int'}");
        assertThat(code)
                .doesNotContain("class UserId")
                .doesNotContain("class user_id")
                .doesNotContain("UserId(")
                .doesNotContain("return user_id(")
                .doesNotContain("= user_id(")
                .doesNotContain("compiledprimitive");
    }

    @Test
    void shouldEraseStringBackedTypesInJavaScriptAndPython() {
        var program = compileProgram(List.of(new RawModule("Tokens", "/foo", """
                type token -> String

                fun make(value: String): token = token { value }
                fun unwrap(value: token): String = @value
                """)));

        var js = generatedCode(new JavaScriptGenerator(), program, Path.of("foo", "Tokens.js"));
        var python = generatedCode(new PythonGenerator(), program, Path.of("foo", "Tokens.py"));

        assertThat(js)
                .contains("token: Object.freeze({ cfunType: '/foo/Tokens.token', backingType: 'String' })")
                .doesNotContain("class Token")
                .doesNotContain("new Token");
        assertThat(python)
                .contains("'token': {\"cfunType\": '/foo/Tokens.token', \"backingType\": 'String'}")
                .doesNotContain("class Token")
                .doesNotContain("Token(");
    }

    @Test
    void shouldNotShadowPythonRuntimeWithPrimitiveBackedConstructorAliases() {
        var program = compileProgram(List.of(new RawModule("Numbers", "/foo", """
                from /capy/lang/Result import { * }

                type count -> int with constructor {
                    if value >= 0
                    then Success { value }
                    else Error { "bad count" }
                }

                type index -> int with constructor {
                    if value >= 0
                    then Success { value }
                    else Error { "bad index" }
                }

                fun next(value: index): index = index! { @value + 1 }
                """)));

        var code = generatedCode(new PythonGenerator(), program, Path.of("foo", "Numbers.py"));

        assertThat(code)
                .contains("import dev.capylang.capybara as capy")
                .contains("capy.int_add")
                .doesNotContain("def capy(*args):");
    }

    @Test
    void shouldUseLocalPrimitiveBackedConstructorsInJavaScriptModuleWithMatchingDataName() {
        var program = compileProgram(List.of(semVerModule()));

        var code = generatedCode(new JavaScriptGenerator(), program, Path.of("foo", "SemVer.js"));

        assertThat(code)
                .contains("__constructor__primitive__major(value)")
                .doesNotContain("require('./SemVer.js')")
                .doesNotContain("__module_foo_SemVerModule.__constructor__primitive__major");
    }

    @Test
    void shouldUseLocalPrimitiveBackedConstructorsInPythonModuleWithMatchingDataName() {
        var program = compileProgram(List.of(semVerModule()));

        var code = generatedCode(new PythonGenerator(), program, Path.of("foo", "SemVer.py"));

        assertThat(code)
                .contains("capy__constructorPrimitiveMajor(value)")
                .doesNotContain("import foo.SemVer as capy_module_foo_SemVerModule")
                .doesNotContain("capy_module_foo_SemVerModule.__constructor__primitive__major");
    }

    private static String generatedCode(Generator generator, Path modulePath) {
        return generatedCode(generator, program(), modulePath);
    }

    private static String generatedCode(Generator generator, CompiledProgram program, Path modulePath) {
        return generator.generate(program).modules().stream()
                .filter(module -> module.relativePath().equals(modulePath))
                .map(GeneratedModule::code)
                .findFirst()
                .orElseThrow(() -> new AssertionError("Generated module " + modulePath + " not found"));
    }

    private static CompiledProgram program() {
        return compileProgram(List.of(new RawModule("Ids", "/foo", """
                type user_id -> int

                fun make(value: int): user_id = user_id { value }
                fun unwrap(value: user_id): int = @value
                fun user_id.plus(other: user_id): user_id = user_id! { @this + @other }
                fun plus(left: user_id, right: user_id): user_id = left.plus(right)
                """)));
    }

    private static RawModule semVerModule() {
        return new RawModule("SemVer", "/foo", """
                from /capy/lang/Result import { * }

                type major -> int with constructor {
                    if value >= 0
                    then Success { value }
                    else Error { "bad major" }
                }

                data SemVer { major: major }

                fun sem_ver(value: int): Result[SemVer] =
                    let major_value <- major { value }
                    Success { SemVer { major_value } }
                """);
    }

    private static CompiledProgram compileProgram(List<RawModule> modules) {
        var result = CapybaraCompiler.INSTANCE.compile(modules, new TreeSet<>());
        if (result instanceof Result.Error<CompiledProgram> error) {
            fail(error.errors().toString());
        }
        return ((Result.Success<CompiledProgram>) result).value();
    }
}
