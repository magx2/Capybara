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
                .contains("public static @dev.capylang.PrimitiveType(cfunType = \"/foo/Ids.user_id\") int plus(@dev.capylang.PrimitiveType(cfunType = \"/foo/Ids.user_id\") int left, @dev.capylang.PrimitiveType(cfunType = \"/foo/Ids.user_id\") int right)")
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
                .doesNotContain("user_id(")
                .doesNotContain("compiledprimitive");
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

    private static CompiledProgram compileProgram(List<RawModule> modules) {
        var result = CapybaraCompiler.INSTANCE.compile(modules, new TreeSet<>());
        if (result instanceof Result.Error<CompiledProgram> error) {
            fail(error.errors().toString());
        }
        return ((Result.Success<CompiledProgram>) result).value();
    }
}
