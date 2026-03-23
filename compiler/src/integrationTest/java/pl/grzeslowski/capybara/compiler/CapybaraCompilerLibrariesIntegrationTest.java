package pl.grzeslowski.capybara.compiler;

import org.junit.jupiter.api.Test;
import pl.grzeslowski.capybara.generator.GeneratedModule;
import pl.grzeslowski.capybara.generator.JavaGenerator;
import pl.grzeslowski.capybara.parser.CapybaraParser;

import java.nio.file.Path;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.fail;

class CapybaraCompilerLibrariesIntegrationTest {
    @Test
    void shouldGenerateJavaReferencingLibraryWithoutEmittingIt() {
        var libraries = compileProgram(new Program(List.of(new Module(
                "Library",
                "/foo/lib",
                CapybaraParser.INSTANCE.parseFunctional("""
                        data Message { value: string }
                        fun make_message(value: string): Message = Message { value: value }
                        """)
        ))), new TreeSet<>()).modules();

        var consumer = new Module(
                "Consumer",
                "/foo/app",
                CapybaraParser.INSTANCE.parseFunctional("""
                        fun consume(value: string): string = make_message(value).value
                        """),
                List.of(new ImportDeclaration("Library", List.of("*"), List.of()))
        );

        var generated = new JavaGenerator().generate(compileProgram(new Program(List.of(consumer)), libraries));

        assertThat(generated.modules()).hasSize(1);
        var module = generated.modules().getFirst();
        assertThat(module.relativePath()).isEqualTo(Path.of("foo", "app", "Consumer.java"));
        assertThat(module.code()).contains("import static foo.lib.Library.*;");
        assertThat(module.code()).contains("makeMessage(value)");
        assertThat(generated.modules())
                .extracting(GeneratedModule::relativePath)
                .doesNotContain(Path.of("foo", "lib", "Library.java"));
    }

    private static CompiledProgram compileProgram(Program program, SortedSet<CompiledModule> libraries) {
        var result = CapybaraCompiler.INSTANCE.compile(program, libraries);
        if (result instanceof ValueOrError.Error<CompiledProgram> error) {
            fail(error.errors().toString());
        }
        return ((ValueOrError.Value<CompiledProgram>) result).value();
    }
}
