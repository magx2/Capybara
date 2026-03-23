package pl.grzeslowski.capybara.compiler;

import org.junit.jupiter.api.Test;
import pl.grzeslowski.capybara.generator.GeneratedModule;
import pl.grzeslowski.capybara.generator.JavaGenerator;
import pl.grzeslowski.capybara.compiler.parser.RawModule;

import java.nio.file.Path;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.fail;

class CapybaraCompilerLibrariesIntegrationTest {
    @Test
    void shouldGenerateJavaReferencingLibraryWithoutEmittingIt() {
        var librarySource = """
                data Message { value: string }
                fun make_message(value: string): Message = Message { value: value }
                """;
        var libraries = compileProgram(List.of(new RawModule("Library", "/foo/lib", librarySource)), new TreeSet<>()).modules();

        var consumerSource = """
                from Library import { * }
                fun consume(value: string): string = make_message(value).value
                """;
        var generated = new JavaGenerator().generate(compileProgram(List.of(new RawModule("Consumer", "/foo/app", consumerSource)), libraries));

        assertThat(generated.modules()).hasSize(1);
        var module = generated.modules().getFirst();
        assertThat(module.relativePath()).isEqualTo(Path.of("foo", "app", "Consumer.java"));
        assertThat(module.code()).contains("import static foo.lib.Library.*;");
        assertThat(module.code()).contains("makeMessage(value)");
        assertThat(generated.modules())
                .extracting(GeneratedModule::relativePath)
                .doesNotContain(Path.of("foo", "lib", "Library.java"));
    }

    private static CompiledProgram compileProgram(List<RawModule> rawModules, SortedSet<CompiledModule> libraries) {
        var result = CapybaraCompiler.INSTANCE.compile(rawModules, libraries);
        if (result instanceof Result.Error<CompiledProgram> error) {
            fail(error.errors().toString());
        }
        return ((Result.Success<CompiledProgram>) result).value();
    }
}
