package pl.grzeslowski.capybara.compiler;

import org.junit.jupiter.api.Test;
import pl.grzeslowski.capybara.compiler.parser.RawModule;

import java.util.List;
import java.util.SortedSet;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.fail;

class CapybaraCompilerLibrariesTest {
    @Test
    void shouldCompileAgainstLibrariesWithoutIncludingThemInOutput() {
        var librarySource = """
                data Message { value: string }
                fun make_message(value: string): Message = Message { value: value }
                """;
        var libraries = compileProgram(List.of(new RawModule("Library", "/foo/lib", librarySource)), new java.util.TreeSet<>()).modules();

        var consumerSource = """
                from Library import { * }
                fun consume(value: string): Message = make_message(value)
                fun unwrap(message: Message): string = message.value
                """;
        var compiled = compileProgram(List.of(new RawModule("Consumer", "/foo/app", consumerSource)), libraries);

        assertThat(compiled.modules()).extracting(CompiledModule::name).containsExactly("Consumer");
        assertThat(compiled.modules().first().functions())
                .extracting(CompiledFunction::name)
                .containsExactlyInAnyOrder("consume", "unwrap");
    }

    private static CompiledProgram compileProgram(List<RawModule> rawModules, SortedSet<CompiledModule> libraries) {
        var result = CapybaraCompiler.INSTANCE.compile(rawModules, libraries);
        if (result instanceof Result.Error<CompiledProgram> error) {
            fail(error.errors().toString());
        }
        return ((Result.Success<CompiledProgram>) result).value();
    }
}
