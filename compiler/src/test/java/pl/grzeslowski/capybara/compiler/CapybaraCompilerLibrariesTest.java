package pl.grzeslowski.capybara.compiler;

import org.junit.jupiter.api.Test;
import pl.grzeslowski.capybara.parser.CapybaraParser;
import pl.grzeslowski.capybara.parser.Module;
import pl.grzeslowski.capybara.parser.Program;

import java.util.List;
import java.util.SortedSet;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.fail;

class CapybaraCompilerLibrariesTest {
    @Test
    void shouldCompileAgainstLibrariesWithoutIncludingThemInOutput() {
        var libraries = compileProgram(new Program(List.of(new Module(
                "Library",
                "/foo/lib",
                CapybaraParser.INSTANCE.parseFunctional("Library", "/foo/lib", """
                        data Message { value: string }
                        fun make_message(value: string): Message = Message { value: value }
                        """).functional()
        ))), new java.util.TreeSet<>()).modules();

        var consumer = new Module(
                "Consumer",
                "/foo/app",
                CapybaraParser.INSTANCE.parseFunctional("Consumer", "/foo/app", """
                        fun consume(value: string): Message = make_message(value)
                        fun unwrap(message: Message): string = message.value
                        """).functional(),
                List.of(new ImportDeclaration("Library", List.of("*"), List.of()))
        );

        var compiled = compileProgram(new Program(List.of(consumer)), libraries);

        assertThat(compiled.modules()).extracting(CompiledModule::name).containsExactly("Consumer");
        assertThat(compiled.modules().first().functions())
                .extracting(CompiledFunction::name)
                .containsExactlyInAnyOrder("consume", "unwrap");
    }

    private static CompiledProgram compileProgram(Program program, SortedSet<CompiledModule> libraries) {
        var result = CapybaraCompiler.INSTANCE.compile(program, libraries);
        if (result instanceof ValueOrError.Error<CompiledProgram> error) {
            fail(error.errors().toString());
        }
        return ((ValueOrError.Value<CompiledProgram>) result).value();
    }
}

