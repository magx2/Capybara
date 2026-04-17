package dev.capylang.compiler;

import org.junit.jupiter.api.Test;
import dev.capylang.generator.GeneratedModule;
import dev.capylang.generator.JavaGenerator;
import dev.capylang.compiler.parser.RawModule;

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

    @Test
    void shouldCompileRegexOperatorsWhenRegexLibraryProvided() {
        var regexLibrarySource = """
                data Regex { pattern: string, flags: string }
                data Match { value: string }
                fun from_literal(pattern: string, flags: string): Regex = Regex { pattern, flags }
                fun Regex.matches(input: string): bool = input ? this.pattern
                fun Regex.find(input: string): Match =
                    if input ? this.pattern
                    then Match { value: this.pattern }
                    else Match { value: "" }
                fun Regex.find_all(input: string): list[Match] =
                    if input ? this.pattern
                    then [Match { value: this.pattern }]
                    else []
                fun Regex.replace(replacement: string): string => string = value => value.replace(this.pattern, replacement)
                fun Regex.split(input: string): list[string] = [input]
                fun Regex.`?`(input: string): bool = this.matches(input)
                fun Regex.`~`(input: string): Match = this.find(input)
                fun Regex.`~~`(input: string): list[Match] = this.find_all(input)
                fun Regex.`~>`(replacement: string): string => string = this.replace(replacement)
                fun Regex.`/>`(input: string): list[string] = [input]
                """;
        var libraries = compileProgram(List.of(new RawModule("Regex", "/capy/lang", regexLibrarySource)), new TreeSet<>()).modules();

        var consumerSource = """
                from /capy/lang/Regex import { * }
                fun matches_named(input: string): bool = regex/\\\\d+/.matches(input)
                fun matches_alias(input: string): bool = regex/\\\\d+/ ? input
                fun find_like(input: string): string = (regex/\\\\d+/ ~ input).value
                fun find_named(input: string): string = regex/\\\\d+/.find(input).value
                fun all_like(input: string): list[string] = (regex/\\\\d+/ ~~ input) | m => m.value
                fun find_all_named(input: string): list[string] = regex/\\\\d+/.find_all(input) | m => m.value
                fun replace_named(input: string): string = regex/\\\\d+/.replace("#")(input)
                fun redact(input: string): string = (regex/\\\\d+/ ~> "#")(input)
                fun split_named(input: string): list[string] = regex/,/.split(input)
                fun split_like(input: string): list[string] = regex/,/ /> input
                """;
        var generated = new JavaGenerator().generate(compileProgram(List.of(new RawModule("RegexConsumer", "/foo/app", consumerSource)), libraries));

        assertThat(generated.modules()).hasSize(1);
        var module = generated.modules().getFirst();
        assertThat(module.relativePath()).isEqualTo(Path.of("foo", "app", "RegexConsumer.java"));
        assertThat(module.code()).contains("fromLiteral(\"\\\\d+\", \"\")");
        assertThat(module.code()).contains("fromLiteral(\",\", \"\")");
    }

    private static CompiledProgram compileProgram(List<RawModule> rawModules, SortedSet<CompiledModule> libraries) {
        var result = CapybaraCompiler.INSTANCE.compile(rawModules, libraries);
        if (result instanceof Result.Error<CompiledProgram> error) {
            fail(error.errors().toString());
        }
        return ((Result.Success<CompiledProgram>) result).value();
    }
}
