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
                data Message { value: String }
                fun make_message(value: String): Message = Message { value: value }
                """;
        var libraries = compileProgram(List.of(new RawModule("Library", "/foo/lib", librarySource)), new TreeSet<>()).modules();

        var consumerSource = """
                from Library import { * }
                fun consume(value: String): String = make_message(value).value
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
                from /capy/lang/Option import { * }
                from /capy/lang/String import { * }

                data Regex { pattern: String, flags: String }
                data Match { group_value: String }
                fun from_literal(pattern: String, flags: String): Regex = Regex { pattern, flags }
                fun Regex.matches(input: String): bool = input ? this.pattern
                fun Regex.find(input: String): Match =
                    if input ? this.pattern
                    then Match { group_value: this.pattern }
                    else Match { group_value: "" }
                fun Regex.find_all(input: String): List[Match] =
                    if input ? this.pattern
                    then [Match { group_value: this.pattern }]
                    else []
                fun Regex.replace(replacement: String): String => String = value => value.replace(this.pattern, replacement)
                fun Regex.split(input: String): List[String] = [input]
                fun Regex.`?`(input: String): bool = this.matches(input)
                fun Regex.`~`(input: String): Match = this.find(input)
                fun Regex.`~~`(input: String): List[Match] = this.find_all(input)
                fun Regex.`~>`(replacement: String): String => String = this.replace(replacement)
                fun Regex.`/>`(input: String): List[String] = [input]
                fun Match.group(index: int): /capy/lang/Option[String] =
                    if index == 0 then /capy/lang/Option.Some { value: this.group_value } else /capy/lang/Option.None {}
                fun Match.groups(): List[/capy/lang/Option[String]] = [/capy/lang/Option.Some { value: this.group_value }]
                """;
        var libraries = compileProgram(List.of(
                optionModule(),
                stringModule(),
                collectionsModule(),
                new RawModule("Regex", "/capy/lang", regexLibrarySource)
        ), new TreeSet<>()).modules();

        var consumerSource = """
                from /capy/lang/Regex import { * }
                from /capy/lang/Option import { * }
                from /capy/collection/List import { * }
                from /capy/collection/Set import { * }
                from /capy/collection/Dict import { * }
                fun matches_named(input: String): bool = regex/\\\\d+/.matches(input)
                fun matches_alias(input: String): bool = regex/\\\\d+/ ? input
                fun find_like(input: String): String = (regex/\\\\d+/ ~ input).group_value
                fun find_named(input: String): String = regex/\\\\d+/.find(input).group_value
                fun all_like(input: String): List[String] = (regex/\\\\d+/ ~~ input) | m => m.group_value
                fun find_all_named(input: String): List[String] = regex/\\\\d+/.find_all(input) | m => m.group_value
                fun replace_named(input: String): String = regex/\\\\d+/.replace("#")(input)
                fun redact(input: String): String = (regex/\\\\d+/ ~> "#")(input)
                fun split_named(input: String): List[String] = regex/,/.split(input)
                fun split_like(input: String): List[String] = regex/,/ /> input
                fun first_group(input: String): String =
                    match regex/\\\\d+/.find(input).group(0) with
                    case Some { group } -> group
                    case None -> ""
                fun groups_count(input: String): int = regex/\\\\d+/.find(input).groups().size()
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

    private static RawModule optionModule() {
        return new RawModule("Option", "/capy/lang", """
                type Option[T] = Some[T] | None
                data Some[T] { value: T }
                single None
                """);
    }

    private static RawModule stringModule() {
        return new RawModule("String", "/capy/lang", """
                data String { <native> }
                fun String.`?`(part: String): bool = <native>
                fun String.replace(old: String, new: String): String = <native>
                """);
    }

    private static RawModule collectionsModule() {
        return new RawModule("List", "/capy/collection", """
                data List[T] { <native> }
                fun List[T].`|`(map: T => Y): List[Y] = <native>
                fun List[T].size(): int = <native>
                """);
    }
}
