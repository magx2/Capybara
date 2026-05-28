package dev.capylang.compiler;

import dev.capylang.compiler.linking.ModuleLinkingPass;
import dev.capylang.compiler.parser.RawModule;
import org.junit.jupiter.api.Test;

import java.util.Collection;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import static org.assertj.core.api.Assertions.assertThat;

class ModuleLinkingPassParityTest {
    private static final String PASS_PROPERTY = "capybara.compiler.useCapybaraModuleLinkingPass";

    @Test
    void shouldMatchLegacyCompileOutputForNormalizedQualifiedImports() {
        var modules = List.of(
                new RawModule("Library", "/./foo//lib/", """
                        data Message { value: String }
                        fun make_message(value: String): Message = Message { value }
                        """),
                new RawModule("Consumer", "/foo/app", """
                        from /foo/lib/Library import { Message, make_message }

                        fun consume(value: String): Message = make_message(value)
                        fun unwrap(message: Message): String = message.value
                        """)
        );

        assertSameCompileResult(modules);
    }

    @Test
    void shouldMatchLegacyDiagnosticsForImportSelectionErrors() {
        var modules = List.of(
                new RawModule("Library", "/foo/lib", """
                        fun public_value(): int = 1
                        """),
                new RawModule("Consumer", "/foo/app", """
                        from /foo/lib/Library import { missing_value }

                        fun consume(): int = 1
                        """)
        );

        assertSameCompileResult(modules);
    }

    @Test
    void shouldExposePureSourceClassificationHelpers() {
        assertThat(ModuleLinkingPass.normalizeModulePath("/./foo//bar/")).isEqualTo("foo/bar");
        assertThat(ModuleLinkingPass.canonicalModuleIdentity("/./foo//bar/", "Baz")).isEqualTo("foo/bar/Baz");
        assertThat(ModuleLinkingPass.moduleKey("\\foo\\bar", "Baz")).isEqualTo("foo/bar/Baz");
        assertThat(ModuleLinkingPass.stripKnownExtension("Library.cfun")).isEqualTo("Library");
        assertThat(ModuleLinkingPass.stripKnownExtension("Widget.coo")).isEqualTo("Widget");
        assertThat(ModuleLinkingPass.moduleFile("FUNCTIONAL", "foo/bar", "Library")).isEqualTo("/foo/bar/Library.cfun");

        var sourceKind = ModuleLinkingPass.sourceKindFromFileName("Library.cfun");
        assertThat(sourceKind).isEqualTo(List.of(true, "FUNCTIONAL", ""));
    }

    @Test
    void shouldBuildDeterministicModuleAndDeclarationIndexes() {
        var firstShared = ModuleLinkingPass.moduleRef("Shared", "/foo/one");
        var secondShared = ModuleLinkingPass.moduleRef("Shared", "/foo/two");
        var library = ModuleLinkingPass.moduleRef("Library", "/foo/lib");
        var index = ModuleLinkingPass.buildModuleLinkIndexUnchecked(List.of(
                ModuleLinkingPass.moduleDescriptor(firstShared, List.of(), List.of("Shared"), "OTHER", true, List.of()),
                ModuleLinkingPass.moduleDescriptor(secondShared, List.of(), List.of("Shared"), "OTHER", true, List.of()),
                ModuleLinkingPass.moduleDescriptor(library, List.of(), List.of("Message"), "NONE", false, List.of())
        ));

        assertThat(entryKeys(tuple(index, 0))).containsExactly("Shared", "Library");
        assertThat(entryKeys(tuple(index, 1))).containsExactly("foo/one/Shared", "foo/two/Shared", "foo/lib/Library");
        assertThat(entryKeys(tuple(index, 2))).containsExactly("Library");
        assertThat(tuple(index, 3)).isEqualTo(List.of("Shared"));

        var descriptor = ModuleLinkingPass.moduleDescriptor(
                library,
                List.of(
                        ModuleLinkingPass.moduleMember("render", "LOCAL", "FUNCTION"),
                        ModuleLinkingPass.moduleMember("render", "PUBLIC", "FUNCTION"),
                        ModuleLinkingPass.moduleMember("Message", "PUBLIC", "TYPE")
                ),
                List.of(),
                "NONE",
                false,
                List.of()
        );
        var declarationIndexResult = ModuleLinkingPass.buildDeclarationIndex(descriptor);
        assertThat(declarationIndexResult.get(0)).isEqualTo(true);
        var declarationIndex = tuple(declarationIndexResult, 1);
        assertThat(entryKeys(tuple(declarationIndex, 0))).containsExactly("render");
        var functionEntry = tuple(tuple(declarationIndex, 0).getFirst());
        assertThat(tuple(functionEntry, 1)).hasSize(2);
        assertThat(entryKeys(tuple(declarationIndex, 1))).containsExactly("Message");
    }

    private static void assertSameCompileResult(List<RawModule> modules) {
        var capybara = compileWithPass(modules, true);
        var legacy = compileWithPass(modules, false);
        assertThat(resultSummary(capybara)).isEqualTo(resultSummary(legacy));
    }

    private static Result<CompiledProgram> compileWithPass(List<RawModule> modules, boolean enabled) {
        var previous = System.getProperty(PASS_PROPERTY);
        try {
            System.setProperty(PASS_PROPERTY, Boolean.toString(enabled));
            return CapybaraCompiler.INSTANCE.compile(modules, new TreeSet<>());
        } finally {
            if (previous == null) {
                System.clearProperty(PASS_PROPERTY);
            } else {
                System.setProperty(PASS_PROPERTY, previous);
            }
        }
    }

    private static Object resultSummary(Result<CompiledProgram> result) {
        if (result instanceof Result.Error<CompiledProgram> error) {
            return error.errors().stream()
                    .map(single -> single.file() + ":" + single.line() + ":" + single.column() + ":" + single.message())
                    .toList();
        }
        var program = ((Result.Success<CompiledProgram>) result).value();
        return program.modules().stream()
                .map(module -> new ModuleSummary(
                        module.name(),
                        module.path(),
                        module.types().keySet(),
                        module.functions().stream().map(CompiledFunction::name).toList(),
                        module.staticImports()
                ))
                .toList();
    }

    private static List<String> entryKeys(List<?> entries) {
        return entries.stream()
                .map(ModuleLinkingPassParityTest::tuple)
                .map(entry -> (String) entry.get(0))
                .toList();
    }

    private static List<?> tuple(List<?> tuple, int index) {
        return (List<?>) tuple.get(index);
    }

    private static List<?> tuple(Object value) {
        return (List<?>) value;
    }

    private record ModuleSummary(
            String name,
            String path,
            Collection<String> typeNames,
            List<String> functionNames,
            SortedSet<CompiledModule.StaticImport> staticImports
    ) {
    }
}
