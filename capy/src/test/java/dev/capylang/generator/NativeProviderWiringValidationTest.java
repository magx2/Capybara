package dev.capylang.generator;

import dev.capylang.compiler.CompiledNativeProviderBinding;
import dev.capylang.compiler.CompiledNativeProviderDeclaration;
import dev.capylang.compiler.CompiledProgram;
import dev.capylang.compiler.NativeProviderBackendBinding;
import dev.capylang.compiler.NativeProviderCatalog;
import dev.capylang.compiler.NativeProviderManifest;
import dev.capylang.compiler.parser.ParserException;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThatCode;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class NativeProviderWiringValidationTest {
    private static final CompiledNativeProviderDeclaration DECLARATION = new CompiledNativeProviderDeclaration(
            "ui", "paper_soccer/ui", "UIProvider", "paper_soccer.ui.UI", "paper_soccer.ui.UI", "", "paper_soccer/ui/UIProvider.cfun"
    );

    @Test
    void acceptsBindingSuppliedByLookupProgram() {
        var source = program(List.of(DECLARATION), List.of());
        var lookup = program(List.of(), List.of(javaBinding(Optional.of("paper_soccer.ui.SwingUI"))));

        assertThatCode(() -> JavaGenerator.validateNativeProviderBindings(source, lookup, "java"))
                .doesNotThrowAnyException();
    }

    @Test
    void rejectsBackendRecordWithoutUsableImplementation() {
        var source = program(List.of(DECLARATION), List.of());
        var lookup = program(List.of(), List.of(javaBinding(Optional.empty())));

        assertThatThrownBy(() -> JavaGenerator.validateNativeProviderBindings(source, lookup, "java"))
                .isInstanceOf(ParserException.class)
                .hasMessageContaining("NotWired: No native provider registered for interface `paper_soccer.ui.UI`");
    }

    private static CompiledNativeProviderBinding javaBinding(Optional<String> className) {
        return new CompiledNativeProviderBinding(
                "paper_soccer.ui.UI",
                "",
                Optional.of(new NativeProviderBackendBinding(className, Optional.empty(), Optional.empty(), Optional.empty())),
                Optional.empty(),
                Optional.empty()
        );
    }

    private static CompiledProgram program(
            List<CompiledNativeProviderDeclaration> declarations,
            List<CompiledNativeProviderBinding> bindings
    ) {
        return new CompiledProgram(
                List.of(),
                List.of(),
                new NativeProviderManifest(List.of()),
                new NativeProviderCatalog(declarations, bindings)
        );
    }
}
