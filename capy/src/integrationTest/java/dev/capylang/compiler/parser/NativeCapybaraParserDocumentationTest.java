package dev.capylang.compiler.parser;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class NativeCapybaraParserDocumentationTest {
    @Test
    void shouldStoreFunctionalDocumentationComments() {
        var source = """
                /// Type docs
                data Box { value: String }

                /// Constant docs
                const DefaultValue: String = "value"

                /// First function line
                /// Second function line
                fun greet(): String = "hello"
                """;

        var module = parse(rawModule("Docs", "/sample", source, SourceKind.FUNCTIONAL));

        var data = module.definitions().stream()
                .filter(Definition.DataDeclaration.class::isInstance)
                .map(Definition.DataDeclaration.class::cast)
                .filter(declaration -> declaration.name().equals("Box"))
                .findFirst()
                .orElseThrow();
        assertThat(data.documentation()).containsExactly("Type docs");

        var constant = module.definitions().stream()
                .filter(Definition.ConstantDefinition.class::isInstance)
                .map(Definition.ConstantDefinition.class::cast)
                .map(Definition.ConstantDefinition::constant)
                .filter(declaration -> declaration.name().equals("DefaultValue"))
                .findFirst()
                .orElseThrow();
        assertThat(constant.documentation()).containsExactly("Constant docs");

        var function = module.definitions().stream()
                .filter(Definition.FunctionDefinition.class::isInstance)
                .map(Definition.FunctionDefinition.class::cast)
                .map(Definition.FunctionDefinition::function)
                .filter(declaration -> declaration.name().equals("greet"))
                .findFirst()
                .orElseThrow();
        assertThat(function.documentation()).containsExactly("First function line", "Second function line");
    }

    @Test
    void shouldStoreObjectOrientedDocumentationComments() {
        var source = """
                /// Class docs
                class Box {
                    /// Field docs
                    field value: String

                    /// Init docs
                    init {
                        return "ready"
                    }

                    /// Method docs
                    def get(): String = "value"
                }
                """;

        var module = parse(rawModule("Objects", "/sample", source, SourceKind.OBJECT_ORIENTED));

        var objectClass = module.objectOriented().classes().getFirst();
        assertThat(objectClass.documentation()).containsExactly("Class docs");
        assertThat(objectClass.fields().getFirst().documentation()).containsExactly("Field docs");
        assertThat(objectClass.initBlocks().getFirst().documentation()).containsExactly("Init docs");
        assertThat(objectClass.methods().getFirst().documentation()).containsExactly("Method docs");
    }

    private static ParsedModule parse(RawModule module) {
        return new NativeCapybaraParser().parse(List.of(module)).modules().getFirst();
    }

    private static RawModule rawModule(String name, String path, String input, SourceKind sourceKind) {
        return new RawModule(name, path, input, sourceKind);
    }

}
