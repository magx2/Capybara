package dev.capylang.compiler.parser;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThatThrownBy;

class NativeCapybaraParserErrorTest {
    @Test
    void shouldExplainThatKeywordCannotBeUsedAsParameterName() {
        var source = """
                interface UI {
                    def draw_field(field: Field): Unit
                }
                """;
        var module = new RawModule("UI", "paper-soccer/ui", source, SourceKind.OBJECT_ORIENTED);

        assertThatThrownBy(() -> new NativeCapybaraParser().parse(List.of(module)))
                .isInstanceOf(ParserException.class)
                .hasMessage(
                        "paper-soccer/ui/UI.coo:2:19: ParserError: "
                                + "keyword 'field' cannot be used as an identifier; choose a different name"
                );
    }

    @Test
    void shouldExplainThatKeywordCannotBeUsedAsMethodName() {
        var source = """
                interface UI {
                    def field(): Unit
                }
                """;
        var module = new RawModule("UI", "paper-soccer/ui", source, SourceKind.OBJECT_ORIENTED);

        assertThatThrownBy(() -> new NativeCapybaraParser().parse(List.of(module)))
                .isInstanceOf(ParserException.class)
                .hasMessage(
                        "paper-soccer/ui/UI.coo:2:8: ParserError: "
                                + "keyword 'field' cannot be used as an identifier; choose a different name"
                );
    }
}
