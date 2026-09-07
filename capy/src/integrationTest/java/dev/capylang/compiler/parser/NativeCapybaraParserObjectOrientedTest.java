package dev.capylang.compiler.parser;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class NativeCapybaraParserObjectOrientedTest {
    @Test
    void shouldDefaultOmittedMethodReturnTypesToVoid() {
        var source = """
                interface UI {
                    def draw_field(game: Game)
                }

                trait Logging {
                    def log(message: String) = println(message)
                }

                class ConsoleUI: UI, Logging {
                    override def draw_field(game: Game) {
                        println(game.name())
                    }
                }

                class Game {
                    def name(): String = "game"
                }
                """;

        var module = parse(rawModule("Objects", "/sample", source, SourceKind.OBJECT_ORIENTED));

        assertThat(module.objectOriented().interfaces())
                .flatExtracting(ObjectOrientedInterface::methods)
                .extracting(method -> method.returnType().name())
                .containsExactly("void", "void");
        assertThat(module.objectOriented().classes())
                .flatExtracting(ObjectOrientedClass::methods)
                .filteredOn(method -> method.name().equals("draw_field"))
                .extracting(method -> method.returnType().name())
                .containsExactly("void");
    }

    private static ParsedModule parse(RawModule module) {
        return new NativeCapybaraParser().parse(List.of(module)).modules().getFirst();
    }

    private static RawModule rawModule(String name, String path, String input, SourceKind sourceKind) {
        return new RawModule(name, path, input, sourceKind);
    }
}
