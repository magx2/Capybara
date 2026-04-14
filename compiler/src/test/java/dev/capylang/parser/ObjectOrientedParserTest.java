package dev.capylang.parser;

import dev.capylang.compiler.Result;
import dev.capylang.compiler.parser.ObjectOriented;
import dev.capylang.compiler.parser.ObjectOrientedModule;
import dev.capylang.compiler.parser.ObjectOrientedParser;
import dev.capylang.compiler.parser.RawModule;
import dev.capylang.compiler.parser.SourceKind;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class ObjectOrientedParserTest {
    @Test
    @DisplayName("should parse class trait and interface declarations from .coo source")
    void parseObjectOrientedModule() {
        var result = ObjectOrientedParser.INSTANCE.parseModule(new RawModule(
                "User",
                "/parser",
                """
                        interface Printable {
                            fun print(): string
                        }

                        trait Named {
                            field name: string = "unknown"
                            fun display_name(): string = name
                        }

                        open class User(name: string): Named, Printable {
                            field name: string = name
                            override fun print(): string = "User(" + this.name + ")"
                        }
                        """,
                SourceKind.OBJECT_ORIENTED
        ));

        assertThat(result).isInstanceOf(Result.Success.class);
        var module = ((Result.Success<ObjectOrientedModule>) result).value();
        assertThat(module.sourceKind()).isEqualTo(SourceKind.OBJECT_ORIENTED);
        assertThat(module.objectOriented().definitions()).hasSize(3);
        assertThat(module.objectOriented().definitions())
                .extracting(ObjectOriented.TypeDeclaration::name)
                .containsExactly("Printable", "Named", "User");

        var userDeclaration = module.objectOriented().definitions().stream()
                .filter(ObjectOriented.ClassDeclaration.class::isInstance)
                .map(ObjectOriented.ClassDeclaration.class::cast)
                .findFirst()
                .orElseThrow();
        assertThat(userDeclaration.constructorParameters()).extracting(ObjectOriented.Parameter::name).containsExactly("name");
        assertThat(userDeclaration.parents()).extracting(ObjectOriented.TypeReference::name).containsExactly("Named", "Printable");
    }

    @Test
    @DisplayName("should report .coo file names in parser diagnostics")
    void reportObjectOrientedFileNameInSyntaxErrors() {
        var result = ObjectOrientedParser.INSTANCE.parseModule(new RawModule(
                "Broken",
                "/parser",
                """
                        class Broken {
                            fun print(): string =
                        }
                        """,
                SourceKind.OBJECT_ORIENTED
        ));

        assertThat(result).isInstanceOf(Result.Error.class);
        assertThat(((Result.Error<ObjectOrientedModule>) result).errors())
                .singleElement()
                .satisfies(error -> assertThat(error.file()).isEqualTo("/parser/Broken.coo"));
    }
}
