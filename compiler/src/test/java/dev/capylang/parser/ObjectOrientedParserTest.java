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
                            fun normalize(values: list[int]): int {
                                let scaled: list[int] = values | value => value * 2
                                if size(scaled) > 0 {
                                    return scaled[0]
                                } else {
                                    return match values with
                                    case _ -> 0
                                }
                            }
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

        var namedDeclaration = module.objectOriented().definitions().stream()
                .filter(ObjectOriented.TraitDeclaration.class::isInstance)
                .map(ObjectOriented.TraitDeclaration.class::cast)
                .findFirst()
                .orElseThrow();
        var normalizeMethod = namedDeclaration.members().stream()
                .filter(ObjectOriented.MethodDeclaration.class::isInstance)
                .map(ObjectOriented.MethodDeclaration.class::cast)
                .filter(method -> method.name().equals("normalize"))
                .findFirst()
                .orElseThrow();
        assertThat(normalizeMethod.body()).hasValueSatisfying(body -> {
            assertThat(body).isInstanceOf(ObjectOriented.StatementBlock.class);
            var block = (ObjectOriented.StatementBlock) body;
            assertThat(block.statements()).hasSize(2);
            assertThat(block.statements().getFirst()).isInstanceOf(ObjectOriented.LetStatement.class);
            assertThat(block.statements().get(1)).isInstanceOf(ObjectOriented.IfStatement.class);
        });
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

    @Test
    @DisplayName("should reject bare expression statements in method blocks")
    void rejectBareExpressionStatements() {
        var result = ObjectOrientedParser.INSTANCE.parseModule(new RawModule(
                "Broken",
                "/parser",
                """
                        class Broken {
                            fun run(): int {
                                foo()
                                return 1
                            }
                        }
                        """,
                SourceKind.OBJECT_ORIENTED
        ));

        assertThat(result).isInstanceOf(Result.Error.class);
        assertThat(((Result.Error<ObjectOrientedModule>) result).errors())
                .singleElement()
                .satisfies(error -> {
                    assertThat(error.file()).isEqualTo("/parser/Broken.coo");
                    assertThat(error.message()).contains("line 3");
                });
    }

    @Test
    @DisplayName("should parse expression-bodied and block-bodied return forms")
    void parseMethodReturns() {
        var result = ObjectOrientedParser.INSTANCE.parseModule(new RawModule(
                "Returns",
                "/parser",
                """
                        open class Base {
                            fun label(): string = "base"
                        }

                        class Returns: Base {
                            fun one(): int = 1

                            fun choose(x: int): int =
                                if x > 0 then x else -x

                            fun first(xs: list[int]): int {
                                let selected: int = xs[0]
                                return selected;
                            }

                            fun nested(flag: bool, xs: list[int]): int {
                                if flag {
                                    return match xs with
                                    case _ -> xs[0]
                                } else {
                                    {
                                        return 0
                                    }
                                }
                            }

                            fun call_parent(): string = super[Base].label()
                        }
                        """,
                SourceKind.OBJECT_ORIENTED
        ));

        assertThat(result).isInstanceOf(Result.Success.class);
        var module = ((Result.Success<ObjectOrientedModule>) result).value();
        var returnsClass = module.objectOriented().definitions().stream()
                .filter(ObjectOriented.ClassDeclaration.class::isInstance)
                .map(ObjectOriented.ClassDeclaration.class::cast)
                .filter(type -> type.name().equals("Returns"))
                .findFirst()
                .orElseThrow();

        var methods = returnsClass.members().stream()
                .filter(ObjectOriented.MethodDeclaration.class::isInstance)
                .map(ObjectOriented.MethodDeclaration.class::cast)
                .toList();

        assertThat(methods)
                .extracting(ObjectOriented.MethodDeclaration::name)
                .containsExactly("one", "choose", "first", "nested", "call_parent");
        assertThat(methods.stream()
                .filter(method -> method.name().equals("one"))
                .findFirst()
                .orElseThrow()
                .body())
                .hasValueSatisfying(body -> assertThat(body).isInstanceOf(ObjectOriented.ExpressionBody.class));
        assertThat(methods.stream()
                .filter(method -> method.name().equals("first"))
                .findFirst()
                .orElseThrow()
                .body())
                .hasValueSatisfying(body -> {
                    assertThat(body).isInstanceOf(ObjectOriented.StatementBlock.class);
                    var block = (ObjectOriented.StatementBlock) body;
                    assertThat(block.statements()).hasSize(2);
                    assertThat(block.statements().getFirst()).isInstanceOf(ObjectOriented.LetStatement.class);
                    assertThat(block.statements().get(1)).isInstanceOf(ObjectOriented.ReturnStatement.class);
                });
        assertThat(methods.stream()
                .filter(method -> method.name().equals("nested"))
                .findFirst()
                .orElseThrow()
                .body())
                .hasValueSatisfying(body -> {
                    assertThat(body).isInstanceOf(ObjectOriented.StatementBlock.class);
                    var block = (ObjectOriented.StatementBlock) body;
                    assertThat(block.statements()).singleElement().isInstanceOf(ObjectOriented.IfStatement.class);
                });
    }

    @Test
    @DisplayName("should reject trailing bare expression after return-oriented statements")
    void rejectTrailingBareExpression() {
        var result = ObjectOrientedParser.INSTANCE.parseModule(new RawModule(
                "Broken",
                "/parser",
                """
                        class Broken {
                            fun run(): int {
                                let value: int = 1
                                value
                            }
                        }
                        """,
                SourceKind.OBJECT_ORIENTED
        ));

        assertThat(result).isInstanceOf(Result.Error.class);
        assertThat(((Result.Error<ObjectOrientedModule>) result).errors())
                .singleElement()
                .satisfies(error -> {
                    assertThat(error.file()).isEqualTo("/parser/Broken.coo");
                    assertThat(error.message()).contains("line 4");
                });
    }

    @Test
    @DisplayName("should reject return without expression")
    void rejectReturnWithoutExpression() {
        var result = ObjectOrientedParser.INSTANCE.parseModule(new RawModule(
                "Broken",
                "/parser",
                """
                        class Broken {
                            fun run(): int {
                                return
                            }
                        }
                        """,
                SourceKind.OBJECT_ORIENTED
        ));

        assertThat(result).isInstanceOf(Result.Error.class);
        assertThat(((Result.Error<ObjectOrientedModule>) result).errors())
                .singleElement()
                .satisfies(error -> assertThat(error.file()).isEqualTo("/parser/Broken.coo"));
    }

    @Test
    @DisplayName("should reject if branches without statement blocks")
    void rejectIfWithoutStatementBlocks() {
        var result = ObjectOrientedParser.INSTANCE.parseModule(new RawModule(
                "Broken",
                "/parser",
                """
                        class Broken {
                            fun run(flag: bool): int {
                                if flag return 1
                                else {
                                    return 0
                                }
                            }
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
