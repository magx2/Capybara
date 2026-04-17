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
                        /// Printable contract
                        interface Printable {
                            def print(): string
                        }

                        trait Named {
                            field name: string = "unknown"
                            def display_name(): string = name
                            def normalize(values: list[int]): int {
                                let scaled: list[int] = values | value => value * 2
                                if size(scaled) > 0 {
                                    return scaled[0]
                                } else {
                                    return match values with
                                    case _ -> 0
                                }
                            }
                        }

                        /// User type
                        open class User(name: string): Named, Printable {
                            field name: string = name
                            override def print(): string = "User(" + this.name + ")"
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
        assertThat(((ObjectOriented.InterfaceDeclaration) module.objectOriented().definitions().getFirst()).comments())
                .containsExactly("Printable contract");

        var userDeclaration = module.objectOriented().definitions().stream()
                .filter(ObjectOriented.ClassDeclaration.class::isInstance)
                .map(ObjectOriented.ClassDeclaration.class::cast)
                .findFirst()
                .orElseThrow();
        assertThat(userDeclaration.comments()).containsExactly("User type");
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
    @DisplayName("should parse doc comments for types methods and local methods")
    void parseDocComments() {
        var result = ObjectOrientedParser.INSTANCE.parseModule(new RawModule(
                "Docs",
                "/parser",
                """
                        /// User type
                        class Docs {
                            /// Greets user
                            def greet(name: string): string {
                                /// Internal formatter
                                def format(value: string): string = "Hello " + value
                                return format(name)
                            }
                        }
                        """,
                SourceKind.OBJECT_ORIENTED
        ));

        assertThat(result).isInstanceOf(Result.Success.class);
        var module = ((Result.Success<ObjectOrientedModule>) result).value();
        var docsClass = (ObjectOriented.ClassDeclaration) module.objectOriented().definitions().getFirst();
        assertThat(docsClass.comments()).containsExactly("User type");
        var greetMethod = docsClass.members().stream()
                .filter(ObjectOriented.MethodDeclaration.class::isInstance)
                .map(ObjectOriented.MethodDeclaration.class::cast)
                .findFirst()
                .orElseThrow();
        assertThat(greetMethod.comments()).containsExactly("Greets user");
        var block = (ObjectOriented.StatementBlock) greetMethod.body().orElseThrow();
        var localMethod = (ObjectOriented.LocalMethodStatement) block.statements().getFirst();
        assertThat(localMethod.comments()).containsExactly("Internal formatter");
    }

    @Test
    @DisplayName("should report .coo file names in parser diagnostics")
    void reportObjectOrientedFileNameInSyntaxErrors() {
        var result = ObjectOrientedParser.INSTANCE.parseModule(new RawModule(
                "Broken",
                "/parser",
                """
                        class Broken {
                            def print(): string =
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
    @DisplayName("should reject fun keyword for OO methods")
    void rejectFunKeywordForMethods() {
        var result = ObjectOrientedParser.INSTANCE.parseModule(new RawModule(
                "Broken",
                "/parser",
                """
                        class Broken {
                            fun print(): string = "nope"
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
    @DisplayName("should reject super bracket parent call syntax")
    void rejectSuperBracketParentCalls() {
        var result = ObjectOrientedParser.INSTANCE.parseModule(new RawModule(
                "Broken",
                "/parser",
                """
                        open class Base {
                            def label(): string = "base"
                        }

                        class Broken: Base {
                            def print(): string = super[Base].label()
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
    @DisplayName("should parse call statements in method blocks")
    void parseCallStatements() {
        var result = ObjectOrientedParser.INSTANCE.parseModule(new RawModule(
                "Calls",
                "/parser",
                """
                        from /capy/io import { Stdout }

                        class Calls {
                            def run(): void {
                                foo()
                                Stdout.println("hi")
                                this.log("done")
                            }

                            def log(message: string): void = Stdout.print(message)
                        }
                        """,
                SourceKind.OBJECT_ORIENTED
        ));

        assertThat(result).isInstanceOf(Result.Success.class);
        var module = ((Result.Success<ObjectOrientedModule>) result).value();
        var callsClass = (ObjectOriented.ClassDeclaration) module.objectOriented().definitions().getFirst();
        var runMethod = callsClass.members().stream()
                .filter(ObjectOriented.MethodDeclaration.class::isInstance)
                .map(ObjectOriented.MethodDeclaration.class::cast)
                .filter(method -> method.name().equals("run"))
                .findFirst()
                .orElseThrow();
        var block = (ObjectOriented.StatementBlock) runMethod.body().orElseThrow();
        assertThat(block.statements())
                .extracting(Object::getClass)
                .containsExactly(
                        ObjectOriented.ExpressionStatement.class,
                        ObjectOriented.ExpressionStatement.class,
                        ObjectOriented.ExpressionStatement.class
                );
    }

    @Test
    @DisplayName("should reject non-call expression statements in method blocks")
    void rejectNonCallExpressionStatements() {
        var result = ObjectOrientedParser.INSTANCE.parseModule(new RawModule(
                "Broken",
                "/parser",
                """
                        class Broken {
                            def run(): int {
                                let value: int = 1
                                value
                                return value
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
                    assertThat(error.message()).contains("line 5");
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
                            def label(): string = "base"
                        }

                        class Returns: Base {
                            def one(): int = 1

                            def choose(x: int): int =
                                if x > 0 then x else -x

                            def first(xs: list[int]): int {
                                let selected: int = xs[0]
                                return selected;
                            }

                            def nested(flag: bool, xs: list[int]): int {
                                if flag {
                                    return match xs with
                                    case _ -> xs[0]
                                } else {
                                    {
                                        return 0
                                    }
                                }
                            }

                            def call_parent(): string = Base.label()
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
    @DisplayName("should parse mutable local declarations and assignments")
    void parseMutableLocalDeclarations() {
        var result = ObjectOrientedParser.INSTANCE.parseModule(new RawModule(
                "MutableLocals",
                "/parser",
                """
                        class MutableLocals {
                            def mutable(): string {
                                def x = "a"
                                x = "2"
                                return x
                            }
                        }
                        """,
                SourceKind.OBJECT_ORIENTED
        ));

        assertThat(result).isInstanceOf(Result.Success.class);
        var module = ((Result.Success<ObjectOrientedModule>) result).value();
        var mutableMethod = module.objectOriented().definitions().stream()
                .filter(ObjectOriented.ClassDeclaration.class::isInstance)
                .map(ObjectOriented.ClassDeclaration.class::cast)
                .flatMap(type -> type.members().stream())
                .filter(ObjectOriented.MethodDeclaration.class::isInstance)
                .map(ObjectOriented.MethodDeclaration.class::cast)
                .filter(method -> method.name().equals("mutable"))
                .findFirst()
                .orElseThrow();

        assertThat(mutableMethod.body()).hasValueSatisfying(body -> {
            assertThat(body).isInstanceOf(ObjectOriented.StatementBlock.class);
            var block = (ObjectOriented.StatementBlock) body;
            assertThat(block.statements()).hasSize(3);
            assertThat(block.statements().get(0)).isInstanceOf(ObjectOriented.MutableVariableStatement.class);
            assertThat(block.statements().get(1)).isInstanceOf(ObjectOriented.AssignmentStatement.class);
            assertThat(block.statements().get(2)).isInstanceOf(ObjectOriented.ReturnStatement.class);
        });
    }

    @Test
    @DisplayName("should parse local methods inside method blocks")
    void parseLocalMethods() {
        var result = ObjectOrientedParser.INSTANCE.parseModule(new RawModule(
                "LocalMethods",
                "/parser",
                """
                        class LocalMethods {
                            def parity(x: int): bool {
                                def is_even(n: int): bool {
                                    if n == 0 {
                                        return true
                                    } else {
                                        return is_odd(n - 1)
                                    }
                                }
                                def is_odd(n: int): bool {
                                    if n == 0 {
                                        return false
                                    } else {
                                        return is_even(n - 1)
                                    }
                                }
                                return is_even(x)
                            }
                        }
                        """,
                SourceKind.OBJECT_ORIENTED
        ));

        assertThat(result).isInstanceOf(Result.Success.class);
        var module = ((Result.Success<ObjectOrientedModule>) result).value();
        var parityMethod = module.objectOriented().definitions().stream()
                .filter(ObjectOriented.ClassDeclaration.class::isInstance)
                .map(ObjectOriented.ClassDeclaration.class::cast)
                .flatMap(type -> type.members().stream())
                .filter(ObjectOriented.MethodDeclaration.class::isInstance)
                .map(ObjectOriented.MethodDeclaration.class::cast)
                .filter(method -> method.name().equals("parity"))
                .findFirst()
                .orElseThrow();

        assertThat(parityMethod.body()).hasValueSatisfying(body -> {
            assertThat(body).isInstanceOf(ObjectOriented.StatementBlock.class);
            var block = (ObjectOriented.StatementBlock) body;
            assertThat(block.statements()).hasSize(3);
            assertThat(block.statements().get(0)).isInstanceOf(ObjectOriented.LocalMethodStatement.class);
            assertThat(block.statements().get(1)).isInstanceOf(ObjectOriented.LocalMethodStatement.class);
            assertThat(block.statements().get(2)).isInstanceOf(ObjectOriented.ReturnStatement.class);
        });
    }

    @Test
    @DisplayName("should parse loop statements in method blocks")
    void parseLoopStatements() {
        var result = ObjectOrientedParser.INSTANCE.parseModule(new RawModule(
                "Loops",
                "/parser",
                """
                        class Loops {
                            def first_positive(values: list[int]): int {
                                for value in values {
                                    if value > 0 {
                                        return value
                                    }
                                }
                                return 0
                            }

                            def first_large(values: list[int]): int {
                                foreach value: int in values {
                                    if value > 10 {
                                        return value
                                    }
                                }
                                return 0
                            }

                            def while_flag(flag: bool): int {
                                while flag {
                                    return 1
                                }
                                return 0
                            }

                            def do_once(flag: bool): int {
                                do {
                                    if flag {
                                        return 1
                                    }
                                    return 2
                                } while false
                            }
                        }
                        """,
                SourceKind.OBJECT_ORIENTED
        ));

        assertThat(result).isInstanceOf(Result.Success.class);
        var module = ((Result.Success<ObjectOrientedModule>) result).value();
        var loopsClass = (ObjectOriented.ClassDeclaration) module.objectOriented().definitions().getFirst();
        var methods = loopsClass.members().stream()
                .filter(ObjectOriented.MethodDeclaration.class::isInstance)
                .map(ObjectOriented.MethodDeclaration.class::cast)
                .toList();

        assertThat(((ObjectOriented.StatementBlock) methods.get(0).body().orElseThrow()).statements().getFirst())
                .isInstanceOf(ObjectOriented.ForEachStatement.class);
        assertThat(((ObjectOriented.StatementBlock) methods.get(1).body().orElseThrow()).statements().getFirst())
                .isInstanceOf(ObjectOriented.ForEachStatement.class);
        assertThat(((ObjectOriented.StatementBlock) methods.get(2).body().orElseThrow()).statements().getFirst())
                .isInstanceOf(ObjectOriented.WhileStatement.class);
        assertThat(((ObjectOriented.StatementBlock) methods.get(3).body().orElseThrow()).statements().getFirst())
                .isInstanceOf(ObjectOriented.DoWhileStatement.class);
    }

    @Test
    @DisplayName("should parse throw and try catch statements")
    void parseExceptionStatements() {
        var result = ObjectOrientedParser.INSTANCE.parseModule(new RawModule(
                "Exceptions",
                "/parser",
                """
                        class Exceptions {
                            def recover(flag: bool): string {
                                try {
                                    if flag {
                                        throw "boom"
                                    }
                                    return "ok"
                                } catch error {
                                    return error.getMessage()
                                }
                            }
                        }
                        """,
                SourceKind.OBJECT_ORIENTED
        ));

        assertThat(result).isInstanceOf(Result.Success.class);
        var module = ((Result.Success<ObjectOrientedModule>) result).value();
        var recoverMethod = module.objectOriented().definitions().stream()
                .filter(ObjectOriented.ClassDeclaration.class::isInstance)
                .map(ObjectOriented.ClassDeclaration.class::cast)
                .flatMap(type -> type.members().stream())
                .filter(ObjectOriented.MethodDeclaration.class::isInstance)
                .map(ObjectOriented.MethodDeclaration.class::cast)
                .filter(method -> method.name().equals("recover"))
                .findFirst()
                .orElseThrow();

        assertThat(recoverMethod.body()).hasValueSatisfying(body -> {
            assertThat(body).isInstanceOf(ObjectOriented.StatementBlock.class);
            var block = (ObjectOriented.StatementBlock) body;
            assertThat(block.statements()).singleElement().isInstanceOf(ObjectOriented.TryCatchStatement.class);
            var tryCatch = (ObjectOriented.TryCatchStatement) block.statements().getFirst();
            assertThat(tryCatch.tryBlock().statements()).hasSize(2);
            assertThat(tryCatch.tryBlock().statements().getFirst()).isInstanceOf(ObjectOriented.IfStatement.class);
            assertThat(tryCatch.tryBlock().statements().get(1)).isInstanceOf(ObjectOriented.ReturnStatement.class);
            assertThat(tryCatch.catches()).singleElement().satisfies(catchClause -> {
                assertThat(catchClause.name()).isEqualTo("error");
                assertThat(catchClause.body().statements()).singleElement().isInstanceOf(ObjectOriented.ReturnStatement.class);
            });
        });
    }

    @Test
    @DisplayName("should parse array types in fields and method signatures")
    void parseArrayTypes() {
        var result = ObjectOrientedParser.INSTANCE.parseModule(new RawModule(
                "Arrays",
                "/parser",
                """
                        class Arrays(values: string[], ids: int[]) {
                            field values: string[] = values
                            field ids: int[] = ids

                            def second_name(input: string[]): string = input[1]
                            def first_id(input: int[]): int = input[0]
                            def copy_people(input: Person[]): Person[] = input
                        }

                        class Person(name: string) {
                            field name: string = name
                        }
                        """,
                SourceKind.OBJECT_ORIENTED
        ));

        assertThat(result).isInstanceOf(Result.Success.class);
        var module = ((Result.Success<ObjectOrientedModule>) result).value();
        var arrays = module.objectOriented().definitions().stream()
                .filter(ObjectOriented.ClassDeclaration.class::isInstance)
                .map(ObjectOriented.ClassDeclaration.class::cast)
                .filter(type -> type.name().equals("Arrays"))
                .findFirst()
                .orElseThrow();

        assertThat(arrays.constructorParameters())
                .extracting(ObjectOriented.Parameter::type)
                .containsExactly("string[]", "int[]");
        assertThat(arrays.members().stream()
                .filter(ObjectOriented.FieldDeclaration.class::isInstance)
                .map(ObjectOriented.FieldDeclaration.class::cast)
                .map(ObjectOriented.FieldDeclaration::type)
                .toList())
                .containsExactly("string[]", "int[]");
        assertThat(arrays.members().stream()
                .filter(ObjectOriented.MethodDeclaration.class::isInstance)
                .map(ObjectOriented.MethodDeclaration.class::cast)
                .map(ObjectOriented.MethodDeclaration::returnType)
                .toList())
                .containsExactly("string", "int", "Person[]");
    }

    @Test
    @DisplayName("should parse array creation expressions")
    void parseArrayCreationExpressions() {
        var result = ObjectOrientedParser.INSTANCE.parseModule(new RawModule(
                "ArrayCreation",
                "/parser",
                """
                        class ArrayCreation {
                            def names(): string[] = string[]{"zero", "one"}

                            def slots(size: int): int[] = int[size]
                        }
                        """,
                SourceKind.OBJECT_ORIENTED
        ));

        assertThat(result).isInstanceOf(Result.Success.class);
        var module = ((Result.Success<ObjectOrientedModule>) result).value();
        var methods = module.objectOriented().definitions().stream()
                .filter(ObjectOriented.ClassDeclaration.class::isInstance)
                .map(ObjectOriented.ClassDeclaration.class::cast)
                .flatMap(type -> type.members().stream())
                .filter(ObjectOriented.MethodDeclaration.class::isInstance)
                .map(ObjectOriented.MethodDeclaration.class::cast)
                .toList();

        assertThat(methods)
                .extracting(ObjectOriented.MethodDeclaration::name)
                .containsExactly("names", "slots");
        assertThat(methods.get(0).body()).hasValueSatisfying(body ->
                assertThat(((ObjectOriented.ExpressionBody) body).expression()).isEqualTo("string[]{\"zero\",\"one\"}"));
        assertThat(methods.get(1).body()).hasValueSatisfying(body ->
                assertThat(((ObjectOriented.ExpressionBody) body).expression()).isEqualTo("int[size]"));
    }

    @Test
    @DisplayName("should reject return without expression")
    void rejectReturnWithoutExpression() {
        var result = ObjectOrientedParser.INSTANCE.parseModule(new RawModule(
                "Broken",
                "/parser",
                """
                        class Broken {
                            def run(): int {
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
    @DisplayName("should reject try without catch clauses")
    void rejectTryWithoutCatch() {
        var result = ObjectOrientedParser.INSTANCE.parseModule(new RawModule(
                "Broken",
                "/parser",
                """
                        class Broken {
                            def run(): string {
                                try {
                                    throw "boom"
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

    @Test
    @DisplayName("should reject if branches without statement blocks")
    void rejectIfWithoutStatementBlocks() {
        var result = ObjectOrientedParser.INSTANCE.parseModule(new RawModule(
                "Broken",
                "/parser",
                """
                        class Broken {
                            def run(flag: bool): int {
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

    @Test
    @DisplayName("should reject while body without statement block")
    void rejectWhileWithoutStatementBlock() {
        var result = ObjectOrientedParser.INSTANCE.parseModule(new RawModule(
                "Broken",
                "/parser",
                """
                        class Broken {
                            def run(flag: bool): int {
                                while flag return 1
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
