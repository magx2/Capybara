package dev.capylang.compiler;

import dev.capylang.compiler.parser.RawModule;
import dev.capylang.compiler.parser.SourceKind;
import dev.capylang.compiler.expression.CompiledEffectBindExpression;
import dev.capylang.compiler.expression.CompiledObjectConstruction;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.TreeSet;

import static org.assertj.core.api.Assertions.assertThat;

class ObjectOrientedCompilerTest {
    @Test
    void shouldCompileObjectOrientedModules() {
        var result = CapybaraCompiler.INSTANCE.compile(List.of(
                new RawModule(
                        "User",
                        "/foo/boo",
                        """
                                class User {
                                    def greet(): String = "hello"
                                }
                                """,
                        SourceKind.OBJECT_ORIENTED
                )
        ), new TreeSet<>());

        assertThat(result).isInstanceOf(Result.Success.class);
        var program = ((Result.Success<CompiledProgram>) result).value();
        assertThat(program.modules()).isEmpty();
        assertThat(program.objectOrientedModules())
                .singleElement()
                .satisfies(module -> {
                    assertThat(module.name()).isEqualTo("User");
                    assertThat(module.path()).isEqualTo("/foo/boo");
                });
    }


    @Test
    void shouldAllowNestedCallsAsStandAloneStatements() {
        var result = CapybaraCompiler.INSTANCE.compile(List.of(
                new RawModule(
                        "User",
                        "/foo/boo",
                        """
                                class User {
                                    def format(name: String): String = name

                                    def print(value: String): void {
                                    }

                                    def greet(name: String): void {
                                        print(format(name))
                                    }
                                }
                                """,
                        SourceKind.OBJECT_ORIENTED
                )
        ), new TreeSet<>());

        assertThat(result).isInstanceOf(Result.Success.class);
    }

    @Test
    void shouldTypeImportedObjectConstructionAsEffect() {
        var result = CapybaraCompiler.INSTANCE.compile(List.of(
                effectModule(),
                constructiblesModule(),
                new RawModule(
                        "ObjectUse",
                        "/foo/app",
                        """
                                from /capy/lang/Effect import { * }
                                from Constructibles import { Person, Printable }

                                fun make_person(name: String): Effect[Person] =
                                    Person(name)

                                fun make_printable(name: String): Effect[Printable] =
                                    Person(name)

                                fun bind_person(name: String): Effect[Person] =
                                    let person <- Person(name)
                                    person
                                """
                )
        ), new TreeSet<>());

        assertThat(result).isInstanceOf(Result.Success.class);
        var program = ((Result.Success<CompiledProgram>) result).value();
        var makePerson = compiledFunction(program, "ObjectUse", "make_person");
        assertThat(makePerson.returnType()).isInstanceOfSatisfying(CompiledDataParentType.class, effect ->
                assertThat(effect.typeParameters()).containsExactly("Person"));
        assertThat(makePerson.expression()).isInstanceOf(CompiledObjectConstruction.class);

        var bindPerson = compiledFunction(program, "ObjectUse", "bind_person");
        assertThat(bindPerson.expression()).isInstanceOf(CompiledEffectBindExpression.class);
    }

    @Test
    void shouldRejectPureObjectConstruction() {
        var result = compileInvalid("""
                from Constructibles import { Person }

                fun bad(name: String): Person =
                    Person(name)
                """);

        assertThat(errorMessages(result))
                .anySatisfy(message -> assertThat(message)
                        .contains("Object construction of Person returns Effect[Person], but Person was expected"));
    }

    @Test
    void shouldRejectInvalidObjectConstructionTargetsAndArguments() {
        assertThat(errorMessages(compileInvalid("""
                fun bad(): any =
                    Missing()
                """))).anySatisfy(message -> assertThat(message)
                .contains("Object type `Missing` is not imported or not found"));

        assertThat(errorMessages(compileInvalid("""
                from Constructibles import { Person }

                fun bad(): Effect[Person] =
                    Person()
                """))).anySatisfy(message -> assertThat(message)
                .contains("Constructor `Person` expects 1 argument(s), got 0"));

        assertThat(errorMessages(compileInvalid("""
                from Constructibles import { Person }

                fun bad(): Effect[Person] =
                    Person(1)
                """))).anySatisfy(message -> assertThat(message)
                .contains("Expected `String`, got `int`"));

        assertThat(errorMessages(compileInvalid("""
                from Constructibles import { Printable }

                fun bad(): Effect[Printable] =
                    Printable()
                """))).anySatisfy(message -> assertThat(message)
                .contains("Object construction target `Printable` is a interface and is not constructible"));

        assertThat(errorMessages(compileInvalid("""
                from Constructibles import { NamedTrait }

                fun bad(): Effect[NamedTrait] =
                    NamedTrait()
                """))).anySatisfy(message -> assertThat(message)
                .contains("Object construction target `NamedTrait` is a trait and is not constructible"));

        assertThat(errorMessages(compileInvalid("""
                from Constructibles import { AbstractPerson }

                fun bad(): Effect[AbstractPerson] =
                    AbstractPerson()
                """))).anySatisfy(message -> assertThat(message)
                .contains("Object construction target `AbstractPerson` is a abstract class and is not constructible"));

        assertThat(errorMessages(compileInvalid("""
                from Constructibles import { ArrayBox }

                fun bad(): Effect[ArrayBox] =
                    ArrayBox([])
                """))).anySatisfy(message -> assertThat(message)
                .contains("Constructor parameter `names` uses OO-only type `String[]` that is not representable in .cfun"));
    }

    private Result<CompiledProgram> compileInvalid(String source) {
        return CapybaraCompiler.INSTANCE.compile(List.of(
                effectModule(),
                constructiblesModule(),
                new RawModule("InvalidObjectUse", "/foo/app", """
                        from /capy/lang/Effect import { * }
                        """ + source)
        ), new TreeSet<>());
    }

    private List<String> errorMessages(Result<CompiledProgram> result) {
        assertThat(result).isInstanceOf(Result.Error.class);
        return ((Result.Error<CompiledProgram>) result).errors().stream()
                .map(Result.Error.SingleError::message)
                .toList();
    }

    private CompiledFunction compiledFunction(CompiledProgram program, String moduleName, String functionName) {
        return program.modules().stream()
                .filter(module -> module.name().equals(moduleName))
                .flatMap(module -> module.functions().stream())
                .filter(function -> function.name().equals(functionName))
                .findFirst()
                .orElseThrow();
    }

    private RawModule effectModule() {
        return new RawModule("Effect", "/capy/lang", """
                union Effect[T] = UnsafeEffect[T]
                private data UnsafeEffect[T] { unsafe_thunk: () => T }
                """);
    }

    private RawModule constructiblesModule() {
        return new RawModule(
                "Constructibles",
                "/foo/app",
                """
                        interface Printable {
                            def label(): String
                        }

                        trait NamedTrait {
                            def label(): String = "trait"
                        }

                        abstract class AbstractPerson {
                        }

                        class Person(name: String): Printable {
                            field name: String = name

                            override def label(): String = this.name
                        }

                        class ArrayBox(names: String[]) {
                            field names: String[] = names
                        }
                        """,
                SourceKind.OBJECT_ORIENTED
        );
    }

}
