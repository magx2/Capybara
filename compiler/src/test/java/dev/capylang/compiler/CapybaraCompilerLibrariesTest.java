package dev.capylang.compiler;

import dev.capylang.compiler.expression.CompiledFunctionCall;
import dev.capylang.compiler.expression.CompiledFieldAccess;
import dev.capylang.compiler.expression.CompiledLetExpression;
import dev.capylang.compiler.expression.CompiledMatchExpression;
import dev.capylang.compiler.expression.CompiledNewData;
import dev.capylang.compiler.expression.CompiledReflectionValue;
import dev.capylang.compiler.parser.RawModule;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.SortedSet;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.fail;

class CapybaraCompilerLibrariesTest {
    @Test
    void shouldReturnErrorInsteadOfThrowingForSyntaxError() {
        var result = CapybaraCompiler.INSTANCE.compile(
                List.of(new RawModule("Broken", "/foo/app", "fun broken(): int = if true then {")),
                new java.util.TreeSet<>()
        );

        assertThat(result).isInstanceOf(Result.Error.class);
    }

    @Test
    void shouldCompileAgainstLibrariesWithoutIncludingThemInOutput() {
        var librarySource = """
                data Message { value: String }
                fun make_message(value: String): Message = Message { value: value }
                """;
        var libraries = compileProgram(List.of(new RawModule("Library", "/foo/lib", librarySource)), new java.util.TreeSet<>()).modules();

        var consumerSource = """
                from Library import { * }
                fun consume(value: String): Message = make_message(value)
                fun unwrap(message: Message): String = message.value
                """;
        var compiled = compileProgram(List.of(new RawModule("Consumer", "/foo/app", consumerSource)), libraries);

        assertThat(compiled.modules()).extracting(CompiledModule::name).containsExactly("Consumer");
        assertThat(compiled.modules().first().functions())
                .extracting(CompiledFunction::name)
                .containsExactlyInAnyOrder("consume", "unwrap");
    }

    @Test
    void shouldUseImportedDeriverWithDeriverModuleImports() {
        var reflectionLibraries = compileProgram(List.of(reflectionMetadataModule()), new java.util.TreeSet<>()).modules();
        var serdeLibraries = compileProgram(List.of(new RawModule("Serde", "/foo/lib", """
                from /capy/meta_prog/Reflection import { DataValueInfo, reflection }

                deriver TypeName {
                    fun type_name(): String =
                        let info: DataValueInfo = reflection(receiver)
                        info.name
                }
                """)), reflectionLibraries).modules();
        var libraries = new java.util.TreeSet<CompiledModule>();
        libraries.addAll(reflectionLibraries);
        libraries.addAll(serdeLibraries);

        var compiled = compileProgram(List.of(new RawModule("Consumer", "/foo/app", """
                from /foo/lib/Serde import { TypeName }

                data User { name: String } derive TypeName

                fun render(): String = User { name: "Ada" }.type_name()
                """)), libraries);

        assertThat(compiled.modules().first().functions())
                .extracting(CompiledFunction::name)
                .contains("__method__User__type_name", "render");
        assertThat(compiledFunction(compiled, "Consumer", "__method__User__type_name").expression())
                .isInstanceOfSatisfying(CompiledLetExpression.class, let -> {
                    assertThat(let.value()).isInstanceOf(CompiledReflectionValue.class);
                    assertThat(let.declaredType().orElseThrow().name()).endsWith("DataValueInfo");
                });
    }

    @Test
    void shouldExpandDeriverIntoGeneratedTypeMethod() {
        var libraries = compileProgram(List.of(reflectionMetadataModule()), new java.util.TreeSet<>()).modules();
        var compiled = compileProgram(List.of(new RawModule("Consumer", "/foo/app", """
                from /capy/meta_prog/Reflection import { DataValueInfo, reflection }

                deriver Show {
                    fun show(): String =
                        let info: DataValueInfo = reflection(receiver)
                        let body: String = info.fields |> info.name + " { ", (acc, field) =>
                            acc + (if acc == info.name + " { " then "" else ", ") + field.name
                        body + " }"
                }

                data AgeLimit { value: int }

                deriver AgeComparable {
                    fun bigger_than(i: int): bool = receiver.age > i

                    fun matches_age_metadata(extra: int, limit: AgeLimit): bool =
                        let info: DataValueInfo = reflection(receiver)
                        let has_name_field: bool = info.fields |any? field => field.name == "name"
                        let has_age_field: bool = info.fields |any? field => field.name == "age"
                        (info.name == "User") & has_name_field & has_age_field & (receiver.age + extra > limit.value)
                }

                data User { name: String, age: int } derive Show, AgeComparable
                type Named { id: String } = Person derive Show
                data Person { id: String, name: String }

                fun render(): String = User { name: "Ada", age: 42 }.show()
                fun older_than(i: int): bool = User { name: "Ada", age: 42 }.bigger_than(i)
                fun mixed_parameters(extra: int, limit: AgeLimit): bool =
                    User { name: "Ada", age: 42 }.matches_age_metadata(extra, limit)
                fun render_named(named: Named): String = named.show()
                """)), libraries);

        assertThat(compiled.modules().first().functions())
                .extracting(CompiledFunction::name)
                .contains("__method__User__show", "__method__User__bigger_than",
                        "__method__User__matches_age_metadata", "__method__Named__show",
                        "render", "older_than", "mixed_parameters", "render_named");
        assertThat(compiledFunction(compiled, "Consumer", "__method__User__show").returnType())
                .isEqualTo(PrimitiveLinkedType.STRING);
        assertThat(compiledFunction(compiled, "Consumer", "__method__Named__show").returnType())
                .isEqualTo(PrimitiveLinkedType.STRING);
        var biggerThan = compiledFunction(compiled, "Consumer", "__method__User__bigger_than");
        assertThat(biggerThan.parameters()).extracting(CompiledFunction.CompiledFunctionParameter::name)
                .containsExactly("this", "i");
        assertThat(biggerThan.parameters()).extracting(CompiledFunction.CompiledFunctionParameter::type)
                .containsExactly(compiledFunction(compiled, "Consumer", "__method__User__show").parameters().getFirst().type(),
                        PrimitiveLinkedType.INT);
        assertThat(biggerThan.returnType()).isEqualTo(PrimitiveLinkedType.BOOL);
        var mixedParameters = compiledFunction(compiled, "Consumer", "__method__User__matches_age_metadata");
        assertThat(mixedParameters.parameters()).extracting(CompiledFunction.CompiledFunctionParameter::name)
                .containsExactly("this", "extra", "limit");
        assertThat(mixedParameters.parameters()).extracting(parameter -> parameter.type().name())
                .containsExactly("User", "INT", "AgeLimit");
        assertThat(mixedParameters.returnType()).isEqualTo(PrimitiveLinkedType.BOOL);
    }

    @Test
    void shouldResolveUserDefinedIndexAccessAsGetMethod() {
        var compiled = compileProgram(List.of(new RawModule("Consumer", "/foo/app", """
                data Box { value: String }
                data Bag { age: double }
                data Cell { name: String }
                data Matrix { cell: Cell }

                fun Box.get(idx: int): String = this.value
                fun Bag.get(key: String): double = this.age
                fun Matrix.get(row: int, col: int): Cell = this.cell

                fun read_box(box: Box): String = box[1]
                fun read_bag(bag: Bag): double = bag["age"]
                fun read_cell(matrix: Matrix): String = matrix[1, 2].name
                """)), new java.util.TreeSet<>());

        assertThat(compiledFunction(compiled, "Consumer", "read_box").expression())
                .isInstanceOfSatisfying(CompiledFunctionCall.class, call -> {
                    assertThat(call.name()).isEqualTo("__method__Box__get");
                    assertThat(call.arguments()).hasSize(2);
                    assertThat(call.returnType()).isEqualTo(PrimitiveLinkedType.STRING);
                });
        assertThat(compiledFunction(compiled, "Consumer", "read_bag").expression())
                .isInstanceOfSatisfying(CompiledFunctionCall.class, call -> {
                    assertThat(call.name()).isEqualTo("__method__Bag__get");
                    assertThat(call.arguments()).hasSize(2);
                    assertThat(call.returnType()).isEqualTo(PrimitiveLinkedType.DOUBLE);
                });
        assertThat(compiledFunction(compiled, "Consumer", "read_cell").expression())
                .isInstanceOfSatisfying(CompiledFieldAccess.class, fieldAccess -> {
                    assertThat(fieldAccess.field()).isEqualTo("name");
                    assertThat(fieldAccess.source()).isInstanceOfSatisfying(CompiledFunctionCall.class, call -> {
                        assertThat(call.name()).isEqualTo("__method__Matrix__get");
                        assertThat(call.arguments()).hasSize(3);
                    });
                });
    }

    @Test
    void shouldUseQualifiedModuleKeyForLocalDeriversWhenModuleNamesCollide() {
        var compiled = compileProgram(List.of(
                new RawModule("Serde", "/foo", """
                        deriver Show {
                            fun show(): String = "foo"
                        }

                        data User { name: String } derive Show

                        fun render(): String = User { name: "Ada" }.show()
                        """),
                new RawModule("Serde", "/bar", """
                        data User { name: String }
                        """)
        ), new java.util.TreeSet<>());

        var fooSerde = compiled.modules().stream()
                .filter(module -> module.name().equals("Serde"))
                .filter(module -> module.path().equals("/foo"))
                .findFirst()
                .orElseThrow();
        var barSerde = compiled.modules().stream()
                .filter(module -> module.name().equals("Serde"))
                .filter(module -> module.path().equals("/bar"))
                .findFirst()
                .orElseThrow();

        assertThat(fooSerde.functions())
                .extracting(CompiledFunction::name)
                .contains("__method__User__show", "render");
        assertThat(barSerde.functions())
                .extracting(CompiledFunction::name)
                .doesNotContain("__method__User__show");
    }

    @Test
    void shouldReflectDataValueFields() {
        var libraries = compileProgram(List.of(reflectionMetadataModule()), new java.util.TreeSet<>()).modules();
        var compiled = compileProgram(List.of(new RawModule("Consumer", "/foo/app", """
                from /capy/meta_prog/Reflection import { DataValueInfo, reflection }

                data User { name: String, age: int }

                fun reflect_user(): DataValueInfo = reflection(User { name: "Ada", age: 42 })
                """)), libraries);

        var function = compiledFunction(compiled, "Consumer", "reflect_user");
        assertThat(function.returnType().name()).endsWith("DataValueInfo");
        assertThat(function.expression()).isInstanceOfSatisfying(CompiledReflectionValue.class, reflectionValue -> {
            assertThat(reflectionValue.name()).isEqualTo("User");
            assertThat(reflectionValue.packageName()).isEqualTo("Consumer");
            assertThat(reflectionValue.packagePath()).isEqualTo("foo/app/Consumer");
            assertThat(reflectionValue.fields()).extracting(CompiledReflectionValue.Field::name)
                    .containsExactly("name", "age");
            assertThat(reflectionValue.fields()).extracting(field -> field.type().name())
                    .containsExactly("STRING", "INT");
        });
    }

    @Test
    void shouldLinkReflectionValueForGenericDataSupertype() {
        var libraries = compileProgram(List.of(reflectionMetadataModule()), new java.util.TreeSet<>()).modules();
        var compiled = compileProgram(List.of(new RawModule("Consumer", "/foo/app", """
                from /capy/meta_prog/Reflection import { DataValueInfo, reflection }

                fun reflect_data(obj: data): DataValueInfo = reflection(obj)
                """)), libraries);

        var function = compiledFunction(compiled, "Consumer", "reflect_data");
        assertThat(function.returnType().name()).endsWith("DataValueInfo");
        assertThat(function.expression()).isInstanceOfSatisfying(CompiledReflectionValue.class, reflectionValue -> {
            assertThat(reflectionValue.target().type()).isEqualTo(PrimitiveLinkedType.DATA);
            assertThat(reflectionValue.fields()).isEmpty();
        });
    }

    @Test
    void shouldLinkReflectionIntrinsicFromWildcardImport() {
        var libraries = compileProgram(List.of(reflectionMetadataModule()), new java.util.TreeSet<>()).modules();
        var compiled = compileProgram(List.of(new RawModule("Consumer", "/foo/app", """
                from /capy/meta_prog/Reflection import { * }

                data User { name: String }

                fun reflect_user(user: User): DataValueInfo = reflection(user)
                """)), libraries);

        assertThat(compiledFunction(compiled, "Consumer", "reflect_user").expression())
                .isInstanceOf(CompiledReflectionValue.class);
    }

    @Test
    void shouldSelectRegularReflectionOverloadWhenIntrinsicIsVisible() {
        var libraries = compileProgram(List.of(reflectionMetadataModule()), new java.util.TreeSet<>()).modules();
        var compiled = compileProgram(List.of(new RawModule("Consumer", "/foo/app", """
                from /capy/meta_prog/Reflection import { DataValueInfo, reflection }

                data User { name: String }

                fun reflection(value: String): String = value

                fun reflect_string(): String = reflection("x")
                fun reflect_user(user: User): DataValueInfo = reflection(user)
                """)), libraries);

        assertThat(compiledFunction(compiled, "Consumer", "reflect_string").expression())
                .isInstanceOfSatisfying(CompiledFunctionCall.class, call ->
                        assertThat(call.returnType()).isEqualTo(PrimitiveLinkedType.STRING))
                .isNotInstanceOf(CompiledReflectionValue.class);
        assertThat(compiledFunction(compiled, "Consumer", "reflect_user").expression())
                .isInstanceOf(CompiledReflectionValue.class);
    }

    @Test
    void shouldNotTreatUserDefinedReflectionFunctionAsIntrinsic() {
        var libraries = compileProgram(List.of(reflectionMetadataModule()), new java.util.TreeSet<>()).modules();
        var compiled = compileProgram(List.of(new RawModule("Consumer", "/foo/app", """
                from /capy/meta_prog/Reflection import { DataValueInfo, PackageInfo }

                fun reflection(obj: data): DataValueInfo =
                    DataValueInfo {
                        name: "local",
                        pkg: PackageInfo { name: "", path: "" },
                        fields: []
                    }

                data User { name: String }

                fun reflect_user(user: User): DataValueInfo = reflection(user)
                """)), libraries);

        assertThat(compiledFunction(compiled, "Consumer", "reflect_user").expression())
                .isInstanceOf(CompiledFunctionCall.class)
                .isNotInstanceOf(CompiledReflectionValue.class);
    }

    @Test
    void shouldPreserveReceiverShadowingInsideDeriverLocalScopes() {
        var compiled = compileProgram(List.of(new RawModule("Consumer", "/foo/app", """
                deriver Shadow {
                    fun shadow_let(): String =
                        let receiver: String = "local"
                        receiver

                    fun shadow_lambda(): String =
                        let f: String => String = receiver => receiver
                        f("local")

                    fun shadow_reduce(): String =
                        ["a"] |> "", (receiver, item) => receiver + item

                    fun shadow_match(value: any): String =
                        match value with
                        case String receiver -> receiver
                        case _ -> ""
                }

                data User { name: String } derive Shadow
                """)), new java.util.TreeSet<>());

        assertThat(compiled.modules().first().functions())
                .extracting(CompiledFunction::name)
                .contains("__method__User__shadow_let", "__method__User__shadow_lambda",
                        "__method__User__shadow_reduce", "__method__User__shadow_match");
    }

    @Test
    void shouldRejectUnknownDeriver() {
        var error = compileFailure(List.of(new RawModule("Consumer", "/foo/app", """
                data User { name: String } derive Missing
                """)));

        assertThat(error.message())
                .contains("Deriver `Missing` not found for `User`");
    }

    @Test
    void shouldRejectDeriverParameterNamedReceiver() {
        var error = compileFailure(List.of(new RawModule("Consumer", "/foo/app", """
                deriver Show {
                    fun show(receiver: String): String = receiver
                }

                data User { name: String } derive Show
                """)));

        assertThat(error.message())
                .contains("Deriver method parameter cannot be named `receiver`");
    }

    @Test
    void shouldRejectDuplicateMethodSignaturesInsideUnusedDeriver() {
        var error = compileFailure(List.of(new RawModule("Consumer", "/foo/app", """
                deriver Show {
                    fun show(): String = "first"
                    fun show(): String = "second"
                }
                """)));

        assertThat(error.message())
                .contains("Duplicate deriver method signature `show`");
    }

    @Test
    void shouldRejectDerivedMethodCollision() {
        var error = compileFailure(List.of(new RawModule("Consumer", "/foo/app", """
                deriver Show {
                    fun show(): String = "generated"
                }

                data User { name: String } derive Show

                fun User.show(): String = "manual"
                """)));

        assertThat(error.message())
                .contains("Duplicate function signature `User.show`");
    }

    @Test
    void shouldApplyLibraryDataConstructorWhenInstantiatingImportedData() {
        var librarySource = """
                data OddInt { number: int } with constructor {
                    if number % 2 == 0 then
                        * { number: number + 1 }
                    else
                        * { number: number }
                }
                """;
        var libraries = compileProgram(List.of(new RawModule("Library", "/foo/lib", librarySource)), new java.util.TreeSet<>()).modules();

        var consumerSource = """
                from Library import { * }
                fun build(value: int): OddInt = OddInt { number: value }
                """;
        var compiled = compileProgram(List.of(new RawModule("Consumer", "/foo/app", consumerSource)), libraries);

        assertThat(compiledFunction(compiled, "Consumer", "build").expression())
                .isInstanceOfSatisfying(CompiledFunctionCall.class, call ->
                        assertThat(call.name()).endsWith(".Library.__constructor__data__OddInt"));
    }

    @Test
    void shouldBypassLocalResultReturningDataConstructor() {
        var compiled = compileProgram(List.of(new RawModule("Consumer", "/foo/app", """
                from /capy/lang/Result import { * }

                data User { age: int } with constructor {
                    if age <= 0 then
                        Error { message: "invalid" }
                    else
                        Success { value: * { age: age } }
                }

                fun fallback(): User = User! { age: 1 }
                """)), new java.util.TreeSet<>());

        assertThat(compiledFunction(compiled, "Consumer", "fallback").expression())
                .isInstanceOfSatisfying(CompiledNewData.class, newData ->
                        assertThat(((CompiledDataType) newData.type()).name()).isEqualTo("User"));
    }

    @Test
    void shouldBypassLocalResultReturningDataConstructorWithNonCanonicalModulePath() {
        var compiled = compileProgram(List.of(new RawModule("Consumer", "/./foo//app/", """
                from /capy/lang/Result import { * }

                data User { age: int } with constructor {
                    if age <= 0 then
                        Error { message: "invalid" }
                    else
                        Success { value: * { age: age } }
                }

                fun fallback(): User = User! { age: 1 }
                """)), new java.util.TreeSet<>());

        assertThat(compiledFunction(compiled, "Consumer", "fallback").expression())
                .isInstanceOfSatisfying(CompiledNewData.class, newData ->
                        assertThat(((CompiledDataType) newData.type()).name()).isEqualTo("User"));
    }

    @Test
    void shouldApplyLibraryTypeConstructorWhenInstantiatingImportedSubtype() {
        var librarySource = """
                from /capy/lang/Result import { * }

                type ValidatedName { name: String } with constructor {
                   if name.size == 0 then
                       Error { message: "Name was empty" }
                   else
                       Success { value: * { name: name } }
                } = NamedUser

                data NamedUser { name: String, role: String }
                """;
        var libraries = compileProgram(List.of(new RawModule("Library", "/foo/lib", librarySource)), new java.util.TreeSet<>()).modules();

        var consumerSource = """
                from Library import { * }

                fun build(name: String, role: String): Result[NamedUser] =
                    NamedUser { name: name, role: role }
                """;
        var compiled = compileProgram(List.of(new RawModule("Consumer", "/foo/app", consumerSource)), libraries);

        assertThat(compiledFunction(compiled, "Consumer", "build").expression())
                .isInstanceOf(CompiledMatchExpression.class);
    }

    @Test
    void shouldApplyTransitiveLibraryTypeConstructorWhenInstantiatingImportedSubtype() {
        var librarySource = """
                from /capy/lang/Result import { * }

                type ValidatedName { name: String } with constructor {
                   if name.size == 0 then
                       Error { message: "Name was empty" }
                   else
                       Success { value: * { name: name } }
                } = NamedEntity

                type NamedEntity { name: String } = NamedUser

                data NamedUser { name: String, role: String }
                """;
        var libraries = compileProgram(List.of(new RawModule("Library", "/foo/lib", librarySource)), new java.util.TreeSet<>()).modules();

        var consumerSource = """
                from Library import { * }

                fun build(name: String, role: String): Result[NamedUser] =
                    NamedUser { name: name, role: role }
                """;
        var compiled = compileProgram(List.of(new RawModule("Consumer", "/foo/app", consumerSource)), libraries);

        assertThat(compiledFunction(compiled, "Consumer", "build").expression())
                .isInstanceOf(CompiledMatchExpression.class);
    }

    @Test
    void shouldApplyDeclaredTypeCoercionInsideResultBindSuccessBranch() {
        var compiled = compileProgram(List.of(
                new RawModule("Consumer", "/foo/app", """
                        from /capy/lang/Result import { * }

                        data Parent { name: String }
                        data Child { age: int, ...Parent }

                        fun child(): Result[Child] =
                            Success { value: 1 } | value => Success { value: Child { age: value, name: "Ada" } }

                        fun use_parent(): Result[String] =
                            let parent: Parent <- child()
                            parent.name
                        """)
        ), new java.util.TreeSet<>());

        assertThat(compiledFunction(compiled, "Consumer", "use_parent").expression())
                .isInstanceOfSatisfying(CompiledMatchExpression.class, match ->
                        assertThat(match.cases().getFirst().expression())
                                .isInstanceOfSatisfying(CompiledNewData.class, success ->
                                        assertThat(success.assignments().getFirst().value())
                                                .isInstanceOf(CompiledLetExpression.class)));
    }

    @Test
    void shouldResolveImportedParentTypeFieldsAcrossModules() {
        var compiled = compileProgram(List.of(
                new RawModule("Assert", "/capy/test", """
                        data Assertion { result: bool, message: String }
                        type Assert { assertions: List[Assertion] } = StringAssert
                        data StringAssert { value: String }
                        """),
                new RawModule("CapyTest", "/capy/test", """
                        from Assert import { * }

                        data TestCase { asserts: List[Assert] }
                        fun assertion_count(test_case: TestCase): int =
                            test_case.asserts
                                |> 0, (acc, a) => acc + a.assertions.size
                        """)
        ), new java.util.TreeSet<>());

        assertThat(compiled.modules())
                .extracting(CompiledModule::name)
                .containsExactlyInAnyOrder("Assert", "CapyTest");
        assertThat(compiled.modules().stream()
                .filter(module -> module.name().equals("CapyTest"))
                .findFirst()
                .orElseThrow()
                .functions())
                .extracting(CompiledFunction::name)
                .contains("assertion_count");
    }


    @Test
    void shouldResolveImportedDataFieldsNestedInCollectionsAcrossModules() {
        var compiled = compileProgram(List.of(
                new RawModule("CapyTest", "/capy/test", """
                        data TestCase { asserts: List[String] }
                        data TestFile { test_cases: List[TestCase] }
                        """),
                new RawModule("Runtime", "/capy/test", """
                        from CapyTest import { * }

                        fun assertion_count(test_file: TestFile): int =
                            test_file.test_cases | tc => tc.asserts.size |> 0, (acc, count) => acc + count
                        """)
        ), new java.util.TreeSet<>());

        assertThat(compiled.modules())
                .extracting(CompiledModule::name)
                .containsExactlyInAnyOrder("CapyTest", "Runtime");
        assertThat(compiled.modules().stream()
                .filter(module -> module.name().equals("Runtime"))
                .findFirst()
                .orElseThrow()
                .functions())
                .extracting(CompiledFunction::name)
                .contains("assertion_count");
    }

    @Test
    void shouldCompileDataWithExpression() {
        var compiled = compileProgram(List.of(
                new RawModule("WithData", "/foo/with", """
                        data Foo { a: int, b: String }
                        fun update(foo: Foo): Foo = foo.with(a: foo.a + 1, b: "x")
                        """)
        ), new java.util.TreeSet<>());

        assertThat(compiled.modules().stream()
                .filter(module -> module.name().equals("WithData"))
                .findFirst()
                .orElseThrow()
                .functions())
                .extracting(CompiledFunction::name)
                .contains("update");
    }

    @Test
    void shouldCompileParentWithExpression() {
        var compiled = compileProgram(List.of(
                new RawModule("WithParent", "/foo/with", """
                        type Letter { x: int } = A | B
                        data A { a: String }
                        data B { b: int }
                        fun update(letter: Letter): Letter = letter.with(x: letter.x + 1)
                        """)
        ), new java.util.TreeSet<>());

        assertThat(compiled.modules().stream()
                .filter(module -> module.name().equals("WithParent"))
                .findFirst()
                .orElseThrow()
                .functions())
                .extracting(CompiledFunction::name)
                .contains("update");
    }

    @Test
    void shouldResolveModuleQualifiedTypeWithoutImport() {
        var compiled = compileProgram(List.of(
                new RawModule("Types", "/foo/lib", """
                        data Thing { value: int }
                        """),
                new RawModule("Consumer", "/foo/app", """
                        data Thing { label: String }

                        fun local_thing(): Thing = Thing { label: "local" }
                        fun imported_thing(value: int): Types.Thing = Types.Thing { value: value }
                        """)
        ), new java.util.TreeSet<>());

        assertThat(compiledFunction(compiled, "Consumer", "local_thing").returnType().name()).isEqualTo("Thing");
        assertThat(compiledFunction(compiled, "Consumer", "imported_thing").returnType().name()).isEqualTo("Types.Thing");
    }

    @Test
    void shouldResolvePathQualifiedTypeWithoutImport() {
        var compiled = compileProgram(List.of(
                new RawModule("Types", "/foo/bar", """
                        data Thing { value: int }
                        """),
                new RawModule("Consumer", "/foo/app", """
                        fun imported_thing(value: int): /foo/bar/Types.Thing = /foo/bar/Types.Thing { value: value }
                        """)
        ), new java.util.TreeSet<>());

        assertThat(compiledFunction(compiled, "Consumer", "imported_thing").returnType().name())
                .isEqualTo("/foo/bar/Types.Thing");
    }

    @Test
    void shouldResolvePathQualifiedModuleShorthandWhenModuleNameMatchesType() {
        var compiled = compileProgram(List.of(
                new RawModule("Program", "/foo/bar", """
                        data Program { value: int }
                        """),
                new RawModule("Consumer", "/foo/app", """
                        fun build(value: int): /foo/bar/Program = /foo/bar/Program { value: value }
                        """)
        ), new java.util.TreeSet<>());

        assertThat(compiledFunction(compiled, "Consumer", "build").returnType().name()).isEqualTo("/foo/bar/Program");
    }

    @Test
    void shouldPreferResultOverDataOverloadForConstructorExpressionsInMethodChains() {
        var compiled = compileProgram(List.of(
                new RawModule("Result", "/capy/lang", """
                        type Result[T] = Success[T] | Error
                        data Success[T] { value: T }
                        data Error { message: String }
                        """),
                new RawModule("Assert", "/capy/test", """
                        from /capy/lang/Result import { * }

                        data ResultAssert[T] { value: Result[T] }
                        data DataAssert { value: data }

                        fun ResultAssert[T].succeeds(): bool = true

                        fun assert_that(value: Result[T]): ResultAssert[T] = ResultAssert { value: value }
                        fun assert_that(value: data): DataAssert = DataAssert { value: value }
                        """),
                new RawModule("Date", "/capy/date_time", """
                        from /capy/lang/Result import { * }

                        data Date { day: int } with constructor {
                            if day > 0
                            then Success { value: * { day: day } }
                            else Error { message: "invalid" }
                        }
                        """),
                new RawModule("Consumer", "/foo/app", """
                        from /capy/test/Assert import { * }
                        from /capy/date_time/Date import { * }

                        fun prefer_result(): bool = assert_that(Date { day: 1 }).succeeds()
                        """)
        ), new java.util.TreeSet<>());

        assertThat(compiledFunction(compiled, "Consumer", "prefer_result").returnType()).isEqualTo(PrimitiveLinkedType.BOOL);
    }

    @Test
    void shouldPreferResultOverDataOverloadForConstructorExpressionsAgainstLinkedLibraries() {
        var libraries = compileProgram(List.of(
                new RawModule("Result", "/capy/lang", """
                        type Result[T] = Success[T] | Error
                        data Success[T] { value: T }
                        data Error { message: String }
                        """),
                new RawModule("Assert", "/capy/test", """
                        from /capy/lang/Result import { * }

                        data ResultAssert[T] { value: Result[T] }
                        data DataAssert { value: data }

                        fun assert_that(value: Result[T]): ResultAssert[T] = ResultAssert { value: value }
                        fun assert_that(value: data): DataAssert = DataAssert { value: value }
                        """),
                new RawModule("Date", "/capy/date_time", """
                        from /capy/lang/Result import { * }

                        data Date { day: int } with constructor {
                            if day > 0
                            then Success { value: * { day: day } }
                            else Error { message: "invalid" }
                        }
                        """)
        ), new java.util.TreeSet<>()).modules();

        var compiled = compileProgram(List.of(
                new RawModule("Consumer", "/foo/app", """
                        from /capy/test/Assert import { * }
                        from /capy/date_time/Date import { * }

                        fun prefer_result(): ResultAssert[Date] = assert_that(Date { day: 1 })
                        """)
        ), libraries);

        assertThat(compiledFunction(compiled, "Consumer", "prefer_result").returnType().name())
                .isEqualTo("ResultAssert");
    }

    @Test
    void shouldPreferDirectOverloadsOverWrappingPlainValuesIntoResult() {
        var compiled = compileProgram(List.of(
                new RawModule("Result", "/capy/lang", """
                        type Result[T] = Success[T] | Error
                        data Success[T] { value: T }
                        data Error { message: String }
                        """),
                new RawModule("Assert", "/capy/test", """
                        from /capy/lang/Result import { * }

                        data ResultAssert[T] { value: Result[T] }
                        data DataAssert { value: data }
                        data ListAssert[T] { value: List[T] }

                        fun ResultAssert[T].is_equal_to(other: Result[T]): ResultAssert[T] = this
                        fun DataAssert.is_equal_to(other: data): DataAssert = this
                        fun ListAssert[T].is_equal_to(other: List[T]): ListAssert[T] = this

                        fun assert_that(value: Result[T]): ResultAssert[T] = ResultAssert { value: value }
                        fun assert_that(value: data): DataAssert = DataAssert { value: value }
                        fun assert_that(value: List[T]): ListAssert[T] = ListAssert { value: value }
                        """),
                new RawModule("Consumer", "/foo/app", """
                        from /capy/lang/Result import { * }
                        from /capy/test/Assert import { * }

                        data Seed { value: long }

                        fun data_assert(seed: Seed): DataAssert = assert_that(seed)
                        fun list_assert(values: List[int]): ListAssert[int] = assert_that(values)
                        fun result_assert(value: Result[Seed]): ResultAssert[Seed] = assert_that(value)
                        fun data_chain(seed: Seed): DataAssert = assert_that(seed).is_equal_to(seed)
                        fun list_chain(values: List[int]): ListAssert[int] = assert_that(values).is_equal_to(values)
                        """)
        ), new java.util.TreeSet<>());

        assertThat(compiledFunction(compiled, "Consumer", "data_assert").returnType().name())
                .isEqualTo("DataAssert");
        assertThat(compiledFunction(compiled, "Consumer", "list_assert").returnType().name())
                .isEqualTo("ListAssert");
        assertThat(compiledFunction(compiled, "Consumer", "result_assert").returnType().name())
                .isEqualTo("ResultAssert");
        assertThat(CompiledExpressionPrinter.printExpression(compiledFunction(compiled, "Consumer", "data_chain").expression(), 0))
                .contains("__method__DataAssert__is_equal_to")
                .doesNotContain("Success");
        assertThat(CompiledExpressionPrinter.printExpression(compiledFunction(compiled, "Consumer", "list_chain").expression(), 0))
                .contains("__method__ListAssert__is_equal_to")
                .doesNotContain("Success");
    }

    @Test
    void shouldResolveAbsoluteImportsAgainstRelativeModulePaths() {
        var compiled = compileProgram(List.of(
                new RawModule("Result", "capy/lang", """
                        type Result[T] = Success[T] | Error
                        data Success[T] { value: T }
                        data Error { message: String }
                        """),
                new RawModule("Date", "capy/date_time", """
                        from /capy/lang/Result import { * }

                        data Date { day: int } with constructor {
                            if day > 0
                            then Success { value: * { day: day } }
                            else Error { message: "invalid" }
                        }
                        """)
        ), new java.util.TreeSet<>());

        assertThat(compiled.modules()).extracting(CompiledModule::name)
                .containsExactlyInAnyOrder("Result", "Date");
    }

    @Test
    void shouldRejectNestedResultWhenResultAssertExpectsSingleResult() {
        var error = compileFailure(List.of(
                new RawModule("Result", "/capy/lang", """
                        type Result[T] = Success[T] | Error
                        data Success[T] { value: T }
                        data Error { message: String }
                        """),
                new RawModule("Assert", "/capy/test", """
                        from /capy/lang/Result import { * }

                        data ResultAssert[T] { value: Result[T] }
                        data DataAssert { value: data }

                        fun ResultAssert[T].is_equal_to(other: Result[T]): ResultAssert[T] = this
                        fun DataAssert.is_equal_to(other: data): DataAssert = this

                        fun assert_that(value: Result[T]): ResultAssert[T] = ResultAssert { value: value }
                        fun assert_that(value: data): DataAssert = DataAssert { value: value }
                        """),
                new RawModule("Widget", "/foo/model", """
                        from /capy/lang/Result import { * }

                        data Widget { size: int }
                        fun Widget.wrap(): Result[Widget] = Success { value: this }
                        """),
                new RawModule("Consumer", "/foo/app", """
                        from /capy/lang/Result import { * }
                        from /capy/test/Assert import { * }
                        from /foo/model/Widget import { * }

                        fun broken() =
                            assert_that(Success { value: Widget { size: 1 } } | widget => Success { value: widget.wrap() })
                                .is_equal_to(Success { value: Widget { size: 2 } })
                        """)
        ));

        assertThat(error.message()).contains("Expected `Widget`, got `Success[Widget]`");
    }

    @Test
    void shouldPreferLocalConstructorOverUnimportedLinkedLibraryConstructorWithSameName() {
        var libraries = compileProgram(List.of(
                new RawModule("Result", "/capy/lang", """
                        type Result[T] = Success[T] | Error
                        data Success[T] { value: T }
                        data Error { message: String }
                        """),
                new RawModule("Assert", "/capy/test", """
                        from /capy/lang/Result import { * }

                        data ResultAssert[T] { value: Result[T] }

                        fun ResultAssert[T].succeeds(): bool = true
                        fun ResultAssert[T].fails(): bool = true

                        fun assert_that(value: Result[T]): ResultAssert[T] = ResultAssert { value: value }
                        """),
                new RawModule("Date", "/capy/date_time", """
                        from /capy/lang/Result import { * }

                        data Date { day: int, month: int, year: int } with constructor {
                            if day > 0 then
                                Success { value: * { day: day, month: month, year: year } }
                            else
                                Error { message: "invalid" }
                        }
                        """)
        ), new java.util.TreeSet<>()).modules();

        var compiled = compileProgram(List.of(
                new RawModule("Consumer", "/dev/capylang/test", """
                        from /capy/lang/Result import { * }
                        from /capy/test/Assert import { * }

                        data Date { day: int } with constructor {
                            if day > 0
                            then Success { value: * { day: day } }
                            else Error { message: "invalid" }
                        }

                        fun valid_date_assert_succeeds(): bool =
                            assert_that(Date { day: 1 }).succeeds()

                        fun invalid_date_assert_fails(): bool =
                            assert_that(Date { day: 0 }).fails()
                        """)
        ), libraries);

        assertThat(compiledFunction(compiled, "Consumer", "valid_date_assert_succeeds").returnType())
                .isEqualTo(PrimitiveLinkedType.BOOL);
        assertThat(compiledFunction(compiled, "Consumer", "invalid_date_assert_fails").returnType())
                .isEqualTo(PrimitiveLinkedType.BOOL);
    }

    @Test
    void shouldCompileTypedAssertLetsForResultAssertLambdaChains() {
        var compiled = compileProgram(List.of(
                new RawModule("Result", "/capy/lang", """
                        type Result[T] = Success[T] | Error
                        data Success[T] { value: T }
                        data Error { message: String }
                        """),
                new RawModule("Assert", "/capy/test", """
                        from /capy/lang/Result import { * }

                        type Assert = TechnicalAssert | DataAssert | IntAssert | ResultAssert
                        data TechnicalAssert { assertions: List[bool] }
                        data DataAssert { value: data }
                        data IntAssert { value: int }
                        data ResultAssert[T] { value: Result[T] }

                        fun assert_all(asserts: List[Assert]): Assert = TechnicalAssert { assertions: [] }
                        fun DataAssert.is_equal_to(other: data): DataAssert = this
                        fun IntAssert.is_equal_to(other: int): IntAssert = this
                        fun ResultAssert[T].fails(): ResultAssert[T] = this
                        fun ResultAssert[T].succeeds(assert_: T => Assert): ResultAssert[T] = this

                        fun assert_that(value: Result[T]): ResultAssert[T] = ResultAssert { value: value }
                        fun assert_that(value: int): IntAssert = IntAssert { value: value }
                        fun assert_that(value: data): DataAssert = DataAssert { value: value }
                        """),
                new RawModule("Date", "/capy/date_time", """
                        from /capy/lang/Result import { * }

                        const JANUARY: int = 1

                        data Date { day: int, month: int, year: int } with constructor {
                            if day > 0
                            then Success { value: * { day: day, month: month, year: year } }
                            else Error { message: "invalid" }
                        }
                        """),
                new RawModule("Consumer", "/foo/app", """
                        from /capy/test/Assert import { * }
                        from /capy/date_time/Date import { * }

                        fun helper(day: int, is_valid: bool): Assert =
                            let assert: Assert =
                                if is_valid
                                then assert_that(Date { day: day, month: JANUARY, year: 2020 })
                                    .succeeds(date => assert_all([
                                        assert_that(date.day).is_equal_to(day),
                                        assert_that(date.month).is_equal_to(JANUARY),
                                    ]))
                                else assert_that(Date { day: day, month: JANUARY, year: 2020 }).fails()
                            assert
                        """)
        ), new java.util.TreeSet<>());

        assertThat(compiledFunction(compiled, "Consumer", "helper").returnType().name())
                .isEqualTo("Assert");
    }

    @Test
    void shouldWrapParentSubtypesIntoExpectedResultWithoutRecursiveCoercion() {
        var compiled = compileProgram(List.of(
                new RawModule("Result", "/capy/lang", """
                        type Result[T] = Success[T] | Error
                        data Success[T] { value: T }
                        data Error { message: String }
                        """),
                new RawModule("Json", "/capy/serialization", """
                        from /capy/lang/Result import { * }

                        type Json = JsonObject | JsonBool
                        data JsonObject { value: Dict[Json] }
                        data JsonBool { value: bool }

                        fun deserialize(valid: bool): Result[Json] =
                            let decoded =
                                if valid
                                then JsonObject { value: { : } }
                                else JsonBool { value: true }
                            decoded
                        """)
        ), new java.util.TreeSet<>());

        assertThat(compiledFunction(compiled, "Json", "deserialize").returnType().name())
                .isEqualTo("Result");
    }

    @Test
    void shouldResolveMethodsDefinedInLinkedLibraryModules() {
        var libraries = compileProgram(List.of(
                new RawModule("FooAssert", "/foo/test", """
                        data OutcomeAssert { value: bool }
                        fun OutcomeAssert.ok(): bool = this.value
                        fun check(value: bool): OutcomeAssert = OutcomeAssert { value: value }
                        """)
        ), new java.util.TreeSet<>()).modules();

        var compiled = compileProgram(List.of(
                new RawModule("Consumer", "/foo/app", """
                        from /foo/test/FooAssert import { * }

                        fun use_method(): bool = check(true).ok()
                        """)
        ), libraries);

        assertThat(compiledFunction(compiled, "Consumer", "use_method").returnType())
                .isEqualTo(PrimitiveLinkedType.BOOL);
    }

    @Test
    void shouldResolveQualifiedImportsWhenSimpleModuleNamesCollide() {
        var libraries = compileProgram(List.of(
                new RawModule("Assert", "/foo/test", """
                        data OutcomeAssert { value: bool }
                        fun OutcomeAssert.ok(): bool = this.value
                        fun check(value: bool): OutcomeAssert = OutcomeAssert { value: value }
                        """)
        ), new java.util.TreeSet<>()).modules();

        var compiled = compileProgram(List.of(
                new RawModule("Consumer", "/foo/app", """
                        from /foo/test/Assert import { * }

                        fun use_method(): bool = check(true).ok()
                        """)
        ), libraries);

        assertThat(compiledFunction(compiled, "Consumer", "use_method").returnType())
                .isEqualTo(PrimitiveLinkedType.BOOL);
    }

    @Test
    void shouldPreferExactOverWidenedNumericOverload() {
        var compiled = compileProgram(List.of(
                new RawModule("Consumer", "/foo/app", """
                        fun pick(value: int): int = value
                        fun pick(value: long): long = value + 1L

                        fun use_int(value: int): int = pick(value)
                        fun use_long(value: int): long = pick(10L * value)
                        """)
        ), new java.util.TreeSet<>());

        assertThat(compiledFunction(compiled, "Consumer", "use_int").expression())
                .isInstanceOfSatisfying(CompiledFunctionCall.class, call ->
                        assertThat(call.returnType()).isEqualTo(PrimitiveLinkedType.INT));
        assertThat(compiledFunction(compiled, "Consumer", "use_long").expression())
                .isInstanceOfSatisfying(CompiledFunctionCall.class, call ->
                        assertThat(call.returnType()).isEqualTo(PrimitiveLinkedType.LONG));
    }

    @Test
    void shouldRejectSubtypeFieldTypeThatDoesNotMatchParentMemberType() {
        var error = compileFailure(List.of(
                new RawModule("Consumer", "/foo/app", """
                        type Duration { seconds: long } = DateDuration
                        data DateDuration { seconds: int }
                        """)
        ));

        assertThat(error.message())
                .contains("Field `seconds` in subtype `DateDuration`")
                .contains("must match parent type `Duration`")
                .contains("LONG")
                .contains("INT");
    }

    @Test
    void shouldRejectTypeMethodThatConflictsWithSubtypeFieldGetterType() {
        var error = compileFailure(List.of(
                new RawModule("Consumer", "/foo/app", """
                        type Duration = DateDuration | WeekDuration
                        data DateDuration { seconds: int }
                        data WeekDuration { weeks: int }

                        fun Duration.seconds(): long =
                            match this with
                            case DateDuration { seconds } -> seconds
                            case WeekDuration -> 0L
                        """)
        ));

        assertThat(error.message())
                .contains("/foo/app/Consumer.cfun")
                .contains("fun Duration.seconds(): long =")
                .contains("Field getter `DateDuration.seconds` returns `int`, but this method returns `long`")
                .contains("Conflicting declaration: data `DateDuration` at /foo/app/Consumer.cfun:2:0");
    }

    private static CompiledProgram compileProgram(List<RawModule> rawModules, SortedSet<CompiledModule> libraries) {
        var result = CapybaraCompiler.INSTANCE.compile(rawModules, libraries);
        if (result instanceof Result.Error<CompiledProgram> error) {
            fail(error.errors().toString());
        }
        return ((Result.Success<CompiledProgram>) result).value();
    }

    private static Result.Error.SingleError compileFailure(List<RawModule> rawModules) {
        var result = CapybaraCompiler.INSTANCE.compile(rawModules, new java.util.TreeSet<>());
        if (result instanceof Result.Success<CompiledProgram> value) {
            fail("Expected compilation to fail but it succeeded: " + value);
        }
        var errors = ((Result.Error<CompiledProgram>) result).errors();
        assertThat(errors).isNotEmpty();
        return errors.first();
    }

    private static RawModule reflectionMetadataModule() {
        return new RawModule("Reflection", "/capy/meta_prog", """
                type AnyInfo { name: String, pkg: PackageInfo } =
                    DataInfo
                    | InterfaceInfo
                    | ObjectInfo
                    | TraitInfo
                    | ListInfo
                    | SetInfo
                    | DictInfo
                    | TupleInfo
                    | FunctionTypeInfo

                data DataInfo { name: String, pkg: PackageInfo }

                data InterfaceInfo { methods: List[MethodInfo], parents: Set[AnyInfo] }
                data ObjectInfo { open: bool, fields: List[FieldInfo], methods: List[MethodInfo], parents: Set[AnyInfo] }
                data TraitInfo { methods: List[MethodInfo], parents: Set[AnyInfo] }
                data MethodInfo { name: String, pkg: PackageInfo, params: List[FieldInfo], return_type: AnyInfo }

                data ListInfo { element_type: AnyInfo }
                data SetInfo { element_type: AnyInfo }
                data DictInfo { value_type: AnyInfo }

                data TupleInfo { elements: List[AnyInfo] }
                data FunctionTypeInfo { params: List[AnyInfo], return_type: AnyInfo }

                data PackageInfo { name: String, path: String }
                data FieldInfo { name: String, type: AnyInfo }
                data FieldValueInfo { name: String, type: AnyInfo, value: any }
                data DataValueInfo { name: String, pkg: PackageInfo, fields: List[FieldValueInfo] }

                fun reflection(obj: data): DataValueInfo = <native>
                """);
    }

    private static CompiledFunction compiledFunction(CompiledProgram program, String moduleName, String functionName) {
        return program.modules().stream()
                .filter(module -> module.name().equals(moduleName))
                .findFirst()
                .orElseThrow()
                .functions().stream()
                .filter(function -> function.name().equals(functionName))
                .findFirst()
                .orElseThrow();
    }
}
