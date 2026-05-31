package dev.capylang.compiler;

import capy.lang.Result;

import dev.capylang.compiler.parser.ObjectOriented;
import dev.capylang.compiler.parser.RawModule;
import dev.capylang.compiler.parser.SourceKind;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.TreeSet;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.fail;

class AnnotationCompilerTest {
    @Test
    void shouldCompileSameModuleAndImportedAnnotationUsages() {
        var compiled = compileSuccess(List.of(
                new RawModule("Annotations", "/capy/test", """
                        annotation Test on fun {
                            name: String
                            retries: int = 0
                        }
                        """, SourceKind.FUNCTIONAL),
                new RawModule("Tests", "/foo/app", """
                        from /capy/test/Annotations import { Test }

                        annotation Local on fun {}

                        @Test(name: "adds values")
                        @Local()
                        fun should_add(): bool = true
                        """, SourceKind.FUNCTIONAL)
        ));

        var annotationsModule = compiled.modules().stream()
                .filter(module -> module.name().equals("Annotations"))
                .findFirst()
                .orElseThrow();
        assertThat(annotationsModule.annotations()).containsKey("Test");
    }

    @Test
    void shouldPersistCompiledAnnotationMetadataOnFunctionsAndTypes() {
        var modules = List.of(
                new RawModule("Annotations", "/capy/test", """
                        annotation Label on fun, data, union, type {
                            label: String
                            order: int = 7
                        }

                        annotation First on data { value: String = "first" }
                        annotation Second on data { value: String = "second" }
                        """, SourceKind.FUNCTIONAL),
                new RawModule("Tests", "/foo/app", """
                        from /capy/test/Annotations import { Label, First, Second }

                        @First()
                        @Second()
                        data User { name: String }

                        @Label(order: 2, label: "pet")
                        union Pet = User

                        @Label(label: "id")
                        type user_id -> int

                        @Label(order: 3, label: "run")
                        fun run(id: user_id): User = User { name: "ok" }
                        """, SourceKind.FUNCTIONAL)
        );

        var compiled = compileSuccess(modules);
        var repeated = compileSuccess(modules);
        var module = module(compiled, "Tests");

        var functionAnnotation = function(compiled, "Tests", "run").annotations().getFirst();
        assertThat(functionAnnotation.name()).isEqualTo("Label");
        assertThat(functionAnnotation.packageName()).isEqualTo("Annotations");
        assertThat(functionAnnotation.packagePath()).isEqualTo("/capy/test");
        assertThat(functionAnnotation.arguments()).extracting(CompiledAnnotationArgument::name)
                .containsExactly("label", "order");
        assertThat(functionAnnotation.arguments().getFirst().value())
                .isInstanceOfSatisfying(CompiledAnnotationValue.StringValue.class,
                        value -> assertThat(value.value()).isEqualTo("\"run\""));
        assertThat(functionAnnotation.arguments().get(1).value())
                .isInstanceOfSatisfying(CompiledAnnotationValue.IntValue.class,
                        value -> assertThat(value.value()).isEqualTo("3"));

        assertThat(module.types().get("User")).isInstanceOfSatisfying(CompiledDataType.class, user -> {
            assertThat(user.annotations()).extracting(CompiledAnnotation::name)
                    .containsExactly("First", "Second");
            assertThat(user.annotations()).isEqualTo(((CompiledDataType) module(repeated, "Tests").types().get("User")).annotations());
        });
        assertThat(module.types().get("Pet")).isInstanceOfSatisfying(CompiledDataParentType.class, pet ->
                assertThat(pet.annotations()).singleElement().satisfies(annotation ->
                        assertThat(annotation.arguments()).extracting(CompiledAnnotationArgument::name)
                                .containsExactly("label", "order")));
        assertThat(module.types().get("user_id")).isInstanceOfSatisfying(CompiledPrimitiveBackedType.class, userId -> {
            assertThat(userId.annotations()).singleElement().satisfies(annotation -> {
                assertThat(annotation.arguments()).extracting(CompiledAnnotationArgument::name)
                        .containsExactly("label", "order");
                assertThat(annotation.arguments().get(1).value())
                        .isInstanceOfSatisfying(CompiledAnnotationValue.IntValue.class,
                                value -> assertThat(value.value()).isEqualTo("7"));
            });
        });
    }

    @Test
    void shouldAllowDefaultedAnnotationArgumentsToBeOmitted() {
        compileSuccess(new RawModule("Tests", "/foo/app", """
                annotation Test on fun {
                    name: String = "default"
                    retries: int = 0
                }

                @Test
                fun should_run(): bool = true
                """, SourceKind.FUNCTIONAL));
    }

    @Test
    void shouldRejectRepeatedSingleUseAnnotation() {
        var error = compileFailure(new RawModule("Tests", "/foo/app", """
                annotation Deprecated on fun {}

                @Deprecated
                @Deprecated
                fun should_run(): bool = true
                """, SourceKind.FUNCTIONAL));

        assertThat(error.message()).contains("Annotation Deprecated cannot be applied multiple times");
    }

    @Test
    void shouldAllowRepeatedMultipleAnnotation() {
        var compiled = compileSuccess(new RawModule("Tests", "/foo/app", """
                multiple annotation Tag on fun {
                    value: String
                }

                @Tag(value: "slow")
                @Tag(value: "integration")
                fun should_run(): bool = true
                """, SourceKind.FUNCTIONAL));

        assertThat(function(compiled, "Tests", "should_run").annotations())
                .extracting(CompiledAnnotation::name)
                .containsExactly("Tag", "Tag");
    }

    @Test
    void shouldRejectMissingRequiredAnnotationArgument() {
        var error = compileFailure(new RawModule("Tests", "/foo/app", """
                annotation Test on fun {
                    name: String
                }

                @Test()
                fun should_run(): bool = true
                """, SourceKind.FUNCTIONAL));

        assertThat(error.message()).contains("Missing required annotation argument name for Test");
    }

    @Test
    void shouldRejectUnknownAnnotation() {
        var error = compileFailure(new RawModule("Tests", "/foo/app", """
                @Missing()
                fun should_run(): bool = true
                """, SourceKind.FUNCTIONAL));

        assertThat(error.message()).contains("Unknown annotation Missing");
    }

    @Test
    void shouldRejectUnimportedAnnotationFromAnotherModule() {
        var error = compileFailure(List.of(
                new RawModule("Annotations", "/capy/test", "annotation Test on fun {}", SourceKind.FUNCTIONAL),
                new RawModule("Tests", "/foo/app", """
                        @Test()
                        fun should_run(): bool = true
                        """, SourceKind.FUNCTIONAL)
        ));

        assertThat(error.message()).contains("Unknown annotation Test");
    }

    @Test
    void shouldRejectInvalidAnnotationTarget() {
        var error = compileFailure(new RawModule("Tests", "/foo/app", """
                annotation Entity on class {}

                @Entity
                fun should_run(): bool = true
                """, SourceKind.FUNCTIONAL));

        assertThat(error.message()).contains("Annotation Entity is not valid on function declarations");
    }

    @Test
    void shouldRejectAnnotationUsageOnAnnotationDeclaration() {
        var error = compileFailure(new RawModule("Tests", "/foo/app", """
                annotation Marker on fun {}

                @Marker()
                annotation Meta on fun {}
                """, SourceKind.FUNCTIONAL));

        assertThat(error.message()).contains("Annotation Marker is not valid on annotation declarations");
    }

    @Test
    void shouldRejectUnknownAnnotationArgument() {
        var messages = compileFailureMessages(new RawModule("Tests", "/foo/app", """
                annotation Test on fun {
                    name: String
                }

                @Test(label: "wrong")
                fun should_run(): bool = true
                """, SourceKind.FUNCTIONAL));

        assertThat(messages).anySatisfy(message ->
                assertThat(message).contains("Unknown annotation argument label for Test"));
    }

    @Test
    void shouldRejectDuplicateAnnotationArgument() {
        var error = compileFailure(new RawModule("Tests", "/foo/app", """
                annotation Test on fun {
                    name: String
                }

                @Test(name: "one", name: "two")
                fun should_run(): bool = true
                """, SourceKind.FUNCTIONAL));

        assertThat(error.message()).contains("Duplicate annotation argument name for Test");
    }

    @Test
    void shouldRejectWrongAnnotationValueType() {
        var error = compileFailure(new RawModule("Tests", "/foo/app", """
                annotation Retry on fun {
                    retries: int
                }

                @Retry(retries: "three")
                fun should_run(): bool = true
                """, SourceKind.FUNCTIONAL));

        assertThat(error.message()).contains("Annotation argument retries for Retry expects int, got String");
    }

    @Test
    void shouldValidateFunctionalMethodTargetSeparatelyFromFunctionTarget() {
        compileSuccess(new RawModule("Tests", "/foo/app", """
                annotation MethodOnly on method {}

                data Box {}

                @MethodOnly()
                fun Box.read(): int = 1
                """, SourceKind.FUNCTIONAL));

        var error = compileFailure(new RawModule("Tests", "/foo/app", """
                annotation MethodOnly on method {}

                @MethodOnly()
                fun read(): int = 1
                """, SourceKind.FUNCTIONAL));

        assertThat(error.message()).contains("Annotation MethodOnly is not valid on function declarations");
    }

    @Test
    void shouldValidateObjectOrientedClassFieldAndMethodTargets() {
        var compiled = compileSuccess(List.of(
                new RawModule("Annotations", "/capy/test", """
                        annotation Entity on class {}
                        annotation JsonName on field { value: String }
                        annotation Endpoint on method {}
                        """, SourceKind.FUNCTIONAL),
                new RawModule("User", "/foo/app", """
                        from /capy/test/Annotations import { Entity, JsonName, Endpoint }

                        @Entity
                        class User(name: String) {
                            @JsonName(value: "name")
                            field name: String = name

                            init {}

                            @Endpoint
                            def label(): String = this.name
                        }
                        """, SourceKind.OBJECT_ORIENTED)
        ));
        var user = (ObjectOriented.ClassDeclaration) compiled.objectOrientedModules().getFirst()
                .objectOriented()
                .definitions()
                .getFirst();
        assertThat(user.linkedAnnotations()).extracting(CompiledAnnotation::name)
                .containsExactly("Entity");
        assertThat(user.members().getFirst()).isInstanceOfSatisfying(ObjectOriented.FieldDeclaration.class, field ->
                assertThat(field.linkedAnnotations()).extracting(CompiledAnnotation::name)
                        .containsExactly("JsonName"));
        assertThat(user.members().stream()
                .filter(ObjectOriented.MethodDeclaration.class::isInstance)
                .map(ObjectOriented.MethodDeclaration.class::cast)
                .filter(method -> method.name().equals("label"))
                .findFirst()
                .orElseThrow()).satisfies(method ->
                assertThat(method.linkedAnnotations()).extracting(CompiledAnnotation::name)
                        .containsExactly("Endpoint"));

        var error = compileFailure(List.of(
                new RawModule("Annotations", "/capy/test", "annotation Entity on class {}", SourceKind.FUNCTIONAL),
                new RawModule("User", "/foo/app", """
                        from /capy/test/Annotations import { Entity }

                        class User {
                            @Entity
                            field name: String = "name"
                        }
                        """, SourceKind.OBJECT_ORIENTED)
        ));

        assertThat(error.message()).contains("Annotation Entity is not valid on field declarations");
    }

    private static CompiledProgram compileSuccess(RawModule module) {
        return compileSuccess(List.of(module));
    }

    private static CompiledProgram compileSuccess(List<RawModule> modules) {
        var result = CapybaraCompiler.INSTANCE.compile(modules, new TreeSet<>());
        if (result instanceof Result.Error<CompiledProgram> error) {
            fail(CompilerErrors.from(error).toString());
        }
        return ((Result.Success<CompiledProgram>) result).value();
    }

    private static CompilerError compileFailure(RawModule module) {
        return compileFailure(List.of(module));
    }

    private static List<String> compileFailureMessages(RawModule module) {
        return compileFailureMessages(List.of(module));
    }

    private static List<String> compileFailureMessages(List<RawModule> modules) {
        var result = CapybaraCompiler.INSTANCE.compile(modules, new TreeSet<>());
        if (result instanceof Result.Success<CompiledProgram> success) {
            fail("Expected compilation to fail but got " + success.value());
        }
        return CompilerErrors.from((Result.Error<CompiledProgram>) result).stream()
                .map(CompilerError::message)
                .toList();
    }

    private static CompilerError compileFailure(List<RawModule> modules) {
        var result = CapybaraCompiler.INSTANCE.compile(modules, new TreeSet<>());
        if (result instanceof Result.Success<CompiledProgram> success) {
            fail("Expected compilation to fail but got " + success.value());
        }
        var errors = CompilerErrors.from((Result.Error<CompiledProgram>) result);
        assertThat(errors).isNotEmpty();
        return errors.first();
    }

    private static CompiledModule module(CompiledProgram compiled, String name) {
        return compiled.modules().stream()
                .filter(module -> module.name().equals(name))
                .findFirst()
                .orElseThrow();
    }

    private static CompiledFunction function(CompiledProgram compiled, String moduleName, String name) {
        return module(compiled, moduleName).functions().stream()
                .filter(function -> function.name().equals(name))
                .findFirst()
                .orElseThrow();
    }
}
