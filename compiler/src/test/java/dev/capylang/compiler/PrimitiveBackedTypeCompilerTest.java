package dev.capylang.compiler;

import dev.capylang.compiler.expression.CompiledFunctionCall;
import dev.capylang.compiler.expression.CompiledNewData;
import dev.capylang.compiler.expression.CompiledUnwrapExpression;
import dev.capylang.compiler.parser.RawModule;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.TreeSet;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.fail;

class PrimitiveBackedTypeCompilerTest {
    @Test
    void shouldConstructUnwrapAndCallPrimitiveBackedTypeMethods() {
        var compiled = compileSuccess(new RawModule("Ids", "/foo/app", """
                /// User identifier
                type user_id -> int
                type order_id -> int

                const FIRST_USER_ID = user_id! { 1 }

                fun make(id: int): user_id = user_id { id }
                fun unwrap(id: user_id): int = @id
                fun user_id.plus(other: user_id): user_id = user_id! { @this + @other }
                fun user_id.`+`(other: user_id): user_id = user_id! { @this + @other }
                fun call_plus(a: user_id, b: user_id): user_id = a.plus(b)
                fun call_op(a: user_id, b: user_id): user_id = a + b
                """));

        var module = compiled.modules().first();
        assertThat(module.types().get("user_id")).isInstanceOfSatisfying(CompiledPrimitiveBackedType.class, type -> {
            assertThat(type.backingType()).isEqualTo(PrimitiveLinkedType.INT);
            assertThat(type.comments()).containsExactly("User identifier");
        });
        assertThat(module.types().get("order_id")).isInstanceOfSatisfying(CompiledPrimitiveBackedType.class, type ->
                assertThat(type.backingType()).isEqualTo(PrimitiveLinkedType.INT));

        assertThat(function(compiled, "Ids", "make").returnType()).isInstanceOfSatisfying(
                CompiledPrimitiveBackedType.class,
                type -> assertThat(type.name()).isEqualTo("user_id")
        );
        assertThat(function(compiled, "Ids", "make").expression()).isInstanceOfSatisfying(
                CompiledNewData.class,
                newData -> assertThat(newData.type().name()).isEqualTo("user_id")
        );
        assertThat(function(compiled, "Ids", "unwrap").expression()).isInstanceOfSatisfying(
                CompiledUnwrapExpression.class,
                unwrap -> assertThat(unwrap.type()).isEqualTo(PrimitiveLinkedType.INT)
        );
        assertThat(function(compiled, "Ids", "call_plus").expression()).isInstanceOfSatisfying(
                CompiledFunctionCall.class,
                call -> {
                    assertThat(call.name()).isEqualTo("__method__user_id__plus");
                    assertThat(call.returnType().name()).isEqualTo("user_id");
                }
        );
        assertThat(function(compiled, "Ids", "call_op").expression()).isInstanceOfSatisfying(
                CompiledFunctionCall.class,
                call -> {
                    assertThat(call.name()).isEqualTo("__method__user_id__+");
                    assertThat(call.returnType().name()).isEqualTo("user_id");
                }
        );
    }

    @Test
    void shouldSupportPrimitiveAndResultReturningConstructors() {
        var compiled = compileSuccess(List.of(resultModule(), new RawModule("Ids", "/foo/app", """
                from /capy/lang/Result import { * }

                type meter -> double with constructor {
                    value
                }

                type user_id -> int with constructor {
                    if value > 0
                    then Success { value }
                    else Error { message: "bad" }
                }

                fun make_meter(value: double): meter = meter { value }
                fun new_user(id: int): Result[user_id] = user_id { id }
                """)));

        assertThat(function(compiled, "Ids", "make_meter").expression()).isInstanceOfSatisfying(
                CompiledFunctionCall.class,
                call -> {
                    assertThat(call.name()).endsWith(".Ids.__constructor__primitive__meter");
                    assertThat(call.returnType().name()).isEqualTo("meter");
                }
        );
        assertThat(function(compiled, "Ids", "new_user").expression()).isInstanceOfSatisfying(
                CompiledFunctionCall.class,
                call -> {
                    assertThat(call.name()).endsWith(".Ids.__constructor__primitive__user_id");
                    assertThat(call.returnType()).isInstanceOfSatisfying(CompiledDataParentType.class, result -> {
                        assertThat(result.name()).isEqualTo("Result");
                        assertThat(result.typeParameters()).containsExactly("/foo/app/Ids.user_id");
                    });
                }
        );
    }

    @Test
    void shouldRejectRawConstructorOutsideDefiningModule() {
        var error = compileFailure(List.of(
                new RawModule("Types", "/foo/lib", "type user_id -> int"),
                new RawModule("Consumer", "/foo/app", """
                        from /foo/lib/Types import { * }

                        const FIRST_USER_ID = user_id! { 1 }
                        """)
        ));

        assertThat(error.message()).contains("Constructor bypass `user_id! { ... }` can only be used in module `foo/lib/Types`");
    }

    @Test
    void shouldRejectPrimitiveBackedTypeMismatches() {
        assertThat(compileFailure(new RawModule("Ids", "/foo/app", """
                type user_id -> int
                fun takes_foo(id: user_id): user_id = id
                fun bad(): user_id = takes_foo(1)
                """)).message()).contains("Expected `user_id`, got `int`");

        assertThat(compileFailure(new RawModule("Ids", "/foo/app", """
                type user_id -> int
                type order_id -> int
                fun bad(id: user_id): order_id = id
                """)).message()).contains("Expected `order_id`, got `user_id`");
    }

    @Test
    void shouldPassPrimitiveBackedValuesToBackingPrimitivePositions() {
        compileSuccess(List.of(optionModule(), new RawModule("Indexes", "/foo/app", """
                type index -> int with constructor {
                    if value >= 0
                    then value
                    else 0
                }

                fun foo(i: int): int = i + 1
                fun boo(idx: index): int = foo(idx)
                fun baz(s: String, idx: index): Option[String] = s[idx]
                fun widened(idx: index): long = idx
                """)));
    }

    @Test
    void shouldRejectUnwrapForNonPrimitiveBackedValues() {
        var error = compileFailure(new RawModule("Ids", "/foo/app", """
                fun bad(id: int): int = @id
                """));

        assertThat(error.message()).contains("`@` can only unwrap primitive-backed types");
    }

    private static CompiledProgram compileSuccess(RawModule module) {
        return compileSuccess(List.of(module));
    }

    private static CompiledProgram compileSuccess(List<RawModule> modules) {
        var result = CapybaraCompiler.INSTANCE.compile(modules, new TreeSet<>());
        if (result instanceof Result.Error<CompiledProgram> error) {
            fail(error.errors().toString());
        }
        return ((Result.Success<CompiledProgram>) result).value();
    }

    private static Result.Error.SingleError compileFailure(RawModule module) {
        return compileFailure(List.of(module));
    }

    private static Result.Error.SingleError compileFailure(List<RawModule> modules) {
        var result = CapybaraCompiler.INSTANCE.compile(modules, new TreeSet<>());
        if (result instanceof Result.Success<CompiledProgram> success) {
            fail("Expected compilation to fail but got " + success.value());
        }
        var errors = ((Result.Error<CompiledProgram>) result).errors();
        assertThat(errors).isNotEmpty();
        return errors.first();
    }

    private static CompiledFunction function(CompiledProgram program, String moduleName, String functionName) {
        return program.modules().stream()
                .filter(module -> module.name().equals(moduleName))
                .flatMap(module -> module.functions().stream())
                .filter(function -> function.name().equals(functionName))
                .findFirst()
                .orElseThrow(() -> new AssertionError("Function " + moduleName + "." + functionName + " not found"));
    }

    private static RawModule resultModule() {
        return new RawModule("Result", "/capy/lang", """
                union Result[T] = Success[T] | Error
                data Success[T] { value: T }
                data Error { message: String }
                """);
    }

    private static RawModule optionModule() {
        return new RawModule("Option", "/capy/lang", """
                union Option[T] = Some[T] | None
                data Some[T] { value: T }
                single None
                """);
    }
}
