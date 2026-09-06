package dev.capylang.generator;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class GeneratedJavaPrunerTest {
    @Test
    void removesUnusedGeneratedSupportMembers() {
        var source = """
                public final class UIProvider {
                    private UIProvider() {}

                    @FunctionalInterface
                    private interface __CapyFunction3<A, B, C, R> {
                        R apply(A a, B b, C c);
                    }

                    public static Object build_ui__5_0() {
                        return "{ __capy_pow_int }";
                    }

                    private static int __capy_pow_int(int base, int exponent) {
                        return base;
                    }
                }
                """;

        var generated = JavaGenerator.generatedModule(java.util.Map.of(
                "relativePath", "paper_soccer/ui/UIProvider.java",
                "code", source
        ));

        assertThat(generated.code())
                .contains("build_ui__5_0", "{ __capy_pow_int }")
                .doesNotContain("__CapyFunction3", "private static int __capy_pow_int");
    }

    @Test
    void retainsTransitivelyRequiredSupportMembersOnly() {
        var source = """
                public final class Values {
                    public static Object value() {
                        return __capy_outer();
                    }

                    private static Object __capy_outer() {
                        return __capy_inner();
                    }

                    private static Object __capy_inner() {
                        return new Object();
                    }

                    private static Object __capy_unused() {
                        return new Object();
                    }
                }
                """;

        assertThat(GeneratedJavaPruner.prune(source))
                .contains("__capy_outer", "__capy_inner")
                .doesNotContain("__capy_unused");
    }

    @Test
    void retainsOnlyRequiredFunctionInterfaceArity() {
        var source = """
                public final class Functions {
                    @FunctionalInterface
                    private interface __CapyFunction3<A, B, C, R> {
                        R apply(A a, B b, C c);
                    }

                    @FunctionalInterface
                    private interface __CapyFunction4<A, B, C, D, R> {
                        R apply(A a, B b, C c, D d);
                    }

                    public static __CapyFunction3<Integer, Integer, Integer, Integer> sum() {
                        return (a, b, c) -> a + b + c;
                    }
                }
                """;

        assertThat(GeneratedJavaPruner.prune(source))
                .contains("__CapyFunction3")
                .doesNotContain("__CapyFunction4");
    }
}
