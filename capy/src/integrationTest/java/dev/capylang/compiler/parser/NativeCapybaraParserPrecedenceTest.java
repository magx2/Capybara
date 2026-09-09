package dev.capylang.compiler.parser;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class NativeCapybaraParserPrecedenceTest {
    @Test
    void shouldBindFieldAccessBeforeLogicalNot() {
        var body = functionBody("fun Box.negate(): bool = !this.value");

        assertThat(body).isInstanceOf(Expression.UnaryExpression.class);
        var unary = (Expression.UnaryExpression) body;
        assertThat(unary.operator()).isEqualTo("!");
        assertThat(unary.expression()).isInstanceOf(Expression.FieldAccessExpression.class);

        var access = (Expression.FieldAccessExpression) unary.expression();
        assertThat(access.name()).isEqualTo("value");
        assertThat(access.receiver()).isInstanceOf(Expression.VariableExpression.class);
        assertThat(((Expression.VariableExpression) access.receiver()).name()).isEqualTo("this");
    }

    @Test
    void shouldBindMethodCallBeforeLogicalNot() {
        var body = functionBody("fun negate(box: Box): bool = !box.value()");

        assertThat(body).isInstanceOf(Expression.UnaryExpression.class);
        var unary = (Expression.UnaryExpression) body;
        assertThat(unary.expression()).isInstanceOf(Expression.MethodCallExpression.class);

        var call = (Expression.MethodCallExpression) unary.expression();
        assertThat(call.name()).isEqualTo("value");
        assertThat(call.receiver()).isInstanceOf(Expression.VariableExpression.class);
    }

    @Test
    void shouldBindFieldAccessBeforeLogicalNotInLambda() {
        var body = functionBody("fun negate(box: Box): () => bool = () => !box.value");

        assertThat(body).isInstanceOf(Expression.LambdaExpression.class);
        var lambda = (Expression.LambdaExpression) body;
        assertThat(lambda.body()).isInstanceOf(Expression.UnaryExpression.class);

        var unary = (Expression.UnaryExpression) lambda.body();
        assertThat(unary.expression()).isInstanceOf(Expression.FieldAccessExpression.class);
    }

    private static Expression functionBody(String function) {
        var source = """
                data Box { value: bool }

                %s
                """.formatted(function);
        var module = new RawModule("Precedence", "/sample", source, SourceKind.FUNCTIONAL);
        var parsed = new NativeCapybaraParser().parse(List.of(module)).modules().getFirst();

        return parsed.definitions().stream()
                .filter(Definition.FunctionDefinition.class::isInstance)
                .map(Definition.FunctionDefinition.class::cast)
                .map(Definition.FunctionDefinition::function)
                .filter(declaration -> declaration.name().endsWith("negate"))
                .findFirst()
                .orElseThrow()
                .body();
    }
}
