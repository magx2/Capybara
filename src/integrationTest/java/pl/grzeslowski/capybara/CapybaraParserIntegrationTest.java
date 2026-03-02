package pl.grzeslowski.capybara;

import org.junit.jupiter.api.Test;
import pl.grzeslowski.capybara.parser.*;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

class CapybaraParserIntegrationTest {
    @Test
    void parsesProgramIntoExpectedFunctions() {
        String input = """
                fun classify(x: int): string =
                    if x > 0 then "positive"
                    else "non-positive"
                fun always_true(): bool = true
                """;

        var tokens = new CapybaraLexer().lex(input);
        var program = new CapybaraParser().parseFuntional(tokens);
        var functionsByName = program.definitions().stream()
                .map(Function.class::cast)
                .collect(java.util.stream.Collectors.toMap(Function::name, function -> function));

        assertEquals(2, functionsByName.size());

        var classify = functionsByName.get("classify");
        assertEquals("classify", classify.name());
        assertEquals(Optional.of(new Type("string")), classify.returnType());
        assertEquals(1, classify.parameters().size());
        assertEquals(new Parameter(Type.INT, "x"), classify.parameters().getFirst());
        assertInstanceOf(Expression.IfExpression.class, classify.expression());

        var alwaysTrue = functionsByName.get("always_true");
        assertEquals("always_true", alwaysTrue.name());
        assertEquals(Optional.of(Type.BOOL), alwaysTrue.returnType());
        assertEquals(0, alwaysTrue.parameters().size());
        assertEquals(Expression.BooleanValue.TRUE, alwaysTrue.expression());
    }
}
