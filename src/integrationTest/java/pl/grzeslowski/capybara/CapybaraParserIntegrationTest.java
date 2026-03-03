package pl.grzeslowski.capybara;

import org.junit.jupiter.api.Test;
import pl.grzeslowski.capybara.parser.*;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

class CapybaraParserIntegrationTest {
    @Test
    void algebraicTypes() {
        String input = """
                // algebraic type
                type Shape = Circle | Rectangle
                data Circle { radius: float }
                data Rectangle { width: float, height: float }
                
                fun area(shape: Shape): float =
                    match shape with
                    | Circle { radius } => 3.14 * radius ^ 2
                    | Rectangle { width, height } => width * height
                
                fun da_vinci(shape: Shape): Shape =
                    match shape with
                    | Circle { radius } => Rectangle { width : radius * 2, height : radius * 2 }
                    | Rectangle { width, height } => Circle { radius : (width + height) / 4 }
                """;

        var program = new CapybaraParser().parseFunctional(input);
        System.out.println(program);
    }
    @Test
    void parsesProgramIntoExpectedFunctions() {
        String input = """
                fun classify(x: int): string =
                    if x > 0 then "positive"
                    else "non-positive"
                fun always_true(): bool = true
                
                // algebraic type
                type Shape = Circle | Rectangle
                data Circle { radius: float }
                data Rectangle { width: float, height: float }
                
                fun area(shape: Shape): float =
                    match shape with
                    | Circle { radius } => 3.14 * radius * radius
                    | Rectangle { width, height } => width * height
                
                fun da_vinci(shape: Shape): Shape =
                    match shape with
                    | Circle { radius } => Rectangle { width = radius * 2, height = radius * 2 }
                    | Rectangle { width, height } => Circle { radius = (width + height) / 4 }
                """;

        var program = new CapybaraParser().parseFunctional(input);
        var functionsByName = program.definitions().stream()
                .map(Function.class::cast)
                .collect(java.util.stream.Collectors.toMap(Function::name, function -> function));

        assertEquals(2, functionsByName.size());

        var classify = functionsByName.get("classify");
        assertEquals("classify", classify.name());
        assertEquals(Optional.of(new Type("string")), classify.returnType());
        assertEquals(1, classify.parameters().size());
        assertEquals(new Parameter(Type.INT, "x"), classify.parameters().getFirst());
        assertInstanceOf(IfExpression.class, classify.expression());

        var alwaysTrue = functionsByName.get("always_true");
        assertEquals("always_true", alwaysTrue.name());
        assertEquals(Optional.of(Type.BOOL), alwaysTrue.returnType());
        assertEquals(0, alwaysTrue.parameters().size());
        assertEquals(BooleanValue.TRUE, alwaysTrue.expression());
    }
}
