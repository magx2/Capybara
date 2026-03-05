package pl.grzeslowski.capybara.generator.java;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import pl.grzeslowski.capybara.compiler.Module;
import pl.grzeslowski.capybara.compiler.Program;
import pl.grzeslowski.capybara.linker.*;
import pl.grzeslowski.capybara.parser.CapybaraParser;

import java.util.Collection;
import java.util.List;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

import static java.util.stream.Collectors.joining;
import static org.assertj.core.api.Assertions.assertThat;

class JavaExpressionEvaluatorTest {
    static {
        var rootLogger = Logger.getLogger("");
        rootLogger.setLevel(Level.FINE);
        for (Handler handler : rootLogger.getHandlers()) {
            handler.setLevel(Level.FINE);
        }
    }

    @Test
    @DisplayName("should evaluate wild if")
    void wildIf() {
        // given
        var fun = """
                fun some_if(x: int): int =
                    let a = "unsued?"
                    if ({
                        let a = x * 2
                        a > 2
                    }) then {
                        let a = x * x
                        "I'm happy " + a
                       } else {
                        let a = x / 2
                        "I'm not happy " + a
                        }
                """;
        var functional = CapybaraParser.INSTANCE.parseFunctional(fun);
        var programValueOrError = CapybaraLinker.INSTANCE.link(new Program(List.of(new Module("test", "/foo/boo", functional))));
        if (programValueOrError instanceof ValueOrError.Error<LinkedProgram> er) {
            throw new AssertionError(er.errors()
                    .stream()
                    .map(ValueOrError.Error.SingleError::message)
                    .collect(joining(", ")));
        }
        var program = ((ValueOrError.Value<LinkedProgram>) programValueOrError).value();
        var expression = program.modules()
                .stream()
                .map(LinkedModule::functions)
                .flatMap(Collection::stream)
                .filter(f -> f.name().equals("some_if"))
                .map(LinkedFunction::expression)
                .peek(LinkedExpressionPrinter::printExpression)
                .findAny()
                .orElseThrow();


        // when
        var evaluated = JavaExpressionEvaluator.evaluateExpression(expression);

        // then
        assertThat(evaluated).isEqualTo("""
                var a = "unsued?";
                var a_j1 = x*2;
                var a_j2 = x*x;
                var a_j3 = x/2;
                return (a_j1>2)? ("I'm happy "+a_j2): ("I'm not happy "+a_j3)
                """);
    }

}
