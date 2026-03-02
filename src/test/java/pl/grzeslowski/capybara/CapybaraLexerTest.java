package pl.grzeslowski.capybara;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class CapybaraLexerTest {
    @Test
    @DisplayName("should ")
    void simple() {
        String input = """
                fun test_if(x: int): string =
                    if x > 0 then "positive"
                    else if x < 0 then "negative"
                    else "zero"
                """;
        var tokens = new CapybaraLexer().lex(input);
        var functional = new CapybaraParser().parse(tokens);
        System.out.println(functional);
    }
}
