package pl.grzeslowski.capybara.parser;

import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.TokenStream;

public class CapybaraLexer {
    public TokenStream lex(String input) {
        var lexer = new pl.grzeslowski.capybara.parser.antlr.FunctionalLexer(CharStreams.fromString(input));
        var tokenStream = new CommonTokenStream(lexer);
        tokenStream.fill();
        return tokenStream;
    }

    public record Token(String type, String text) {}
}
