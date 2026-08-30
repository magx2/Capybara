package dev.capylang.compiler.parser;

/** Reports invalid Capybara source syntax. */
public final class ParserException extends IllegalArgumentException {
    public ParserException(String message) {
        super(message);
    }
}
