package pl.grzeslowski.capybara.parser;

import java.util.Arrays;

public enum InfixOperator {
    PLUS("+"),
    MINUS("-"),
    MUL("*"),
    DIV("/"),
    CARET("^"),
    // bool
    GT(">"),
    LT("<"),
    EQUAL("=="),
    NOTEQUAL("!="),
    LE("<="),
    GE(">=");

    private final String symbol;

    InfixOperator(String symbol) {
        this.symbol = symbol;
    }

    @Override
    public String toString() {
        return '"' + symbol + '"';
    }

    public static InfixOperator fromSymbol(String symbol) {
        return Arrays.stream(values())
                .filter(s -> s.symbol.equals(symbol))
                .findAny()
                .orElseThrow(() -> new IllegalArgumentException("Unknown operator: " + symbol));
    }

    public String symbol() {
        return symbol;
    }
}
