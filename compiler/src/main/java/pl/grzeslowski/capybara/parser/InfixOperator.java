package pl.grzeslowski.capybara.parser;

import java.util.Arrays;

public enum InfixOperator {
    PLUS("+"),
    MINUS("-"),
    MUL("*"),
    DIV("/"),
    CARET("^"),
    POWER("**"),
    // bool
    GT(">"),
    LT("<"),
    EQUAL("=="),
    NOTEQUAL("!="),
    LE("<="),
    GE(">="),
    QUESTION("?");

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

    public int precedence() {
        return switch (this) {
            case POWER -> 7;
            case MUL, DIV -> 6;
            case PLUS, MINUS -> 5;
            case GT, LT, LE, GE -> 4;
            case EQUAL, NOTEQUAL -> 3;
            case CARET, QUESTION -> 2;
        };
    }
}

