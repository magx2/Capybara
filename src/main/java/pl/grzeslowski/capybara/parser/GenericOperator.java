package pl.grzeslowski.capybara.parser;

import java.util.Arrays;

public enum GenericOperator implements InfixOperator {
    PLUS("+"),
    MINUS("-"),
    MUL("*"),
    DIV("/"),
    CARET("^");

    private final String symbol;

    GenericOperator(String symbol) {
        this.symbol = symbol;
    }

    @Override
    public String toString() {
        return '"' + symbol + '"';
    }

    public static GenericOperator fromSymbol(String symbol) {
        return Arrays.stream(values())
                .filter(s -> s.symbol.equals(symbol))
                .findAny()
                .orElseThrow(() -> new IllegalArgumentException("Unknown operator: " + symbol));
    }
}
