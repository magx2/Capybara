package pl.grzeslowski.capybara.parser;

import java.util.Arrays;

public enum BoolInfixOperator implements InfixOperator {
    GT(">"), LT("<"), EQUAL("=="), NOTEQUAL("!="), LE("<="), GE(">=");
    private final String symbol;

    BoolInfixOperator(String symbol) {
        this.symbol = symbol;
    }

    @Override
    public String toString() {
        return '"' + symbol + '"';
    }

    public static BoolInfixOperator fromSymbol(String symbol) {
        return Arrays.stream(BoolInfixOperator.values())
                .filter(s -> s.symbol.equals(symbol))
                .findAny()
                .orElseThrow(() -> new IllegalArgumentException("Unknown operator: " + symbol));
    }
}
