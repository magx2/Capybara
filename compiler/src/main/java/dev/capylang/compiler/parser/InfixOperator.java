package dev.capylang.compiler.parser;

import java.util.Arrays;

public enum InfixOperator {
    PLUS("+"),
    MINUS("-"),
    MUL("*"),
    DIV("/"),
    MOD("%"),
    POWER("^"),
    BITWISE_AND(".and."),
    BITWISE_NAND(".nand."),
    BITWISE_OR(".or."),
    BITWISE_XOR(".xor."),
    BITWISE_NOT(".not."),
    // bool
    GT(">"),
    LT("<"),
    EQUAL("=="),
    NOTEQUAL("!="),
    LE("<="),
    GE(">="),
    AND("&"),
    QUESTION("?"),
    PIPE("|"),
    PIPE_MINUS("|-"),
    PIPE_FLATMAP("|*"),
    PIPE_REDUCE("|>"),
    PIPE_ANY("|any?"),
    PIPE_ALL("|all?");

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

    public String javaSymbol() {
        return switch (this) {
            case BITWISE_NOT -> "~";
            case BITWISE_AND -> "&";
            case BITWISE_NAND -> "&";
            case BITWISE_OR -> "|";
            case BITWISE_XOR -> "^";
            default -> symbol;
        };
    }

    public int precedence() {
        return switch (this) {
            case BITWISE_NOT -> 8;
            case POWER -> 7;
            case MUL, DIV, MOD -> 6;
            case PLUS, MINUS, BITWISE_AND, BITWISE_NAND, BITWISE_OR, BITWISE_XOR -> 5;
            case GT, LT, LE, GE -> 4;
            case EQUAL, NOTEQUAL -> 3;
            case AND -> 2;
            case QUESTION -> 2;
            case PIPE, PIPE_MINUS, PIPE_FLATMAP, PIPE_REDUCE, PIPE_ANY, PIPE_ALL -> 1;
        };
    }
}

