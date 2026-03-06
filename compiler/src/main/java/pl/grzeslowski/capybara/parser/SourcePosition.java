package pl.grzeslowski.capybara.parser;

import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.TerminalNode;

import java.util.Optional;

public record SourcePosition(int line, int column, Optional<Integer> length) {
    public static final SourcePosition EMPTY = new SourcePosition(0, 0, Optional.empty());

    public SourcePosition {
        if (line < 0) {
            throw new IllegalArgumentException("line must be >= 0");
        }
        if (column < 0) {
            throw new IllegalArgumentException("column must be >= 0");
        }
        //noinspection OptionalAssignedToNull
        if (length == null) {
            throw new IllegalArgumentException("length cannot be null");
        }
        if (length.isPresent() && length.get() < 0) {
            throw new IllegalArgumentException("length must be >= 0 when provided");
        }
    }

    public SourcePosition(int line, int column, int length) {
        this(line, column, Optional.of(length));
    }

    public static SourcePosition of(Token token) {
        if (token == null) {
            return EMPTY;
        }
        var start = token.getStartIndex();
        var stop = token.getStopIndex();
        var length = start >= 0 && stop >= start ? Optional.of(stop - start + 1) : Optional.<Integer>empty();
        return new SourcePosition(token.getLine(), token.getCharPositionInLine(), length);
    }

    public static Optional<SourcePosition> of(ParserRuleContext context) {
        if (context == null) {
            return Optional.empty();
        }
        var start = context.getStart();
        var stop = context.getStop();
        if (start == null) {
            return Optional.empty();
        }
        var line = start.getLine();
        var column = start.getCharPositionInLine();
        if (stop == null || start.getStartIndex() < 0 || stop.getStopIndex() < start.getStartIndex()) {
            return Optional.of(new SourcePosition(line, column, Optional.empty()));
        }
        return Optional.of(new SourcePosition(line, column, stop.getStopIndex() - start.getStartIndex() + 1));
    }

    public static SourcePosition of(TerminalNode node) {
        if (node == null) {
            return EMPTY;
        }
        return of(node.getSymbol());
    }
}
