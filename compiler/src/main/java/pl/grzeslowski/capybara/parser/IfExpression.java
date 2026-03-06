package pl.grzeslowski.capybara.parser;

public record IfExpression(Expression condition, Expression thenBranch,
                           Expression elseBranch, SourcePosition position) implements Expression {
}

