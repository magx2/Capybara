package pl.grzeslowski.capybara.parser;

public sealed interface InfixOperator permits BoolInfixOperator, GenericOperator {
    String symbol();
}
