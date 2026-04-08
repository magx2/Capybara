package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledType;
import dev.capylang.compiler.parser.InfixOperator;

public record CompiledInfixExpression(CompiledExpression left,
                                    InfixOperator operator,
                                    CompiledExpression right,
                                    CompiledType type) implements CompiledExpression {
}
