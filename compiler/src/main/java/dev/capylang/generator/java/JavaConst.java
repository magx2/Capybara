package dev.capylang.generator.java;

import dev.capylang.compiler.expression.CompiledExpression;

import java.util.Comparator;
import java.util.List;

public record JavaConst(
        String name,
        boolean isPrivate,
        JavaType type,
        CompiledExpression expression,
        List<String> comments) implements Comparable<JavaConst> {
    @Override
    public int compareTo(JavaConst o) {
        return Comparator.comparing(JavaConst::name)
                .thenComparing(JavaConst::type)
                .compare(this, o);
    }
}
