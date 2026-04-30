package dev.capylang.compiler;

import dev.capylang.compiler.expression.CompiledExpression;

import java.util.Comparator;
import java.util.List;

public record CompiledFunction(String name,
                             CompiledType returnType,
                             List<CompiledFunctionParameter> parameters,
                             CompiledExpression expression,
                             List<String> comments,
                             Visibility visibility,
                             boolean programMain,
                             boolean recursive,
                             boolean tailRecursive) implements Comparable<CompiledFunction> {
    public CompiledFunction(String name,
                          CompiledType returnType,
                          List<CompiledFunctionParameter> parameters,
                          CompiledExpression expression,
                          List<String> comments) {
        this(name, returnType, parameters, expression, comments, null, false, false, false);
    }

    public CompiledFunction(String name,
                            CompiledType returnType,
                            List<CompiledFunctionParameter> parameters,
                            CompiledExpression expression,
                            List<String> comments,
                            boolean programMain) {
        this(name, returnType, parameters, expression, comments, null, programMain, false, false);
    }

    public record CompiledFunctionParameter(String name, CompiledType type) {
    }

    @Override
    public int compareTo(CompiledFunction o) {
        return Comparator.comparing(CompiledFunction::name)
                .thenComparing(compareParameters())
                .compare(this, o);
    }

    private Comparator<CompiledFunction> compareParameters() {
        return new Comparator<CompiledFunction>() {
            @Override
            public int compare(CompiledFunction o1, CompiledFunction o2) {
                var left = o1.parameters();
                var right = o2.parameters();
                var sizeCompare = Integer.compare(left.size(), right.size());
                if (sizeCompare != 0) {
                    return sizeCompare;
                }

                for (var i = 0; i < left.size(); i++) {
                    var leftParameter = left.get(i);
                    var rightParameter = right.get(i);

                    var typeCompare = leftParameter.type().toString().compareTo(rightParameter.type().toString());
                    if (typeCompare != 0) {
                        return typeCompare;
                    }

                    var nameCompare = leftParameter.name().compareTo(rightParameter.name());
                    if (nameCompare != 0) {
                        return nameCompare;
                    }
                }
                return 0;
            }
        };
    }

    @Override
    public final boolean equals(Object o) {
        if (!(o instanceof CompiledFunction that)) return false;

        return name.equals(that.name) && parameters.equals(that.parameters);
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }
}
