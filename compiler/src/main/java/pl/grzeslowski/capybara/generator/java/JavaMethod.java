package pl.grzeslowski.capybara.generator.java;

import pl.grzeslowski.capybara.compiler.expression.CompiledExpression;

import java.util.Comparator;
import java.util.List;

public record JavaMethod(
        String name,
        boolean isPrivate,
        boolean programMain,
        List<String> typeParameters,
        JavaType returnType,
        List<JavaFunctionParameter> parameters,
        CompiledExpression expression,
        List<String> comments) implements Comparable<JavaMethod> {
    @Override
    public int compareTo(JavaMethod o) {
        var nameCompare = name.compareTo(o.name);
        if (nameCompare != 0) {
            return nameCompare;
        }

        var sizeCompare = Integer.compare(parameters.size(), o.parameters.size());
        if (sizeCompare != 0) {
            return sizeCompare;
        }

        for (var i = 0; i < parameters.size(); i++) {
            var left = parameters.get(i);
            var right = o.parameters.get(i);

            var typeCompare = left.type().compareTo(right.type());
            if (typeCompare != 0) {
                return typeCompare;
            }

            var generatedNameCompare = left.generatedName().compareTo(right.generatedName());
            if (generatedNameCompare != 0) {
                return generatedNameCompare;
            }

            var sourceNameCompare = left.sourceName().compareTo(right.sourceName());
            if (sourceNameCompare != 0) {
                return sourceNameCompare;
            }
        }

        return returnType.compareTo(o.returnType);
    }

    @Override
    public final boolean equals(Object o) {
        if (!(o instanceof JavaMethod that)) return false;

        return name.equals(that.name)
                && returnType.equals(that.returnType)
                && parameters.equals(that.parameters);
    }

    @Override
    public int hashCode() {
        return java.util.Objects.hash(name, returnType, parameters);
    }

    public record JavaFunctionParameter(JavaType type, String sourceName, String generatedName) {
    }
}
