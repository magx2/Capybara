package dev.capylang.compiler;

import java.util.List;

public record CompiledAnnotation(
        String name,
        String packageName,
        String packagePath,
        List<CompiledAnnotationArgument> arguments
) {
    public CompiledAnnotation {
        arguments = arguments == null ? List.of() : List.copyOf(arguments);
    }
}
