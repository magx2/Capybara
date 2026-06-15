package dev.capylang.generator.java;

import java.util.List;

public sealed interface JavaInterface extends Comparable<JavaInterface> permits JavaNormalInterface, JavaSealedInterface {
    JavaType name();

    List<JavaInterfaceMethod> methods();

    List<JavaMethod> defaultMethods();

    @Override
    default int compareTo(JavaInterface other) {
        var nameCompare = name().compareTo(other.name());
        if (nameCompare != 0) {
            return nameCompare;
        }

        return Integer.compare(interfaceRank(this), interfaceRank(other));
    }

    private static int interfaceRank(JavaInterface javaInterface) {
        return switch (javaInterface) {
            case JavaNormalInterface ignored -> 0;
            case JavaSealedInterface ignored -> 1;
        };
    }

    public record JavaInterfaceMethod(String name, JavaType returnType) {
    }
}
