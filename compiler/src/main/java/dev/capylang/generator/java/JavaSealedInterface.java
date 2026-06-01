package dev.capylang.generator.java;

import java.util.List;
import java.util.Set;

public record JavaSealedInterface(JavaType name, Set<JavaType> extendInterfaces, List<String> comments, List<JavaInterfaceMethod> methods,
                                  List<String> permits,
                                  List<String> typeParameters,
                                  List<JavaMethod> defaultMethods) implements JavaInterface {
    @Override
    public final boolean equals(Object o) {
        if (!(o instanceof JavaSealedInterface that)) return false;

        return name.equals(that.name);
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }

}
