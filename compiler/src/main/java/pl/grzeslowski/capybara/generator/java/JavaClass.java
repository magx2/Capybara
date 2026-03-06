package pl.grzeslowski.capybara.generator.java;

import java.util.Set;

public record JavaClass(
        Set<JavaAnnotation> annotations,
        JavaType name,
        JavaPackage javaPackage,
        Set<String> staticImports,
        Set<JavaMethod> staticMethods,
        Set<JavaInterface> interfaces,
        Set<JavaRecord> records,
        Set<JavaEnum> enums) {
}
