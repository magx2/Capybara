package dev.capylang.generator.java;

import java.util.Set;
import java.util.SortedSet;

public record JavaClass(
        SortedSet<JavaAnnotation> annotations,
        JavaType name,
        JavaPackage javaPackage,
        SortedSet<String> staticImports,
        SortedSet<JavaMethod> staticMethods,
        SortedSet<JavaInterface> interfaces,
        SortedSet<JavaRecord> records,
        SortedSet<JavaEnum> enums) {
}
