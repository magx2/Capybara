package dev.capylang.generator.java;

import java.util.Map;
import java.util.Set;
import java.util.SortedSet;

public record JavaClass(
        SortedSet<JavaAnnotation> annotations,
        JavaType name,
        JavaPackage javaPackage,
        SortedSet<String> staticImports,
        SortedSet<JavaConst> staticConsts,
        SortedSet<JavaMethod> staticMethods,
        SortedSet<JavaInterface> interfaces,
        SortedSet<JavaRecord> records,
        SortedSet<JavaEnum> enums,
        Map<String, String> enumValueOwnerOverrides) {
}
