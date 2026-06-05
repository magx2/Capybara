package dev.capylang.generator.java;

public record JavaAnnotation(String value) implements Comparable<JavaAnnotation> {
    public static JavaAnnotation generatedAnnotation() {
        return new JavaAnnotation("@javax.annotation.processing.Generated(\"Capybara Compiler\")");
    }

    @Override
    public int compareTo(JavaAnnotation o) {
        return value.compareTo(o.value);
    }

    @Override
    public String toString() {
        return value;
    }
}
