package dev.capylang.generator.java;

import java.time.ZonedDateTime;

public record JavaAnnotation(String value) implements Comparable<JavaAnnotation> {
    public static JavaAnnotation generatedAnnotation() {
        return new JavaAnnotation("@javax.annotation.processing.Generated(date = \""
                                  + ZonedDateTime.now()
                                  + "\", value = \"Capybara Compiler\")");
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
