package dev.capylang.generator.java;

public record JavaType(String name) implements Comparable<JavaType> {
    @Override
    public String toString() {
        return name;
    }

    @Override
    public int compareTo(JavaType o) {
        return name.compareTo(o.name);
    }

}
