package dev.capylang.generator.java;

public record JavaPackage(String packageName) implements Comparable<JavaPackage> {
    @Override
    public String toString() {
        return packageName;
    }

    @Override
    public int compareTo(JavaPackage o) {
        return packageName.compareTo(o.packageName);
    }
}
