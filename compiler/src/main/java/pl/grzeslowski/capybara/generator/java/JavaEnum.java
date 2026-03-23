package pl.grzeslowski.capybara.generator.java;

import java.util.Set;

public record JavaEnum(JavaType name, Set<JavaType> implementInterfaces, java.util.List<String> values) implements Comparable<JavaEnum> {
    @Override
    public int compareTo(JavaEnum o) {
        return name.compareTo(o.name);
    }
}
