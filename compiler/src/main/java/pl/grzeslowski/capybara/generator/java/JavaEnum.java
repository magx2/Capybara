package pl.grzeslowski.capybara.generator.java;

import java.util.Set;

public record JavaEnum(JavaType name, Set<JavaType> implementInterfaces) {
}
