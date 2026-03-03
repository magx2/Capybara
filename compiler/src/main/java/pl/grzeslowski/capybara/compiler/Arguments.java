package pl.grzeslowski.capybara.compiler;

import java.nio.file.Path;
import java.util.List;

public record Arguments(List<Path> inputs, Path output, OutputType outputType) {
}
