package pl.grzeslowski.capybara.compiler;

import java.util.List;

public record CompiledProgram(List<CompiledModule> modules) {
}
