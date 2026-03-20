package pl.grzeslowski.capybara.generator;

import pl.grzeslowski.capybara.compiler.CompiledProgram;

import java.util.List;

public final class JavaScriptGenerator implements Generator {
    @Override
    public GeneratedProgram generate(CompiledProgram program) {
        return new GeneratedProgram(List.of());
    }
}
