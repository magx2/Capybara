package pl.grzeslowski.capybara.generator;

import pl.grzeslowski.capybara.compiler.LinkedProgram;

import java.util.List;

public final class JavaScriptGenerator implements Generator {
    @Override
    public CompiledProgram generate(LinkedProgram program) {
        return new CompiledProgram(List.of());
    }
}
