package pl.grzeslowski.capybara.generator;

import pl.grzeslowski.capybara.linker.LinkedProgram;

import java.util.List;

public final class JavaGenerator implements Generator {
    @Override
    public CompiledProgram generate(LinkedProgram program) {
        return new CompiledProgram(List.of());
    }
}
