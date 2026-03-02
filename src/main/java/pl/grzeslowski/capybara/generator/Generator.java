package pl.grzeslowski.capybara.generator;

import pl.grzeslowski.capybara.linker.LinkedProgram;

public sealed interface Generator permits JavaGenerator, JavaScriptGenerator, PythonGenerator {
    CompiledProgram generate(LinkedProgram program);
}
