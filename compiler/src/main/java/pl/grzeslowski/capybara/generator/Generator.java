package pl.grzeslowski.capybara.generator;

import pl.grzeslowski.capybara.compiler.OutputType;
import pl.grzeslowski.capybara.compiler.CompiledProgram;

public sealed interface Generator permits JavaGenerator, JavaScriptGenerator, PythonGenerator {
    static Generator findGenerator(OutputType type) {
        return switch (type) {
            case JAVA -> new JavaGenerator();
            case JAVASCRIPT -> new JavaScriptGenerator();
            case PYTHON -> new PythonGenerator();
        };
    }

    GeneratedProgram generate(CompiledProgram program);
}
