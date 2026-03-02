package pl.grzeslowski.capybara.linker;

import pl.grzeslowski.capybara.compiler.Program;

public class CapybaraLinker {
    public static final CapybaraLinker INSTANCE = new CapybaraLinker();

    public LinkedProgram link(Program program) {
        throw new UnsupportedOperationException("CapybaraLinker.link(program)");
    }
}
