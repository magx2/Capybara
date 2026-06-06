package dev.capylang.compiler.parser;

import java.util.List;

public interface CapybaraParser {
    ParsedProgram parse(List<RawModule> modules);
}
