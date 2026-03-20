package pl.grzeslowski.capybara.compiler;

import java.util.Map;
import java.util.Set;

public record CompiledModule(
        String name,
        String path,
        Map<String, GenericDataType> types,
        Set<CompiledFunction> functions,
        Set<StaticImport> staticImports) {
    public record StaticImport(String className, String memberName) {
    }
}
