package dev.capylang.compiler;

import capy.lang.Result;
import dev.capylang.CapybaraException;
import dev.capylang.NativeImplementation;
import dev.capylang.PathUtil;

import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

@NativeImplementation
public final class NativeLinkedProgramIO implements LinkedProgramIO {
    private static final ThreadLocal<Map<String, CompiledModule>> LINKED_MODULES =
            ThreadLocal.withInitial(LinkedHashMap::new);

    @Override
    public LinkedProgramReadResult read_program(Object inputDir, boolean requireModules) {
        Path javaPath = null;
        try {
            javaPath = PathUtil.toJavaPath((capy.io.Path) inputDir);
            var program = LinkedJsonCodec.readProgram(javaPath, requireModules);
            rememberLinkedModules(program.modules());
            return new LinkedProgramReadResult(new Result.Success<>(program));
        } catch (RuntimeException exception) {
            return new LinkedProgramReadResult(linkedError("read", javaPath, inputDir, exception));
        }
    }

    static List<CompiledModule> linkedModules() {
        return List.copyOf(LINKED_MODULES.get().values());
    }

    private static void rememberLinkedModules(List<CompiledModule> modules) {
        var linked = LINKED_MODULES.get();
        for (var module : modules) {
            linked.put(modulePath(module), module);
        }
    }

    private static String modulePath(CompiledModule module) {
        return module.path().isBlank() ? module.name() : module.path() + "/" + module.name();
    }

    private static <T> Result<T> linkedError(String operation, Path javaPath, Object capyPath, Exception exception) {
        var path = javaPath == null ? String.valueOf(capyPath) : javaPath.toString();
        var message = "Unable to " + operation + " linked program `" + path + "`: " + safeMessage(exception);
        return new Result.Error<>(new CapybaraException(message, exception));
    }

    private static String safeMessage(Exception exception) {
        var message = exception.getMessage();
        return message == null || message.isBlank() ? exception.getClass().getSimpleName() : message;
    }
}
