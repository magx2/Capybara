package dev.capylang.compiler;

import dev.capylang.CapybaraException;
import dev.capylang.NativeImplementation;

import java.nio.file.Path;
import java.util.Collections;
import java.util.Comparator;
import java.util.Set;
import java.util.TreeSet;

@NativeImplementation
public final class NativeLinkedProgramReader implements LinkedProgramReader {
    private static final Comparator<CompiledModule> COMPILED_MODULE_COMPARATOR = Comparator
            .comparing(NativeLinkedProgramReader::compiledModulePath)
            .thenComparing(CompiledModule::name);

    @Override
    public CompiledProgram read_program(String inputDir, boolean requireModules) {
        try {
            return LinkedJsonCodec.readProgram(path(inputDir), requireModules);
        } catch (RuntimeException exception) {
            throw error(rootCauseMessage(exception));
        }
    }

    @Override
    public Set<CompiledModule> read_libraries(String libs) {
        try {
            var modules = new TreeSet<>(COMPILED_MODULE_COMPARATOR);
            for (String rawLibrary : libs.split(",")) {
                var library = rawLibrary.trim();
                if (!library.isEmpty()) {
                    modules.addAll(LinkedJsonCodec.readProgram(path(library), false).modules());
                }
            }
            return Collections.unmodifiableSet(modules);
        } catch (RuntimeException exception) {
            throw error(rootCauseMessage(exception));
        }
    }

    private static Path path(String value) {
        if ("~".equals(value)) {
            return Path.of(System.getProperty("user.home"));
        }
        if (value.startsWith("~/")) {
            return Path.of(System.getProperty("user.home")).resolve(value.substring(2));
        }
        return Path.of(value);
    }

    private static String compiledModulePath(CompiledModule module) {
        return module.path().isBlank() ? module.name() : module.path() + "/" + module.name();
    }

    private static CapybaraException error(String message) {
        return new CapybaraException(message);
    }

    private static String rootCauseMessage(Throwable throwable) {
        var cause = throwable;
        while (cause.getCause() != null) {
            cause = cause.getCause();
        }
        var message = cause.getMessage();
        return message == null || message.isBlank() ? cause.getClass().getSimpleName() : message;
    }
}
