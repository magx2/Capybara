package dev.capylang.compiler;

import capy.lang.Result;
import dev.capylang.CapybaraException;
import dev.capylang.NativeImplementation;
import dev.capylang.PathUtil;

import java.nio.file.Path;

@NativeImplementation
public final class NativeLinkedProgramIO implements LinkedProgramIO {
    @Override
    public LinkedProgramReadResult read_program(Object inputDir, boolean requireModules) {
        Path javaPath = null;
        try {
            javaPath = PathUtil.toJavaPath((capy.io.Path) inputDir);
            return new LinkedProgramReadResult(new Result.Success<>(LinkedJsonCodec.readProgram(javaPath, requireModules)));
        } catch (RuntimeException exception) {
            return new LinkedProgramReadResult(linkedError("read", javaPath, inputDir, exception));
        }
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
