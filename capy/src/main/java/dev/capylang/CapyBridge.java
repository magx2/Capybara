package dev.capylang;

import capy.lang.Effect;
import capy.lang.Program;

import java.io.PrintStream;
import java.nio.file.Path;
import java.util.Optional;

final class CapyBridge {
    private CapyBridge() {
    }

    static Effect<Program> exitOnFailed(Effect<Program> effect) {
        return Effect.delay(() -> {
            var program = effect.unsafeRun();
            if (program instanceof Program.Failed failed) {
                System.exit(failed.exit_code());
            }
            return program;
        });
    }

    static boolean isNull(Object value) {
        return value == null;
    }

    static Optional<String> optionalPath(Path path) {
        return path == null ? Optional.empty() : Optional.of(path.toString());
    }

    static boolean pathIsNull(Path path) {
        return path == null;
    }

    static String pathString(Path path) {
        return path.toString();
    }

    static int printErrorAndFail(PrintStream errors, String message) {
        errors.println(message);
        return Program.DEFAULT_FAILED_EXIT_CODE;
    }

    static int runProgram(Effect<Program> effect, PrintStream errors) {
        try {
            var program = effect.unsafeRun();
            if (program instanceof Program.Failed failed) {
                return failed.exit_code();
            }
            return 0;
        } catch (RuntimeException exception) {
            errors.println(exception.getMessage());
            return Program.DEFAULT_FAILED_EXIT_CODE;
        }
    }

    static dev.capylang.cli.Capy.LogLevel warnLogLevel() {
        return dev.capylang.cli.Capy.LogLevel.WARN;
    }
}
