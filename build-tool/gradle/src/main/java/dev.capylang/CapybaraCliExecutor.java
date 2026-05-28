package dev.capylang;

import capy.lang.Program;

import java.io.PrintStream;
import java.util.List;

final class CapybaraCliExecutor {
    private static final Object IO_LOCK = new Object();

    private CapybaraCliExecutor() {
    }

    static int execute(List<String> args, PrintStream out, PrintStream err) {
        synchronized (IO_LOCK) {
            var originalOut = System.out;
            var originalErr = System.err;
            try {
                System.setOut(out);
                System.setErr(err);
                return exitCode(Capy.execute(List.copyOf(args)).unsafeRun());
            } finally {
                out.flush();
                err.flush();
                System.setOut(originalOut);
                System.setErr(originalErr);
            }
        }
    }

    private static int exitCode(Program program) {
        if (program instanceof Program.Failed failed) {
            return failed.exit_code();
        }
        return 0;
    }
}
