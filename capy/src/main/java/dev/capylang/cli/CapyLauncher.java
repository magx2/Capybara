package dev.capylang.cli;

import capy.lang.Program;
import dev.capylang.compiler.parser.ParserException;

import java.util.List;

/** Entry point for the packaged Capybara CLI. */
public final class CapyLauncher {
    private CapyLauncher() {
    }

    public static void main(String[] args) {
        try {
            run(args);
        } catch (ParserException exception) {
            System.err.println(exception.getMessage());
            System.exit(1);
        }
    }

    private static void run(String[] args) {
        if (args.length > 0 && "test".equals(args[0])) {
            System.exit(TestCommand.execute(java.util.Arrays.copyOfRange(args, 1, args.length)));
        }
        var program = Capy.main(List.of(args)).unsafeRun();
        if (program instanceof Program.Failed failed) {
            System.exit(failed.exit_code());
        }
    }
}
