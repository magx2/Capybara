package dev.capylang.cli;

import java.io.IOException;
import java.nio.charset.StandardCharsets;

/** Executable launcher that reports the version packaged by the current build. */
public final class CapyMain {
    private CapyMain() {
    }

    public static void main(String... arguments) {
        if (arguments.length == 1 && arguments[0].equals("--version")) {
            System.out.println("Capybara compiler version: " + packagedVersion());
            return;
        }
        Capy.main(arguments);
    }

    private static String packagedVersion() {
        try (var input = CapyMain.class.getResourceAsStream("/capybara-version.txt")) {
            if (input == null) {
                throw new IllegalStateException("Missing capybara-version.txt");
            }
            return new String(input.readAllBytes(), StandardCharsets.UTF_8).trim();
        } catch (IOException exception) {
            throw new IllegalStateException("Unable to read packaged Capybara version", exception);
        }
    }
}
