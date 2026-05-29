package dev.capylang.compiler;

import capy.lang.Result;

import java.util.Collection;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.regex.Pattern;

public final class CompilerErrors {
    private static final Pattern FORMATTED_ERROR = Pattern.compile("^(.*?) (\\d+):(\\d+): (.*)$");

    private CompilerErrors() {
    }

    public static SortedSet<CompilerError> from(Result.Error<?> error) {
        return from(Results.errorMessage(error));
    }

    public static SortedSet<CompilerError> from(String messages) {
        var errors = new TreeSet<CompilerError>();
        var lines = messages.split("\\R", -1);
        var hasFormattedError = false;
        for (var line : lines) {
            if (!line.isBlank() && parseLine(line) != null) {
                hasFormattedError = true;
                break;
            }
        }
        if (!hasFormattedError) {
            if (!messages.isBlank()) {
                errors.add(new CompilerError(messages));
            }
            return errors;
        }

        CompilerError current = null;
        for (var line : lines) {
            if (line.isBlank()) {
                if (current != null) {
                    current = new CompilerError(
                            current.line(),
                            current.column(),
                            current.file(),
                            current.message() + "\n");
                }
                continue;
            }

            var parsed = parseLine(line);
            if (parsed != null) {
                if (current != null) {
                    errors.add(current);
                }
                current = parsed;
            } else if (current != null) {
                current = new CompilerError(
                        current.line(),
                        current.column(),
                        current.file(),
                        current.message() + "\n" + line);
            } else {
                errors.add(new CompilerError(line));
            }
        }
        if (current != null) {
            errors.add(current);
        }
        return errors;
    }

    private static CompilerError parseLine(String message) {
        var matcher = FORMATTED_ERROR.matcher(message);
        if (!matcher.matches()) {
            return null;
        }

        return new CompilerError(
                Integer.parseInt(matcher.group(2)),
                Integer.parseInt(matcher.group(3)),
                matcher.group(1),
                matcher.group(4));
    }

    public static <T> Result<T> result(CompilerError error) {
        return Results.error(error.toString());
    }

    public static <T> Result<T> result(Collection<CompilerError> errors) {
        return Results.error(messages(errors));
    }

    public static List<String> messages(Collection<CompilerError> errors) {
        return errors.stream()
                .sorted()
                .map(CompilerError::toString)
                .toList();
    }
}
