package dev.capylang;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Optional;

public final class ConsoleUtil {
    private static InputStream currentIn;
    private static BufferedReader currentReader;

    private ConsoleUtil() {
    }

    public static String print(String text) {
        System.out.print(text);
        return text;
    }

    public static byte print(byte text) {
        System.out.print(text);
        return text;
    }

    public static int print(int text) {
        System.out.print(text);
        return text;
    }

    public static long print(long text) {
        System.out.print(text);
        return text;
    }

    public static float print(float text) {
        System.out.print(text);
        return text;
    }

    public static double print(double text) {
        System.out.print(text);
        return text;
    }

    public static boolean print(boolean text) {
        System.out.print(text);
        return text;
    }

    public static List<Byte> print(List<Byte> text) {
        System.out.writeBytes(toByteArray(text));
        return text;
    }

    public static String println(String text) {
        System.out.println(text);
        return text;
    }

    public static byte println(byte text) {
        System.out.println(text);
        return text;
    }

    public static int println(int text) {
        System.out.println(text);
        return text;
    }

    public static long println(long text) {
        System.out.println(text);
        return text;
    }

    public static float println(float text) {
        System.out.println(text);
        return text;
    }

    public static double println(double text) {
        System.out.println(text);
        return text;
    }

    public static boolean println(boolean text) {
        System.out.println(text);
        return text;
    }

    public static List<Byte> println(List<Byte> text) {
        System.out.writeBytes(toByteArray(text));
        System.out.println();
        return text;
    }

    public static String printError(String text) {
        System.err.print(text);
        return text;
    }

    public static byte printError(byte text) {
        System.err.print(text);
        return text;
    }

    public static int printError(int text) {
        System.err.print(text);
        return text;
    }

    public static long printError(long text) {
        System.err.print(text);
        return text;
    }

    public static float printError(float text) {
        System.err.print(text);
        return text;
    }

    public static double printError(double text) {
        System.err.print(text);
        return text;
    }

    public static boolean printError(boolean text) {
        System.err.print(text);
        return text;
    }

    public static List<Byte> printError(List<Byte> text) {
        System.err.writeBytes(toByteArray(text));
        return text;
    }

    public static String printlnError(String text) {
        System.err.println(text);
        return text;
    }

    public static byte printlnError(byte text) {
        System.err.println(text);
        return text;
    }

    public static int printlnError(int text) {
        System.err.println(text);
        return text;
    }

    public static long printlnError(long text) {
        System.err.println(text);
        return text;
    }

    public static float printlnError(float text) {
        System.err.println(text);
        return text;
    }

    public static double printlnError(double text) {
        System.err.println(text);
        return text;
    }

    public static boolean printlnError(boolean text) {
        System.err.println(text);
        return text;
    }

    public static List<Byte> printlnError(List<Byte> text) {
        System.err.writeBytes(toByteArray(text));
        System.err.println();
        return text;
    }

    public static synchronized Optional<String> readLine() {
        try {
            return Optional.ofNullable(reader().readLine());
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    private static BufferedReader reader() {
        if (currentReader == null || currentIn != System.in) {
            currentIn = System.in;
            currentReader = new BufferedReader(new InputStreamReader(currentIn, StandardCharsets.UTF_8));
        }
        return currentReader;
    }

    private static byte[] toByteArray(List<Byte> values) {
        var bytes = new byte[values.size()];
        for (var i = 0; i < values.size(); i++) {
            bytes[i] = values.get(i);
        }
        return bytes;
    }
}
