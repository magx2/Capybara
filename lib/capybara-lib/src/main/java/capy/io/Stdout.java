package capy.io;

public final class Stdout {
    private Stdout() {
    }

    public static void print(String text) {
        System.out.print(text);
    }

    public static void println(String text) {
        System.out.println(text);
    }
}
