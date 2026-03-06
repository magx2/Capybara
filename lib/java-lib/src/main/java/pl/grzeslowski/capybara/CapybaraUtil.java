package pl.grzeslowski.capybara;

public interface CapybaraUtil {
    // todo write versions for float, double, short, byte
    static int power(int a, int b) {
        int result = 1;

        while (b > 0) {
            if ((b & 1) == 1) {
                result *= a;
            }
            a *= a;
            b >>= 1;
        }

        return result;
    }
}
