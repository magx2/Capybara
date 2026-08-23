package dev.capylang;

public interface CapybaraUtil {
    static int power(int base, int exponent) {
        int result = 1;

        while (exponent > 0) {
            if ((exponent & 1) == 1) {
                result *= base;
            }
            base *= base;
            exponent >>= 1;
        }

        return result;
    }

    static double power(double base, int exponent) {
        double result = 1;

        while (exponent > 0) {
            if ((exponent & 1) == 1) {
                result *= base;
            }
            base *= base;
            exponent >>= 1;
        }

        return result;
    }

    static float power(float base, int exponent) {
        return (float) power((double) base, exponent);
    }

    static short power(short base, int exponent) {
        return (short) power((int) base, exponent);
    }

    static byte power(byte base, int exponent) {
        return (byte) power((int) base, exponent);
    }
}
