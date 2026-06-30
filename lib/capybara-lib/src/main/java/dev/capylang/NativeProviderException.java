package dev.capylang;

public final class NativeProviderException extends RuntimeException {
    public NativeProviderException(String message) {
        super(message);
    }

    public NativeProviderException(String message, Throwable cause) {
        super(message, cause);
    }
}
