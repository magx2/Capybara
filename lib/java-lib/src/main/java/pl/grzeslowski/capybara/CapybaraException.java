package pl.grzeslowski.capybara;

import java.io.Serial;

public class CapybaraException extends RuntimeException {
    @Serial
    private static final long serialVersionUID = -7188357179233293932L;

    public CapybaraException() {
    }

    public CapybaraException(String message) {
        super(message);
    }

    public CapybaraException(String message, Throwable cause) {
        super(message, cause);
    }

    public CapybaraException(Throwable cause) {
        super(cause);
    }

    public CapybaraException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
