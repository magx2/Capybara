package dev.capylang.test.nativeinterop;

import dev.capylang.NativeImplementation;
import dev.capylang.test.Clock;

@NativeImplementation(qualifier = "system")
public final class SystemClock implements Clock {
    @Override
    public long now_millis() {
        return 12345L;
    }
}
