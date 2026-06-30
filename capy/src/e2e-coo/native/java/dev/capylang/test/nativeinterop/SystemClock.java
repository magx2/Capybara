package dev.capylang.test.nativeinterop;

import dev.capylang.NativeImplementation;
import dev.capylang.test.NativeClock;

@NativeImplementation(qualifier = "system")
public final class SystemClock implements NativeClock {
    @Override
    public long now_millis() {
        return 12345L;
    }
}
