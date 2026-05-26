package dev.capylang.test.nativeinterop;

import dev.capylang.test.NativeClock;

public final class SystemClock implements NativeClock {
    @Override
    public long now_millis() {
        return 12345L;
    }
}
