package dev.capylang.test.nativeinterop;

import dev.capylang.test.Clock;
import dev.capylang.test.NativeClock;

public final class SystemClock implements Clock, NativeClock {
    @Override
    public long now_millis() {
        return 12345L;
    }
}
