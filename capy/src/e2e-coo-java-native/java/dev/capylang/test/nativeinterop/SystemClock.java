package dev.capylang.test.nativeinterop;

import dev.capylang.test.Clock;

public final class SystemClock implements Clock {
    @Override
    public long now_millis() {
        return 987654321L;
    }
}
