package performance.sample.nativeimpl;

import dev.capylang.NativeImplementation;
import performance.sample.nativeinterop.NativeClock;

@NativeImplementation(qualifier = "fixed")
public final class FixedClock implements NativeClock {
    @Override
    public long fixed_millis() {
        return 4242L;
    }
}
