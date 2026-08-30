package performance.sample.nativeimpl;

import dev.capylang.NativeImplementation;
import performance.sample.nativeinterop.NativeHasher;

@NativeImplementation(qualifier = "stable")
public final class StableHasher implements NativeHasher {
    @Override
    public String stable_hash(String value) {
        return "stable-" + value.replace(':', '-');
    }
}
