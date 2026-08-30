from dev.capylang.capybara import NativeImplementation
from dev.capylang.test.NativeClock import NativeClock


@NativeImplementation(qualifier="system")
class SystemClock(NativeClock):
    def now_millis(self):
        return 12345
