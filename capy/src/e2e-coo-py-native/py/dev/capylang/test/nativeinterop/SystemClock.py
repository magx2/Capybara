from dev.capylang.capybara import NativeImplementation
from dev.capylang.test.Clock import Clock


@NativeImplementation(qualifier="system")
class SystemClock(Clock):
    def now_millis(self):
        return 12345
