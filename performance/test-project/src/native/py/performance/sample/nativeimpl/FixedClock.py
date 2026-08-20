def NativeImplementation(_target=None, **_kwargs):
    def decorate(cls):
        return cls
    if _target is None:
        return decorate
    return decorate(_target)


@NativeImplementation(qualifier="fixed", interface_id="performance.sample.nativeinterop.NativeClock")
class FixedClock:
    def fixed_millis(self):
        return 4242
