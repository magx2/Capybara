def NativeImplementation(_target=None, **_kwargs):
    def decorate(cls):
        return cls
    if _target is None:
        return decorate
    return decorate(_target)


@NativeImplementation(qualifier="stable", interface_id="performance.sample.nativeinterop.NativeHasher")
class StableHasher:
    def stable_hash(self, value):
        return "stable-" + str(value).replace(":", "-")
