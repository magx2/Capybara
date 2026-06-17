def NativeImplementation(*args, **kwargs):
    if args and len(args) == 1 and callable(args[0]) and not kwargs:
        return args[0]

    def decorate(cls):
        return cls

    return decorate
