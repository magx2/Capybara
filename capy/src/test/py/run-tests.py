#!/usr/bin/env python3
import pathlib
import sys
import unittest
import importlib.util


def main() -> int:
    if len(sys.argv) != 2:
        print("Usage: run-tests.py <test-dir>", file=sys.stderr)
        return 2
    test_dir = pathlib.Path(sys.argv[1]).resolve()
    suite = unittest.TestSuite()
    for index, path in enumerate(sorted(test_dir.rglob("*_test.py"))):
        module_name = "capy_py_test_%s_%s" % (index, path.stem)
        spec = importlib.util.spec_from_file_location(module_name, path)
        module = importlib.util.module_from_spec(spec)
        sys.modules[module_name] = module
        spec.loader.exec_module(module)
        suite.addTests(unittest.defaultTestLoader.loadTestsFromModule(module))
    result = unittest.TextTestRunner(verbosity=2).run(suite)
    return 0 if result.wasSuccessful() else 1


if __name__ == "__main__":
    raise SystemExit(main())
