import os
import pathlib
import sys
import unittest


generated_dir = pathlib.Path(os.environ["CAPY_E2E_CFUN_PY_GENERATED_DIR"])
sys.path.insert(0, str(generated_dir))

import dev.capylang.test.SimpleFunction as simple


class SimpleFunctionPythonE2ETest(unittest.TestCase):
    def test_simple_functions_execute_from_generated_python(self):
        self.assertTrue(simple.alwaysTrue())
        self.assertEqual(simple.add(1, 2), 3)
        self.assertEqual(simple.subtract(1, 2), -1)
        self.assertEqual(simple.multiply(5, 3), 15)
        self.assertEqual(simple.divide(10, 2), 5)
        self.assertEqual(simple.classify(1), "positive")
        self.assertEqual(simple.classify(0), "non-positive")
        self.assertEqual(simple.greet("World"), "Hello, World")
        self.assertEqual(simple.power(2, 3), 8)
        self.assertEqual(simple.staticList(), [1, 2, 3])
