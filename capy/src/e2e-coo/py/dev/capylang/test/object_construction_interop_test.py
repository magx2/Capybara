import contextlib
import io
import os
import pathlib
import sys
import unittest


generated_dir = pathlib.Path(os.environ["CAPY_E2E_COO_PY_GENERATED_DIR"])
sys.path.insert(0, str(generated_dir))

import dev.capylang.test.ObjectConstructionInterop as interop
from dev.capylang.test.EmptyTracked import EmptyTracked
from dev.capylang.test.TrackedPerson import TrackedPerson


class ObjectConstructionInteropPythonE2ETest(unittest.TestCase):
    def capture(self, callback):
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            result = callback()
        return result, out.getvalue()

    def test_functional_object_construction_is_lazy_and_fresh(self):
        effect, output = self.capture(lambda: interop.makePerson("Ada"))
        self.assertEqual(output, "")

        first, output = self.capture(effect.unsafe_run)
        self.assertIsInstance(first, TrackedPerson)
        self.assertEqual(first.label(), "Ada")
        self.assertEqual(output, "constructed:Ada\n")

        second, output = self.capture(effect.unsafe_run)
        self.assertIsInstance(second, TrackedPerson)
        self.assertIsNot(second, first)
        self.assertEqual(output, "constructed:Ada\n")

    def test_parent_zero_arg_and_sequence_cases(self):
        printable, _ = self.capture(lambda: interop.makePrintable("Bara").unsafe_run())
        self.assertIsInstance(printable, TrackedPerson)
        self.assertEqual(printable.label(), "Bara")

        empty_effect, output = self.capture(interop.makeEmpty)
        self.assertEqual(output, "")
        empty, output = self.capture(empty_effect.unsafe_run)
        self.assertIsInstance(empty, EmptyTracked)
        self.assertEqual(empty.label(), "empty")
        self.assertEqual(output, "empty\n")

        result, output = self.capture(lambda: interop.sequenceTwo("A", "B").unsafe_run())
        self.assertEqual(result, "A:B")
        self.assertEqual(output, "constructed:A\nconstructed:B\n")
