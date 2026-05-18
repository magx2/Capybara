import os
import pathlib
import sys
import unittest


generated_dir = pathlib.Path(os.environ["CAPY_E2E_CFUN_PY_GENERATED_DIR"])
sys.path.insert(0, str(generated_dir))

import dev.capylang.test.PrimitiveBackedTypes as primitive_backed_types


class PrimitiveBackedTypesPythonE2ETest(unittest.TestCase):
    def test_primitive_backed_types_execute_as_ints(self):
        self.assertEqual(primitive_backed_types.rawUserId(7), 7)
        self.assertEqual(primitive_backed_types.unwrapUserId(7), 7)
        self.assertEqual(primitive_backed_types.unwrapRawUserId(7), 7)
        self.assertEqual(primitive_backed_types.plusUserIds(2, 3), 5)
        self.assertEqual(primitive_backed_types.addUserIds(4, 5), 9)
        self.assertEqual(primitive_backed_types.unwrapScore(primitive_backed_types.scoreOf(11)), 11)
