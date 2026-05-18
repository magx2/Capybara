import os
import pathlib
import sys
import unittest


generated_dir = pathlib.Path(os.environ["CAPY_E2E_COO_PY_GENERATED_DIR"])
sys.path.insert(0, str(generated_dir))

from dev.capylang.test.PetInteractor import PetInteractor


class ObjectOrientedFpInteropPythonE2ETest(unittest.TestCase):
    def test_object_oriented_code_uses_primitive_backed_functional_types(self):
        interactor = PetInteractor()

        self.assertEqual(interactor.echo_user_id(7), 7)
        self.assertEqual(interactor.construct_user_id(11), 11)
        self.assertEqual(interactor.make_user_id(13), 13)
        self.assertEqual(interactor.unwrap_user_id(17), 17)
        self.assertEqual(interactor.add_user_ids(19, 23), 42)
        self.assertEqual(interactor.local_user_id(29), 29)
