import os
import pathlib
import sys
import unittest


generated_dir = pathlib.Path(os.environ["CAPY_E2E_COO_PY_GENERATED_DIR"])
sys.path.insert(0, str(generated_dir))

from dev.capylang.test.Person import Person


class ObjectOrientedRuntimePythonE2ETest(unittest.TestCase):
    def test_class_methods_and_runtime_behaviour_execute_from_generated_python(self):
        person = Person("Capy")

        self.assertEqual(person.greet(), "hello Capy")
        self.assertEqual(person.print(), "hello Capy!")
        self.assertEqual(person.display(True), "[Capy]")
        self.assertEqual(person.display(False), "Capy")
        self.assertEqual(person.local_increment(7), 8)
        self.assertTrue(person.parity(12))
        self.assertFalse(person.parity(15))
        self.assertEqual(person.second_name(["zero", "one", "two"]), "one")
        self.assertEqual(person.names(), ["zero", "one"])
        self.assertEqual(len(person.slots(4)), 4)
        self.assertEqual(person.recover(True), "boom")
        self.assertEqual(person.catch_index(["zero"]), "ArrayIndexOutOfBoundsException")
