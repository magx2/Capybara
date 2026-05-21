import os
import pathlib
import subprocess
import sys
import unittest


generated_dir = pathlib.Path(os.environ["CAPY_E2E_COO_PY_GENERATED_DIR"])
sys.path.insert(0, str(generated_dir))

from dev.capylang.test.Person import Person
from dev.capylang.test.Main import Main


class ObjectOrientedRuntimePythonE2ETest(unittest.TestCase):
    def test_main_method_is_ordinary_instance_method_only(self):
        main = Main()

        self.assertEqual(main.main(["one", "two"]), 2)

        run = subprocess.run(
            [sys.executable, str(generated_dir / "dev" / "capylang" / "test" / "Main.py"), "one", "two"],
            text=True,
            capture_output=True,
            check=False,
        )
        self.assertEqual(run.returncode, 0, run.stderr)
        self.assertEqual(run.stdout.strip(), "")
        self.assertEqual(run.stderr.strip(), "")

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
