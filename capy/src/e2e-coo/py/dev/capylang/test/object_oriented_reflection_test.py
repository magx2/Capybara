import os
import pathlib
import sys
import unittest


generated_dir = pathlib.Path(os.environ["CAPY_E2E_COO_PY_GENERATED_DIR"])
sys.path.insert(0, str(generated_dir))

from dev.capylang.test.X import X
from dev.capylang.test.Y import Y
from dev.capylang.test.Z import Z


def annotation_argument(annotation, name):
    return next(argument for argument in annotation.arguments if argument.name == name)


class ObjectOrientedReflectionPythonE2ETest(unittest.TestCase):
    def test_reflects_object_oriented_interface_annotations(self):
        info = X.type()

        self.assertEqual(info.name, "X")
        self.assertEqual([annotation.name for annotation in info.annotations], ["CooTypeMarker"])
        self.assertEqual(annotation_argument(info.annotations[0], "label").value.value, "contract")
        self.assertEqual([method.name for method in info.methods], ["print"])
        self.assertEqual([annotation.name for annotation in info.methods[0].annotations], ["CooMethodMarker"])
        self.assertEqual(info.parents, [])

    def test_reflects_object_oriented_trait_annotations(self):
        info = Y.type()

        self.assertEqual(info.name, "Y")
        self.assertEqual([annotation.name for annotation in info.annotations], ["CooTypeMarker"])
        self.assertEqual(annotation_argument(info.annotations[0], "label").value.value, "mixin")
        self.assertEqual([method.name for method in info.methods], ["bracket"])
        self.assertEqual([annotation.name for annotation in info.methods[0].annotations], ["CooMethodMarker"])
        self.assertEqual(info.parents, [])

    def test_reflects_object_oriented_class_field_and_method_annotations(self):
        info = Z.type()

        self.assertEqual(info.name, "Z")
        self.assertFalse(info.open)
        self.assertEqual([annotation.name for annotation in info.annotations], ["CooTypeMarker"])
        self.assertEqual(annotation_argument(info.annotations[0], "label").value.value, "entity")
        self.assertEqual([field.name for field in info.fields], ["name"])
        self.assertEqual([annotation.name for annotation in info.fields[0].annotations], ["CooFieldMarker"])
        self.assertEqual(annotation_argument(info.fields[0].annotations[0], "value").value.value, "display_name")
        self.assertEqual(sorted(method.name for method in info.methods), ["greet", "print"])
        self.assertEqual(
            sorted(method.annotations[0].name for method in info.methods),
            ["CooMethodMarker", "CooMethodMarker"],
        )
        self.assertEqual(sorted(parent.name for parent in info.parents), ["X", "Y"])
