import os
import pathlib
import sys
import unittest


generated_dir = pathlib.Path(os.environ["CAPY_E2E_CFUN_PY_GENERATED_DIR"])
sys.path.insert(0, str(generated_dir))

import dev.capylang.test.ReflectionFeature as reflection


def annotation_argument(annotation, name):
    return next(argument for argument in annotation.arguments if argument.name == name)


class ReflectionFeaturePythonE2ETest(unittest.TestCase):
    def test_reflection_returns_data_value_fields(self):
        reflected = reflection.reflectionDataA(reflection.A({"name": "letter-a", "a": 1}))

        self.assertEqual(reflected.name, "A")
        self.assertEqual([field.name for field in reflected.fields], ["name", "a"])
        self.assertEqual([field.value for field in reflected.fields], ["letter-a", 1])

    def test_reflection_returns_functional_data_and_field_annotations(self):
        reflected = reflection.reflectionDataAnnotatedUser(reflection.ReflectedAnnotatedUser({
            "id": "U-1",
            "display": "Ada",
        }))

        self.assertEqual([annotation.name for annotation in reflected.annotations], ["ReflectionDataMarker"])
        self.assertEqual(annotation_argument(reflected.annotations[0], "label").value.kind, "string")
        self.assertEqual(annotation_argument(reflected.annotations[0], "label").value.value, "user")
        self.assertEqual(annotation_argument(reflected.annotations[0], "order").value.value, 7)
        self.assertEqual(annotation_argument(reflected.annotations[0], "active").value.value, True)
        self.assertEqual(reflected.pkg.path, "dev/capylang/test/ReflectionFeature")

        self.assertEqual(
            [field.annotations[0].name for field in reflected.fields],
            ["ReflectionFieldMarker", "ReflectionFieldMarker"],
        )
        self.assertEqual(annotation_argument(reflected.fields[0].annotations[0], "value").value.value, "identifier")
        self.assertEqual(annotation_argument(reflected.fields[1].annotations[0], "value").value.value, "display")
        self.assertEqual([field.type.name for field in reflected.fields], ["String", "String"])
