import os
import pathlib
import sys
import unittest


generated_dir = pathlib.Path(os.environ["CAPY_E2E_COO_PY_GENERATED_DIR"])
sys.path.insert(0, str(generated_dir))

from dev.capylang.test.AnnotatedContract import AnnotatedContract
from dev.capylang.test.AnnotatedNaming import AnnotatedNaming
from dev.capylang.test.AnnotatedWidget import AnnotatedWidget


def annotation_argument(annotation, name):
    return next(argument for argument in annotation.arguments if argument.name == name)


class ObjectOrientedAnnotationsPythonE2ETest(unittest.TestCase):
    def test_reflects_object_oriented_interface_and_trait_annotations(self):
        contract = AnnotatedContract.type()
        naming = AnnotatedNaming.type()

        self.assertEqual([annotation.name for annotation in contract.annotations], ["OoAnnotationMarker", "OoTypeLabel", "Deprecated"])
        self.assertEqual(contract.annotations[0].arguments, [])
        self.assertEqual(contract.annotations[0].pkg.name, "ObjectOrientedAnnotationDefinitions")
        self.assertEqual(annotation_argument(contract.annotations[1], "label").value.value, "contract")
        self.assertEqual(annotation_argument(contract.annotations[2], "message").value.value, "use AnnotatedContractV2")
        self.assertEqual(annotation_argument(contract.annotations[2], "since").value.value, "")
        self.assertEqual([method.name for method in contract.methods], ["render"])
        self.assertEqual([annotation.name for annotation in contract.methods[0].annotations], ["OoMethodName"])

        self.assertEqual([annotation.name for annotation in naming.annotations], ["OoAnnotationMarker", "OoTypeLabel", "Deprecated"])
        self.assertEqual(annotation_argument(naming.annotations[1], "label").value.value, "mixin")
        self.assertEqual(annotation_argument(naming.annotations[2], "message").value.value, "use AnnotatedNamingV2")
        self.assertEqual(annotation_argument(naming.annotations[2], "since").value.value, "3.0")
        self.assertEqual([method.name for method in naming.methods], ["prefix"])

    def test_reflects_object_oriented_class_field_and_method_annotations(self):
        widget = AnnotatedWidget.type()

        self.assertEqual([annotation.name for annotation in widget.annotations], ["OoAnnotationMarker", "OoTypeLabel", "Deprecated"])
        self.assertEqual(annotation_argument(widget.annotations[1], "label").value.value, "entity")
        self.assertEqual(widget.annotations[2].pkg.name, "Annotations")
        self.assertEqual(widget.annotations[2].pkg.path, "capy/meta_prog")
        self.assertEqual(annotation_argument(widget.annotations[2], "message").value.value, "use AnnotatedView")
        self.assertEqual(annotation_argument(widget.annotations[2], "since").value.value, "3.0")
        self.assertEqual(sorted(parent.name for parent in widget.parents), ["AnnotatedContract", "AnnotatedNaming"])
        self.assertEqual([field.name for field in widget.fields], ["name"])
        self.assertEqual(
            [annotation.name for annotation in widget.fields[0].annotations],
            ["OoAnnotationMarker", "OoFieldName", "Deprecated"],
        )
        self.assertEqual(widget.fields[0].annotations[0].arguments, [])
        self.assertEqual(annotation_argument(widget.fields[0].annotations[1], "value").value.value, "display_name")
        self.assertEqual(annotation_argument(widget.fields[0].annotations[2], "message").value.value, "use label")
        self.assertEqual(annotation_argument(widget.fields[0].annotations[2], "since").value.value, "")
        self.assertEqual([method.name for method in widget.methods], ["render"])
        self.assertEqual([annotation.name for annotation in widget.methods[0].annotations], ["OoMethodName", "Deprecated"])
        self.assertEqual(annotation_argument(widget.methods[0].annotations[0], "value").value.value, "render")
        self.assertEqual(annotation_argument(widget.methods[0].annotations[1], "message").value.value, "use render_label")
        self.assertEqual(annotation_argument(widget.methods[0].annotations[1], "since").value.value, "3.1")
