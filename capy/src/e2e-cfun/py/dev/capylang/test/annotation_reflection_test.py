import os
import pathlib
import sys
import unittest


generated_dir = pathlib.Path(os.environ["CAPY_E2E_CFUN_PY_GENERATED_DIR"])
sys.path.insert(0, str(generated_dir))

import dev.capylang.test.AnnotationReflection as annotations
import dev.capylang.test.AnnotationQualifiedImportReflection as qualified_import_annotations
import dev.capylang.test.DeprecatedAnnotationReflection as deprecated_annotations


def annotation_argument(annotation, name):
    return next(argument for argument in annotation.arguments if argument.name == name)


class AnnotationReflectionPythonE2ETest(unittest.TestCase):
    def test_reflects_functional_declaration_annotations(self):
        info = annotations.annotationReflectionSample()

        self.assertEqual(info.name, "AnnotatedCustomer")
        self.assertEqual(
            [annotation.name for annotation in info.annotations],
            ["ImportedAnnotationMarker", "LocalDataLabel", "ImportedDataLabel"],
        )
        self.assertEqual(info.annotations[0].arguments, [])
        self.assertEqual(info.annotations[0].pkg.name, "AnnotationReflectionDefinitions")
        self.assertEqual(annotation_argument(info.annotations[1], "label").value.value, "local")
        self.assertEqual(annotation_argument(info.annotations[2], "label").value.value, "imported")
        self.assertEqual(annotation_argument(info.annotations[2], "order").value.value, 5)
        self.assertEqual(annotation_argument(info.annotations[2], "active").value.value, True)

    def test_reflects_functional_field_annotations(self):
        info = annotations.annotationReflectionData(annotations.AnnotatedCustomer({
            "id": "C-2",
            "display": "Bea",
        }))

        self.assertEqual([field.name for field in info.fields], ["id", "display"])
        self.assertEqual(
            [annotation.name for annotation in info.fields[0].annotations],
            ["ImportedAnnotationMarker", "ImportedFieldName"],
        )
        self.assertEqual(info.fields[0].annotations[0].arguments, [])
        self.assertEqual(annotation_argument(info.fields[0].annotations[1], "value").value.value, "customer_id")
        self.assertEqual([annotation.name for annotation in info.fields[1].annotations], ["LocalFieldMarker"])
        self.assertEqual(info.fields[1].value, "Bea")

    def test_reflects_annotations_imported_with_qualified_import_syntax(self):
        info = qualified_import_annotations.qualifiedImportAnnotationReflectionSample()

        self.assertEqual(info.name, "QualifiedImportCustomer")
        self.assertEqual([annotation.name for annotation in info.annotations], ["ImportedDataLabel"])
        self.assertEqual(info.annotations[0].pkg.name, "AnnotationReflectionDefinitions")
        self.assertEqual(annotation_argument(info.annotations[0], "label").value.value, "qualified-import")
        self.assertEqual(annotation_argument(info.annotations[0], "order").value.value, 11)
        self.assertEqual(annotation_argument(info.annotations[0], "active").value.value, True)
        self.assertEqual([annotation.name for annotation in info.fields[0].annotations], ["ImportedFieldName"])
        self.assertEqual(annotation_argument(info.fields[0].annotations[0], "value").value.value, "qualified_id")

    def test_reflects_standard_deprecated_annotation(self):
        info = deprecated_annotations.deprecatedAnnotationReflectionSample()

        self.assertEqual(info.name, "DeprecatedCustomer")
        self.assertEqual([annotation.name for annotation in info.annotations], ["Deprecated"])
        self.assertEqual(info.annotations[0].pkg.name, "Annotations")
        self.assertEqual(info.annotations[0].pkg.path, "capy/meta_prog")
        self.assertEqual(annotation_argument(info.annotations[0], "message").value.value, "use CurrentCustomer")
        self.assertEqual(annotation_argument(info.annotations[0], "since").value.value, "2.0")

        self.assertEqual([annotation.name for annotation in info.fields[0].annotations], ["Deprecated"])
        self.assertEqual(annotation_argument(info.fields[0].annotations[0], "message").value.value, "use display_name")
        self.assertEqual(annotation_argument(info.fields[0].annotations[0], "since").value.value, "")

    def test_reflects_standard_deprecated_annotation_on_enum_values(self):
        info = deprecated_annotations.deprecatedEnumAnnotationReflectionSample()

        self.assertEqual(info.name, "DEPRECATED_READY")
        self.assertEqual(info.fields, [])
        self.assertEqual([annotation.name for annotation in info.annotations], ["Deprecated"])
        self.assertEqual(info.annotations[0].pkg.name, "Annotations")
        self.assertEqual(annotation_argument(info.annotations[0], "message").value.value, "use CurrentStatus")
        self.assertEqual(annotation_argument(info.annotations[0], "since").value.value, "2.2")
