package dev.capylang.test;

import capy.metaProg.Reflection;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class AnnotationReflectionTest {
    @Test
    void reflectsFunctionalDeclarationAnnotations() {
        var info = AnnotationReflection.annotationReflectionSample();

        assertThat(info.name()).isEqualTo("AnnotatedCustomer");
        assertThat(info.annotations()).extracting(Reflection.AnnotationInfo::name)
                .containsExactly("ImportedAnnotationMarker", "LocalDataLabel", "ImportedDataLabel");
        assertThat(info.annotations().get(0).arguments()).isEmpty();
        assertThat(info.annotations().get(0).pkg().name())
                .isEqualTo("AnnotationReflectionDefinitions");
        assertThat(annotationString(info.annotations().get(1), "label")).isEqualTo("local");
        assertThat(annotationString(info.annotations().get(2), "label")).isEqualTo("imported");
        assertThat(annotationInt(info.annotations().get(2), "order")).isEqualTo(5);
        assertThat(annotationBool(info.annotations().get(2), "active")).isTrue();
    }

    @Test
    void reflectsFunctionalFieldAnnotations() {
        var info = AnnotationReflection.annotationReflectionData(
                new AnnotationReflection.AnnotatedCustomer("C-2", "Bea"));

        assertThat(info.fields()).extracting(Reflection.FieldValueInfo::name)
                .containsExactly("id", "display");
        assertThat(info.fields().get(0).annotations()).extracting(Reflection.AnnotationInfo::name)
                .containsExactly("ImportedAnnotationMarker", "ImportedFieldName");
        assertThat(info.fields().get(0).annotations().get(0).arguments()).isEmpty();
        assertThat(annotationString(info.fields().get(0).annotations().get(1), "value"))
                .isEqualTo("customer_id");
        assertThat(info.fields().get(1).annotations()).extracting(Reflection.AnnotationInfo::name)
                .containsExactly("LocalFieldMarker");
        assertThat(info.fields().get(1).value()).isEqualTo("Bea");
    }

    @Test
    void reflectsAnnotationsImportedWithQualifiedImportSyntax() {
        var info = AnnotationQualifiedImportReflection.qualifiedImportAnnotationReflectionSample();

        assertThat(info.name()).isEqualTo("QualifiedImportCustomer");
        assertThat(info.annotations()).extracting(Reflection.AnnotationInfo::name)
                .containsExactly("ImportedDataLabel");
        assertThat(info.annotations().getFirst().pkg().name())
                .isEqualTo("AnnotationReflectionDefinitions");
        assertThat(annotationString(info.annotations().getFirst(), "label"))
                .isEqualTo("qualified-import");
        assertThat(annotationInt(info.annotations().getFirst(), "order"))
                .isEqualTo(11);
        assertThat(annotationBool(info.annotations().getFirst(), "active"))
                .isTrue();
        assertThat(info.fields().getFirst().annotations()).extracting(Reflection.AnnotationInfo::name)
                .containsExactly("ImportedFieldName");
        assertThat(annotationString(info.fields().getFirst().annotations().getFirst(), "value"))
                .isEqualTo("qualified_id");
    }

    @Test
    void reflectsStandardDeprecatedAnnotation() {
        var info = DeprecatedAnnotationReflection.deprecatedAnnotationReflectionSample();

        assertThat(info.name()).isEqualTo("DeprecatedCustomer");
        assertThat(info.annotations()).extracting(Reflection.AnnotationInfo::name)
                .containsExactly("Deprecated");
        assertThat(info.annotations().getFirst().pkg().name()).isEqualTo("Annotations");
        assertThat(info.annotations().getFirst().pkg().path()).isEqualTo("capy/meta_prog");
        assertThat(annotationString(info.annotations().getFirst(), "message"))
                .isEqualTo("use CurrentCustomer");
        assertThat(annotationString(info.annotations().getFirst(), "since"))
                .isEqualTo("2.0");

        assertThat(info.fields().getFirst().annotations()).extracting(Reflection.AnnotationInfo::name)
                .containsExactly("Deprecated");
        assertThat(annotationString(info.fields().getFirst().annotations().getFirst(), "message"))
                .isEqualTo("use display_name");
        assertThat(annotationString(info.fields().getFirst().annotations().getFirst(), "since"))
                .isEmpty();
    }

    @Test
    void reflectsStandardDeprecatedAnnotationOnEnumValues() {
        var info = DeprecatedAnnotationReflection.deprecatedEnumAnnotationReflectionSample();

        assertThat(info.name()).isEqualTo("DEPRECATED_READY");
        assertThat(info.fields()).isEmpty();
        assertThat(info.annotations()).extracting(Reflection.AnnotationInfo::name)
                .containsExactly("Deprecated");
        assertThat(info.annotations().getFirst().pkg().name()).isEqualTo("Annotations");
        assertThat(annotationString(info.annotations().getFirst(), "message"))
                .isEqualTo("use CurrentStatus");
        assertThat(annotationString(info.annotations().getFirst(), "since"))
                .isEqualTo("2.2");
    }

    private static String annotationString(Reflection.AnnotationInfo annotation, String name) {
        return annotation.arguments().stream()
                .filter(argument -> argument.name().equals(name))
                .map(argument -> ((Reflection.AnnotationString) argument.value()).value())
                .findFirst()
                .orElseThrow();
    }

    private static int annotationInt(Reflection.AnnotationInfo annotation, String name) {
        return annotation.arguments().stream()
                .filter(argument -> argument.name().equals(name))
                .map(argument -> ((Reflection.AnnotationInt) argument.value()).value())
                .findFirst()
                .orElseThrow();
    }

    private static boolean annotationBool(Reflection.AnnotationInfo annotation, String name) {
        return annotation.arguments().stream()
                .filter(argument -> argument.name().equals(name))
                .map(argument -> ((Reflection.AnnotationBool) argument.value()).value())
                .findFirst()
                .orElseThrow();
    }
}
