package dev.capylang.test;

import capy.metaProg.Reflection;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class ObjectOrientedAnnotationsTest {
    @Test
    void reflectsInterfaceAndTraitAnnotations() {
        var contract = AnnotatedContract.type();
        var naming = AnnotatedNaming.type();

        assertThat(contract.annotations()).extracting(Reflection.AnnotationInfo::name)
                .containsExactly("OoAnnotationMarker", "OoTypeLabel", "Deprecated");
        assertThat(contract.annotations().getFirst().arguments()).isEmpty();
        assertThat(contract.annotations().getFirst().pkg().name())
                .isEqualTo("ObjectOrientedAnnotationDefinitions");
        assertThat(annotationString(contract.annotations().get(1), "label")).isEqualTo("contract");
        assertThat(annotationString(contract.annotations().get(2), "message")).isEqualTo("use AnnotatedContractV2");
        assertThat(annotationString(contract.annotations().get(2), "since")).isEmpty();
        assertThat(contract.methods()).extracting(Reflection.MethodInfo::name).containsExactly("render");
        assertThat(contract.methods().getFirst().annotations()).extracting(Reflection.AnnotationInfo::name)
                .containsExactly("OoMethodName");

        assertThat(naming.annotations()).extracting(Reflection.AnnotationInfo::name)
                .containsExactly("OoAnnotationMarker", "OoTypeLabel", "Deprecated");
        assertThat(annotationString(naming.annotations().get(1), "label")).isEqualTo("mixin");
        assertThat(annotationString(naming.annotations().get(2), "message")).isEqualTo("use AnnotatedNamingV2");
        assertThat(annotationString(naming.annotations().get(2), "since")).isEqualTo("3.0");
        assertThat(naming.methods()).extracting(Reflection.MethodInfo::name).containsExactly("prefix");
    }

    @Test
    void reflectsClassFieldAndMethodAnnotations() {
        var widget = AnnotatedWidget.type();

        assertThat(widget.annotations()).extracting(Reflection.AnnotationInfo::name)
                .containsExactly("OoAnnotationMarker", "OoTypeLabel", "Deprecated");
        assertThat(annotationString(widget.annotations().get(1), "label")).isEqualTo("entity");
        assertThat(widget.annotations().get(2).pkg().name()).isEqualTo("Annotations");
        assertThat(widget.annotations().get(2).pkg().path()).isEqualTo("capy/meta_prog");
        assertThat(annotationString(widget.annotations().get(2), "message")).isEqualTo("use AnnotatedView");
        assertThat(annotationString(widget.annotations().get(2), "since")).isEqualTo("3.0");
        assertThat(widget.parents()).extracting(parent -> ((Reflection.AnyInfo) parent).name())
                .containsExactlyInAnyOrder("AnnotatedContract", "AnnotatedNaming");
        assertThat(widget.fields()).extracting(Reflection.FieldInfo::name).containsExactly("name");
        assertThat(widget.fields().getFirst().annotations()).extracting(Reflection.AnnotationInfo::name)
                .containsExactly("OoAnnotationMarker", "OoFieldName", "Deprecated");
        assertThat(widget.fields().getFirst().annotations().getFirst().arguments()).isEmpty();
        assertThat(annotationString(widget.fields().getFirst().annotations().get(1), "value"))
                .isEqualTo("display_name");
        assertThat(annotationString(widget.fields().getFirst().annotations().get(2), "message"))
                .isEqualTo("use label");
        assertThat(annotationString(widget.fields().getFirst().annotations().get(2), "since"))
                .isEmpty();
        assertThat(widget.methods()).extracting(Reflection.MethodInfo::name).containsExactly("render");
        assertThat(widget.methods().getFirst().annotations()).extracting(Reflection.AnnotationInfo::name)
                .containsExactly("OoMethodName", "Deprecated");
        assertThat(annotationString(widget.methods().getFirst().annotations().getFirst(), "value"))
                .isEqualTo("render");
        assertThat(annotationString(widget.methods().getFirst().annotations().get(1), "message"))
                .isEqualTo("use render_label");
        assertThat(annotationString(widget.methods().getFirst().annotations().get(1), "since"))
                .isEqualTo("3.1");
    }

    private static String annotationString(Reflection.AnnotationInfo annotation, String name) {
        return annotation.arguments().stream()
                .filter(argument -> argument.name().equals(name))
                .map(argument -> ((Reflection.AnnotationString) argument.value()).value())
                .findFirst()
                .orElseThrow();
    }
}
