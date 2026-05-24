package dev.capylang.test;

import capy.metaProg.Reflection;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class ObjectOrientedReflectionTest {
    @Test
    void reflectsObjectOrientedInterface() {
        var x = X.type();

        assertThat(x.name()).isEqualTo("X");
        assertThat(x.annotations()).extracting(Reflection.AnnotationInfo::name).containsExactly("CooTypeMarker");
        assertThat(annotationString(x.annotations().getFirst(), "label")).isEqualTo("contract");
        assertThat(x.methods()).extracting(Reflection.MethodInfo::name).containsExactly("print");
        assertThat(x.methods().getFirst().annotations()).extracting(Reflection.AnnotationInfo::name)
                .containsExactly("CooMethodMarker");
        assertThat(x.parents()).isEmpty();
    }

    @Test
    void reflectsObjectOrientedTrait() {
        var y = Y.type();

        assertThat(y.name()).isEqualTo("Y");
        assertThat(y.annotations()).extracting(Reflection.AnnotationInfo::name).containsExactly("CooTypeMarker");
        assertThat(annotationString(y.annotations().getFirst(), "label")).isEqualTo("mixin");
        assertThat(y.methods()).extracting(Reflection.MethodInfo::name).containsExactly("bracket");
        assertThat(y.parents()).isEmpty();
    }

    @Test
    void reflectsObjectOrientedClassWithParents() {
        var z = Z.type();

        assertThat(z.name()).isEqualTo("Z");
        assertThat(z.open()).isFalse();
        assertThat(z.annotations()).extracting(Reflection.AnnotationInfo::name).containsExactly("CooTypeMarker");
        assertThat(annotationString(z.annotations().getFirst(), "label")).isEqualTo("entity");
        assertThat(z.fields()).extracting(Reflection.FieldInfo::name).containsExactly("name");
        assertThat(z.fields().getFirst().annotations()).extracting(Reflection.AnnotationInfo::name)
                .containsExactly("CooFieldMarker");
        assertThat(annotationString(z.fields().getFirst().annotations().getFirst(), "value"))
                .isEqualTo("display_name");
        assertThat(z.methods()).extracting(Reflection.MethodInfo::name).containsExactlyInAnyOrder("greet", "print");
        assertThat(z.methods()).allSatisfy(method ->
                assertThat(method.annotations()).extracting(Reflection.AnnotationInfo::name)
                        .containsExactly("CooMethodMarker"));
        assertThat(z.parents()).extracting(parent -> ((Reflection.AnyInfo) parent).name())
                .containsExactlyInAnyOrder("X", "Y");
    }

    private static String annotationString(Reflection.AnnotationInfo annotation, String name) {
        return annotation.arguments().stream()
                .filter(argument -> argument.name().equals(name))
                .map(argument -> ((Reflection.AnnotationString) argument.value()).value())
                .findFirst()
                .orElseThrow();
    }
}
