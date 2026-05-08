package dev.capylang.test;

import capy.metaProg.Reflection;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class ObjectOrientedReflectionTest {
    @Test
    void reflectsObjectOrientedInterface() {
        var x = X.type();

        assertThat(x.name()).isEqualTo("X");
        assertThat(x.methods()).extracting(Reflection.MethodInfo::name).containsExactly("print");
        assertThat(x.parents()).isEmpty();
    }

    @Test
    void reflectsObjectOrientedTrait() {
        var y = Y.type();

        assertThat(y.name()).isEqualTo("Y");
        assertThat(y.methods()).extracting(Reflection.MethodInfo::name).containsExactly("bracket");
        assertThat(y.parents()).isEmpty();
    }

    @Test
    void reflectsObjectOrientedClassWithParents() {
        var z = Z.type();

        assertThat(z.name()).isEqualTo("Z");
        assertThat(z.open()).isFalse();
        assertThat(z.fields()).extracting(Reflection.FieldInfo::name).containsExactly("name");
        assertThat(z.methods()).extracting(Reflection.MethodInfo::name).containsExactlyInAnyOrder("greet", "print");
        assertThat(z.parents()).extracting(parent -> ((Reflection.AnyInfo) parent).name())
                .containsExactlyInAnyOrder("X", "Y");
    }
}
