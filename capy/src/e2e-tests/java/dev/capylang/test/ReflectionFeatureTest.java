package dev.capylang.test;

import capy.metaProg.Reflection;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class ReflectionFeatureTest {
    @Test
    void reflectsFunctionalParentType() {
        var letter = (Reflection.TypeInfo) ReflectionFeature.reflectionLetter();

        assertThat(letter.name()).isEqualTo("Letter");
        assertThat(letter.fields()).extracting(Reflection.DataFieldInfo::name).containsExactly("name");
        assertThat(letter.data()).extracting(Reflection.DataInfo::name).containsExactlyInAnyOrder("A", "B");

        var letterFromValue = (Reflection.TypeInfo) ReflectionFeature.reflectionLetter2(new ReflectionFeature.A("letter-a", 1));
        assertThat(letterFromValue.name()).isEqualTo("Letter");
        assertThat(letterFromValue.fields()).extracting(Reflection.DataFieldInfo::name).containsExactly("name");
        assertThat(letterFromValue.data()).extracting(Reflection.DataInfo::name).containsExactlyInAnyOrder("A", "B");
    }

    @Test
    void reflectsFunctionalDataTypes() {
        var a = (Reflection.DataInfo) ReflectionFeature.reflectionA();
        assertThat(a.name()).isEqualTo("A");
        assertThat(a.fields()).extracting(Reflection.DataFieldInfo::name).containsExactly("name", "a");
        assertThat(a.fields().get(1).type()).isInstanceOf(Reflection.PrimitiveInfo.class);
        assertThat(a.fields().get(1).type().name()).isEqualTo("int");

        var aFromValue = (Reflection.DataInfo) ReflectionFeature.reflectionA2(new ReflectionFeature.A("letter-a", 1));
        assertThat(aFromValue.name()).isEqualTo("A");
        assertThat(aFromValue.fields()).extracting(Reflection.DataFieldInfo::name).containsExactly("name", "a");

        var b = (Reflection.DataInfo) ReflectionFeature.reflectionB();
        var bField = b.fields().stream()
                .filter(field -> field.name().equals("b"))
                .findFirst()
                .orElseThrow();
        assertThat(bField.type()).isInstanceOf(Reflection.ListInfo.class);
        assertThat(((Reflection.ListInfo) bField.type()).element_type().name()).isEqualTo("string");
    }

    @Test
    void reflectsFunctionalDataValues() {
        var a = ReflectionFeature.reflectionValueA(new ReflectionFeature.A("letter-a", 1));

        assertThat(a.name()).isEqualTo("A");
        assertThat(a.fields()).extracting(Reflection.DataFieldInfo::name).containsExactly("name", "a");
        assertThat(a.fields().get(1).type()).isInstanceOf(Reflection.PrimitiveInfo.class);
        assertThat(a.fields().get(1).type().name()).isEqualTo("int");
    }

    @Test
    void reflectsFunctionReferences() {
        var method = ReflectionFeature.reflectionMethod();

        assertThat(method.name()).isEqualTo("test_method");
        assertThat(method.params()).extracting(Reflection.ParamInfo::name)
                .containsExactly("l", "x", "d", "a", "letter");
        assertThat(method.return_type().name()).isEqualTo("string");
        assertThat(method.params().getFirst().type()).isInstanceOf(Reflection.ListInfo.class);
    }
}
