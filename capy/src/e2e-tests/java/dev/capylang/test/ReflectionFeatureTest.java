package dev.capylang.test;

import capy.metaProg.Reflection;
import org.junit.jupiter.api.Test;

import java.util.List;

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
        assertThat(a.values()).extracting(Reflection.DataFieldValueInfo::name).containsExactly("name", "a");
        assertThat(a.values()).extracting(Reflection.DataFieldValueInfo::value).containsExactly("letter-a", 1);
        assertThat(a.values().get(1).type().name()).isEqualTo("int");
    }

    @Test
    void reflectsFunctionalDataValuesWithCollectionFields() {
        var b = ReflectionFeature.reflectionValueB(new ReflectionFeature.B("letter-b", List.of("red", "blue")));

        assertThat(b.name()).isEqualTo("B");
        assertThat(b.fields()).extracting(Reflection.DataFieldInfo::name).containsExactly("name", "b");
        assertThat(b.values()).extracting(Reflection.DataFieldValueInfo::name).containsExactly("name", "b");
        assertThat(b.values()).extracting(Reflection.DataFieldValueInfo::value)
                .containsExactly("letter-b", List.of("red", "blue"));
        assertThat(b.values().get(1).type()).isInstanceOf(Reflection.ListInfo.class);
        assertThat(((Reflection.ListInfo) b.values().get(1).type()).element_type().name()).isEqualTo("string");
    }

    @Test
    void reflectsEmptyFunctionalDataValues() {
        var empty = ReflectionFeature.reflectionValueEmpty();

        assertThat(empty.name()).isEqualTo("ReflectedEmpty");
        assertThat(empty.pkg().name()).isEqualTo("ReflectionFeature");
        assertThat(empty.pkg().path()).isEqualTo("dev/capylang/test/ReflectionFeature");
        assertThat(empty.fields()).isEmpty();
        assertThat(empty.values()).isEmpty();
    }

    @Test
    void reflectsFunctionalDataValuesWithSpreadFields() {
        var employee = ReflectionFeature.reflectionValueEmployee(
                new ReflectionFeature.ReflectedEmployee("E-1", "R&D", true));

        assertThat(employee.name()).isEqualTo("ReflectedEmployee");
        assertThat(employee.fields()).extracting(Reflection.DataFieldInfo::name)
                .containsExactly("id", "department", "active");
        assertThat(employee.values()).extracting(Reflection.DataFieldValueInfo::name)
                .containsExactly("id", "department", "active");
        assertThat(employee.values()).extracting(Reflection.DataFieldValueInfo::value)
                .containsExactly("E-1", "R&D", true);
        assertThat(employee.values().get(2).type().name()).isEqualTo("bool");
    }

    @Test
    void reflectionValuePreservesDataInfoMetadataSurface() {
        var a = ReflectionFeature.reflectionValueA(new ReflectionFeature.A("letter-a", 1));

        assertThat(a.pkg().name()).isEqualTo("ReflectionFeature");
        assertThat(a.pkg().path()).isEqualTo("dev/capylang/test/ReflectionFeature");
        assertThat(a.functions()).isEmpty();
        assertThat(a.fields()).extracting(Reflection.DataFieldInfo::name).containsExactly("name", "a");
    }

    @Test
    void reflectionValueEntriesPreserveDataFieldInfoMetadata() {
        var a = ReflectionFeature.reflectionValueA(new ReflectionFeature.A("letter-a", 1));

        assertThat(a.values()).hasSize(2);
        assertThat(a.values().get(0).name()).isEqualTo(a.fields().get(0).name());
        assertThat(a.values().get(0).type().name()).isEqualTo(a.fields().get(0).type().name());
        assertThat(a.values().get(1).name()).isEqualTo(a.fields().get(1).name());
        assertThat(a.values().get(1).type().name()).isEqualTo(a.fields().get(1).type().name());
    }

    @Test
    void reflectionValueIncludesInheritedParentFieldValuesInMetadataOrder() {
        var a = ReflectionFeature.reflectionValueA(new ReflectionFeature.A("letter-a", 1));

        assertThat(a.fields()).extracting(Reflection.DataFieldInfo::name).containsExactly("name", "a");
        assertThat(a.values()).extracting(Reflection.DataFieldValueInfo::name).containsExactly("name", "a");
        assertThat(a.values()).extracting(Reflection.DataFieldValueInfo::value).containsExactly("letter-a", 1);
    }

    @Test
    void reflectsFunctionalDataValuesThroughGenericDataParameter() {
        var a = ReflectionFeature.reflectionValueData(new ReflectionFeature.A("letter-a", 1));

        assertThat(a.name()).isEqualTo("A");
        assertThat(a.fields()).extracting(Reflection.DataFieldInfo::name).containsExactly("name", "a");
        assertThat(a.values()).extracting(Reflection.DataFieldValueInfo::value).containsExactly("letter-a", 1);
    }

    @Test
    void reflectsFunctionalDataValuesWithCollectionFieldsThroughGenericDataParameter() {
        var b = ReflectionFeature.reflectionValueData(new ReflectionFeature.B("letter-b", List.of("red", "blue")));

        assertThat(b.name()).isEqualTo("B");
        assertThat(b.values()).extracting(Reflection.DataFieldValueInfo::name).containsExactly("name", "b");
        assertThat(b.values()).extracting(Reflection.DataFieldValueInfo::value)
                .containsExactly("letter-b", List.of("red", "blue"));
        assertThat(b.values().get(1).type()).isInstanceOf(Reflection.ListInfo.class);
    }

    @Test
    void reflectsEmptyFunctionalDataValuesThroughGenericDataParameter() {
        var empty = ReflectionFeature.reflectionValueData(new ReflectionFeature.ReflectedEmpty());

        assertThat(empty.name()).isEqualTo("ReflectedEmpty");
        assertThat(empty.fields()).isEmpty();
        assertThat(empty.values()).isEmpty();
    }

    @Test
    void reflectsFunctionalDataValuesWithSpreadFieldsThroughGenericDataParameter() {
        var employee = ReflectionFeature.reflectionValueData(
                new ReflectionFeature.ReflectedEmployee("E-1", "R&D", true));

        assertThat(employee.name()).isEqualTo("ReflectedEmployee");
        assertThat(employee.values()).extracting(Reflection.DataFieldValueInfo::name)
                .containsExactly("id", "department", "active");
        assertThat(employee.values()).extracting(Reflection.DataFieldValueInfo::value)
                .containsExactly("E-1", "R&D", true);
    }

    @Test
    void reflectsSingletonAndEnumDataValuesThroughGenericDataParameter() {
        var singleton = ReflectionFeature.reflectionValueData(ReflectionFeature.ReflectedSingleton.INSTANCE);
        var enumValue = ReflectionFeature.reflectionValueData(ReflectionFeature.ReflectedStatus.REFLECTED_READY);

        assertThat(singleton.name()).isEqualTo("ReflectedSingleton");
        assertThat(singleton.values()).isEmpty();
        assertThat(enumValue.name()).isEqualTo("REFLECTED_READY");
        assertThat(enumValue.values()).isEmpty();
    }

    @Test
    void jsonShapedReflectionUsesGenericDataForNestedRecords() {
        var json = ReflectionFeature.reflectionValueJsonShape(
                new ReflectionFeature.ReflectedEnvelope("outer", new ReflectionFeature.A("inner", 7)));

        assertThat(json.value()).containsEntry("label", new ReflectionFeature.ReflectedJsonString("outer"));
        assertThat(json.value().get("payload")).isInstanceOfSatisfying(
                ReflectionFeature.ReflectedJsonObject.class,
                payload -> assertThat(payload.value())
                        .containsEntry("name", new ReflectionFeature.ReflectedJsonString("inner"))
                        .containsEntry("a", new ReflectionFeature.ReflectedJsonNumber(7))
        );
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
