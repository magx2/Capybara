package dev.capylang.test;

import capy.metaProg.Reflection;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class ReflectionFeatureTest {
    @Test
    void reflectsFunctionalDataValues() {
        var a = ReflectionFeature.reflectionDataA(new ReflectionFeature.A("letter-a", 1));

        assertThat(a.name()).isEqualTo("A");
        assertThat(a.fields()).extracting(Reflection.FieldValueInfo::name).containsExactly("name", "a");
        assertThat(a.fields().get(1).type()).isInstanceOf(Reflection.DataInfo.class);
        assertThat(a.fields().get(1).type().name()).isEqualTo("int");
        assertThat(a.fields()).extracting(Reflection.FieldValueInfo::value).containsExactly("letter-a", 1);
    }

    @Test
    void reflectsFunctionalDataValuesWithCollectionFields() {
        var b = ReflectionFeature.reflectionDataB(new ReflectionFeature.B("letter-b", List.of("red", "blue")));

        assertThat(b.name()).isEqualTo("B");
        assertThat(b.fields()).extracting(Reflection.FieldValueInfo::name).containsExactly("name", "b");
        assertThat(b.fields()).extracting(Reflection.FieldValueInfo::value)
                .containsExactly("letter-b", List.of("red", "blue"));
        assertThat(b.fields().get(1).type()).isInstanceOf(Reflection.ListInfo.class);
        assertThat(((Reflection.ListInfo) b.fields().get(1).type()).element_type().name()).isEqualTo("String");
    }

    @Test
    void reflectsEmptyFunctionalDataValues() {
        var empty = ReflectionFeature.reflectionDataEmpty();

        assertThat(empty.name()).isEqualTo("ReflectedEmpty");
        assertThat(empty.pkg().name()).isEqualTo("ReflectionFeature");
        assertThat(empty.pkg().path()).isEqualTo("dev/capylang/test/ReflectionFeature");
        assertThat(empty.fields()).isEmpty();
    }

    @Test
    void reflectsFunctionalDataValuesWithSpreadFields() {
        var employee = ReflectionFeature.reflectionDataEmployee(
                new ReflectionFeature.ReflectedEmployee("E-1", "R&D", true));

        assertThat(employee.name()).isEqualTo("ReflectedEmployee");
        assertThat(employee.fields()).extracting(Reflection.FieldValueInfo::name)
                .containsExactly("id", "department", "active");
        assertThat(employee.fields()).extracting(Reflection.FieldValueInfo::value)
                .containsExactly("E-1", "R&D", true);
        assertThat(employee.fields().get(2).type().name()).isEqualTo("bool");
    }

    @Test
    void reflectionDataPreservesDataInfoMetadataSurface() {
        var a = ReflectionFeature.reflectionDataA(new ReflectionFeature.A("letter-a", 1));

        assertThat(a.pkg().name()).isEqualTo("ReflectionFeature");
        assertThat(a.pkg().path()).isEqualTo("dev/capylang/test/ReflectionFeature");
        assertThat(a.fields()).extracting(Reflection.FieldValueInfo::name).containsExactly("name", "a");
    }

    @Test
    void reflectionDataEntriesPreserveFieldMetadata() {
        var a = ReflectionFeature.reflectionDataA(new ReflectionFeature.A("letter-a", 1));

        assertThat(a.fields()).hasSize(2);
        assertThat(a.fields().get(0).name()).isEqualTo("name");
        assertThat(a.fields().get(0).type().name()).isEqualTo("String");
        assertThat(a.fields().get(1).name()).isEqualTo("a");
        assertThat(a.fields().get(1).type().name()).isEqualTo("int");
    }

    @Test
    void reflectionDataIncludesInheritedParentFieldValuesInMetadataOrder() {
        var a = ReflectionFeature.reflectionDataA(new ReflectionFeature.A("letter-a", 1));

        assertThat(a.fields()).extracting(Reflection.FieldValueInfo::name).containsExactly("name", "a");
        assertThat(a.fields()).extracting(Reflection.FieldValueInfo::value).containsExactly("letter-a", 1);
    }

    @Test
    void reflectsFunctionalDataValuesThroughGenericDataParameter() {
        var a = ReflectionFeature.reflectionData(new ReflectionFeature.A("letter-a", 1));

        assertThat(a.name()).isEqualTo("A");
        assertThat(a.fields()).extracting(Reflection.FieldValueInfo::name).containsExactly("name", "a");
        assertThat(a.fields()).extracting(Reflection.FieldValueInfo::value).containsExactly("letter-a", 1);
    }

    @Test
    void reflectsFunctionalDataValuesWithCollectionFieldsThroughGenericDataParameter() {
        var b = ReflectionFeature.reflectionData(new ReflectionFeature.B("letter-b", List.of("red", "blue")));

        assertThat(b.name()).isEqualTo("B");
        assertThat(b.fields()).extracting(Reflection.FieldValueInfo::name).containsExactly("name", "b");
        assertThat(b.fields()).extracting(Reflection.FieldValueInfo::value)
                .containsExactly("letter-b", List.of("red", "blue"));
        assertThat(b.fields().get(1).type()).isInstanceOf(Reflection.ListInfo.class);
    }

    @Test
    void reflectsEmptyFunctionalDataValuesThroughGenericDataParameter() {
        var empty = ReflectionFeature.reflectionData(ReflectionFeature.ReflectedEmpty.INSTANCE);

        assertThat(empty.name()).isEqualTo("ReflectedEmpty");
        assertThat(empty.fields()).isEmpty();
    }

    @Test
    void reflectsFunctionalDataValuesWithSpreadFieldsThroughGenericDataParameter() {
        var employee = ReflectionFeature.reflectionData(
                new ReflectionFeature.ReflectedEmployee("E-1", "R&D", true));

        assertThat(employee.name()).isEqualTo("ReflectedEmployee");
        assertThat(employee.fields()).extracting(Reflection.FieldValueInfo::name)
                .containsExactly("id", "department", "active");
        assertThat(employee.fields()).extracting(Reflection.FieldValueInfo::value)
                .containsExactly("E-1", "R&D", true);
    }

    @Test
    void reflectsSingletonAndEnumDataValuesThroughGenericDataParameter() {
        var singleton = ReflectionFeature.reflectionData(ReflectionFeature.ReflectedSingleton.INSTANCE);
        var enumValue = ReflectionFeature.reflectionData(ReflectionFeature.ReflectedStatus.REFLECTED_READY);

        assertThat(singleton.name()).isEqualTo("ReflectedSingleton");
        assertThat(singleton.fields()).isEmpty();
        assertThat(enumValue.name()).isEqualTo("REFLECTED_READY");
        assertThat(enumValue.fields()).isEmpty();
    }

    @Test
    void jsonShapedReflectionUsesGenericDataForNestedRecords() {
        var json = ReflectionFeature.reflectionJsonShape(
                new ReflectionFeature.ReflectedEnvelope("outer", new ReflectionFeature.A("inner", 7)));

        assertThat(json.value()).containsEntry("label", new ReflectionFeature.ReflectedJsonString("outer"));
        assertThat(json.value().get("payload")).isInstanceOfSatisfying(
                ReflectionFeature.ReflectedJsonObject.class,
                payload -> assertThat(payload.value())
                        .containsEntry("name", new ReflectionFeature.ReflectedJsonString("inner"))
                        .containsEntry("a", new ReflectionFeature.ReflectedJsonNumber(7))
        );
    }

}
