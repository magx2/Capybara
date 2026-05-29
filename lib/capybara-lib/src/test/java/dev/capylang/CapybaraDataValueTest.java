package dev.capylang;

import capy.metaProg.Reflection;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CapybaraDataValueTest {
    private record SampleRecord(String name, int age, List<String> tags) {
    }

    @Test
    void shouldTreatJavaRecordsAsDataValues() {
        var value = new SampleRecord("Ada", 42, List.of("compiler", "capybara"));

        assertTrue(CapybaraDataValue.isDataValue(value));
        assertFalse(CapybaraDataValue.isDataValue("not data"));

        var info = (Reflection.DataValueInfo) CapybaraDataValue.dataValueInfo(value);

        assertEquals("SampleRecord", info.name());
        assertEquals("dev.capylang", info.pkg().name());
        assertEquals("dev/capylang", info.pkg().path());
        assertEquals(List.of("name", "age", "tags"), info.fields().stream().map(Reflection.FieldValueInfo::name).toList());
        assertEquals("Ada", info.fields().get(0).value());
        assertEquals(42, info.fields().get(1).value());
        assertEquals(List.of("compiler", "capybara"), info.fields().get(2).value());

        var nameType = (Reflection.DataInfo) info.fields().get(0).type();
        assertEquals("String", nameType.name());
        assertEquals("", nameType.pkg().name());
        var ageType = (Reflection.DataInfo) info.fields().get(1).type();
        assertEquals("int", ageType.name());
        assertEquals("", ageType.pkg().name());
        var tagsType = (Reflection.ListInfo) info.fields().get(2).type();
        assertEquals("List", tagsType.name());
        assertEquals("any", tagsType.element_type().name());
    }
}
