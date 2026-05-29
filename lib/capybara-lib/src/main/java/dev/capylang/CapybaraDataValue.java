package dev.capylang;

public interface CapybaraDataValue {
    Object capybaraDataValueInfo();

    static boolean isDataValue(Object value) {
        return value instanceof CapybaraDataValue || value != null && value.getClass().isRecord();
    }

    static Object dataValueInfo(Object value) {
        if (value instanceof CapybaraDataValue dataValue) {
            return dataValue.capybaraDataValueInfo();
        }
        if (value == null || !value.getClass().isRecord()) {
            throw new IllegalArgumentException("reflection expects a Capybara data value");
        }
        var type = value.getClass();
        var packageName = type.getPackageName();
        var fields = java.util.Arrays.stream(type.getRecordComponents())
                .map(component -> new capy.metaProg.Reflection.FieldValueInfo(
                        component.getName(),
                        recordComponentTypeInfo(component.getType()),
                        recordComponentValue(value, component),
                        java.util.List.<capy.metaProg.Reflection.AnnotationInfo>of()
                ))
                .toList();
        return new capy.metaProg.Reflection.DataValueInfo(
                type.getSimpleName(),
                new capy.metaProg.Reflection.PackageInfo(packageName, packageName.replace('.', '/')),
                fields,
                java.util.List.<capy.metaProg.Reflection.AnnotationInfo>of()
        );
    }

    private static capy.metaProg.Reflection.AnyInfo recordComponentTypeInfo(Class<?> type) {
        if (java.util.List.class.isAssignableFrom(type)) {
            return new capy.metaProg.Reflection.ListInfo(
                    "List",
                    new capy.metaProg.Reflection.PackageInfo("", ""),
                    anyInfo()
            );
        }
        if (java.util.Set.class.isAssignableFrom(type)) {
            return new capy.metaProg.Reflection.SetInfo(
                    "Set",
                    new capy.metaProg.Reflection.PackageInfo("", ""),
                    anyInfo()
            );
        }
        if (java.util.Map.class.isAssignableFrom(type)) {
            return new capy.metaProg.Reflection.DictInfo(
                    "Dict",
                    new capy.metaProg.Reflection.PackageInfo("", ""),
                    anyInfo()
            );
        }
        return new capy.metaProg.Reflection.DataInfo(
                capybaraTypeName(type),
                capybaraPackageInfo(type),
                java.util.List.<capy.metaProg.Reflection.AnnotationInfo>of()
        );
    }

    private static capy.metaProg.Reflection.DataInfo anyInfo() {
        return new capy.metaProg.Reflection.DataInfo(
                "any",
                new capy.metaProg.Reflection.PackageInfo("", ""),
                java.util.List.<capy.metaProg.Reflection.AnnotationInfo>of()
        );
    }

    private static String capybaraTypeName(Class<?> type) {
        if (type == byte.class || type == Byte.class) {
            return "byte";
        }
        if (type == int.class || type == Integer.class) {
            return "int";
        }
        if (type == long.class || type == Long.class) {
            return "long";
        }
        if (type == float.class || type == Float.class) {
            return "float";
        }
        if (type == double.class || type == Double.class) {
            return "double";
        }
        if (type == boolean.class || type == Boolean.class) {
            return "bool";
        }
        if (type == String.class) {
            return "String";
        }
        return type.getSimpleName();
    }

    private static capy.metaProg.Reflection.PackageInfo capybaraPackageInfo(Class<?> type) {
        if (type.isPrimitive()
            || type == Byte.class
            || type == Integer.class
            || type == Long.class
            || type == Float.class
            || type == Double.class
            || type == Boolean.class
            || type == String.class) {
            return new capy.metaProg.Reflection.PackageInfo("", "");
        }
        return new capy.metaProg.Reflection.PackageInfo(type.getPackageName(), type.getPackageName().replace('.', '/'));
    }

    private static Object recordComponentValue(Object value, java.lang.reflect.RecordComponent component) {
        try {
            return component.getAccessor().invoke(value);
        } catch (ReflectiveOperationException e) {
            throw new IllegalStateException(
                    "Failed to read record component `" + component.getName() + "` from `" + value.getClass().getName() + "`",
                    e
            );
        }
    }
}
