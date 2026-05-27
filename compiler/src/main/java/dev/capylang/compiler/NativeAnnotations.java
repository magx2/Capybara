package dev.capylang.compiler;

import java.util.Optional;

public final class NativeAnnotations {
    public static final String NATIVE_PROVIDER_ANNOTATION_NAME = "NativeProvider";
    public static final String NATIVE_PROVIDER_ANNOTATION_MODULE_NAME = "NativeProvider";
    public static final String NATIVE_PROVIDER_ANNOTATION_MODULE_PATH = "capy/meta_prog";
    public static final String NATIVE_IMPLEMENTATION_ANNOTATION_NAME = "NativeImplementation";
    public static final String NATIVE_IMPLEMENTATION_ANNOTATION_MODULE_NAME = "NativeImplementation";
    public static final String NATIVE_IMPLEMENTATION_ANNOTATION_MODULE_PATH = "capy/meta_prog";

    private NativeAnnotations() {
    }

    public static boolean isNativeProviderAnnotation(CompiledAnnotation annotation) {
        return isAnnotation(
                annotation,
                NATIVE_PROVIDER_ANNOTATION_NAME,
                NATIVE_PROVIDER_ANNOTATION_MODULE_NAME,
                NATIVE_PROVIDER_ANNOTATION_MODULE_PATH
        );
    }

    public static boolean isNativeImplementationAnnotation(CompiledAnnotation annotation) {
        return isAnnotation(
                annotation,
                NATIVE_IMPLEMENTATION_ANNOTATION_NAME,
                NATIVE_IMPLEMENTATION_ANNOTATION_MODULE_NAME,
                NATIVE_IMPLEMENTATION_ANNOTATION_MODULE_PATH
        );
    }

    public static Optional<String> stringArgument(CompiledAnnotation annotation, String name) {
        return annotation.arguments().stream()
                .filter(argument -> argument.name().equals(name))
                .map(CompiledAnnotationArgument::value)
                .filter(CompiledAnnotationValue.StringValue.class::isInstance)
                .map(CompiledAnnotationValue.StringValue.class::cast)
                .map(CompiledAnnotationValue.StringValue::value)
                .map(NativeAnnotations::annotationStringLiteralValue)
                .findFirst();
    }

    private static boolean isAnnotation(CompiledAnnotation annotation, String name, String moduleName, String modulePath) {
        return name.equals(annotation.name())
               && moduleName.equals(annotation.packageName())
               && modulePath.equals(normalizedAnnotationModulePath(annotation.packagePath()));
    }

    private static String normalizedAnnotationModulePath(String path) {
        var normalized = path == null ? "" : path.replace('\\', '/');
        return normalized.startsWith("/") ? normalized.substring(1) : normalized;
    }

    private static String annotationStringLiteralValue(String value) {
        if (value.length() < 2) {
            return value;
        }
        var first = value.charAt(0);
        var last = value.charAt(value.length() - 1);
        if ((first == '"' && last == '"') || (first == '\'' && last == '\'')) {
            return value.substring(1, value.length() - 1);
        }
        return value;
    }
}
