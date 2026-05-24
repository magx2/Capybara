package dev.capylang.compiler;

public sealed interface CompiledAnnotationValue permits CompiledAnnotationValue.StringValue,
        CompiledAnnotationValue.IntValue,
        CompiledAnnotationValue.LongValue,
        CompiledAnnotationValue.FloatValue,
        CompiledAnnotationValue.DoubleValue,
        CompiledAnnotationValue.BoolValue,
        CompiledAnnotationValue.NothingValue,
        CompiledAnnotationValue.TypeNameValue {
    record StringValue(String value) implements CompiledAnnotationValue {
    }

    record IntValue(String value) implements CompiledAnnotationValue {
    }

    record LongValue(String value) implements CompiledAnnotationValue {
    }

    record FloatValue(String value) implements CompiledAnnotationValue {
    }

    record DoubleValue(String value) implements CompiledAnnotationValue {
    }

    record BoolValue(boolean value) implements CompiledAnnotationValue {
    }

    record NothingValue() implements CompiledAnnotationValue {
    }

    record TypeNameValue(String name) implements CompiledAnnotationValue {
    }
}
