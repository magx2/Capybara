package dev.capylang.compiler.parser;

import java.util.Optional;

public sealed interface AnnotationValue permits AnnotationValue.StringValue,
        AnnotationValue.IntValue,
        AnnotationValue.LongValue,
        AnnotationValue.FloatValue,
        AnnotationValue.DoubleValue,
        AnnotationValue.BoolValue,
        AnnotationValue.NothingValue,
        AnnotationValue.TypeNameValue {
    Optional<SourcePosition> position();

    record StringValue(String value, Optional<SourcePosition> position) implements AnnotationValue {
    }

    record IntValue(String value, Optional<SourcePosition> position) implements AnnotationValue {
    }

    record LongValue(String value, Optional<SourcePosition> position) implements AnnotationValue {
    }

    record FloatValue(String value, Optional<SourcePosition> position) implements AnnotationValue {
    }

    record DoubleValue(String value, Optional<SourcePosition> position) implements AnnotationValue {
    }

    record BoolValue(boolean value, Optional<SourcePosition> position) implements AnnotationValue {
    }

    record NothingValue(Optional<SourcePosition> position) implements AnnotationValue {
    }

    record TypeNameValue(String name, Optional<SourcePosition> position) implements AnnotationValue {
    }
}
