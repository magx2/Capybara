package dev.capylang.compiler;

import java.util.List;

public sealed interface GenericDataType extends CompiledType permits CompiledDataParentType, CompiledDataType {
    List<CompiledDataType.CompiledField> fields();

    Visibility visibility();
}
