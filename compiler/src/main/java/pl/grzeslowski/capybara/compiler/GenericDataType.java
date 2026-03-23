package pl.grzeslowski.capybara.compiler;

import java.util.List;

public sealed interface GenericDataType extends CompiledType permits CompiledDataParentType, CompiledDataType {
    List<CompiledDataType.CompiledField> fields();

    Visibility visibility();
}
