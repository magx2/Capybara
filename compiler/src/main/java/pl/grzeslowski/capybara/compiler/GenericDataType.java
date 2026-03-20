package pl.grzeslowski.capybara.compiler;

import java.util.List;

public sealed interface GenericDataType extends LinkedType permits LinkedDataParentType, LinkedDataType {
    List<LinkedDataType.LinkedField> fields();
}
