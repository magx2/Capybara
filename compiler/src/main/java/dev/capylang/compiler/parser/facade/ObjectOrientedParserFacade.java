package dev.capylang.compiler.parser.facade;

import dev.capylang.compiler.parser.facade.ParserSchema.ObjectOrientedModuleDto;
import dev.capylang.compiler.parser.facade.ParserSchema.ParseResultDto;
import dev.capylang.compiler.parser.facade.ParserSchema.SourceModuleDto;

import java.util.Collection;
import java.util.List;

public interface ObjectOrientedParserFacade {
    ParseResultDto<ObjectOrientedModuleDto> parseObjectOrientedModule(SourceModuleDto module);

    ParseResultDto<List<ObjectOrientedModuleDto>> parseObjectOrientedModules(Collection<SourceModuleDto> modules);
}
