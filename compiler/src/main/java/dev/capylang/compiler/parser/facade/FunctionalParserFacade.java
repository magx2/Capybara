package dev.capylang.compiler.parser.facade;

import dev.capylang.compiler.parser.facade.ParserSchema.FunctionalModuleDto;
import dev.capylang.compiler.parser.facade.ParserSchema.FunctionalProgramDto;
import dev.capylang.compiler.parser.facade.ParserSchema.ParseResultDto;
import dev.capylang.compiler.parser.facade.ParserSchema.SourceModuleDto;

import java.util.Collection;

public interface FunctionalParserFacade {
    ParseResultDto<FunctionalModuleDto> parseFunctionalModule(SourceModuleDto module);

    ParseResultDto<FunctionalProgramDto> parseFunctionalModules(Collection<SourceModuleDto> modules);
}
