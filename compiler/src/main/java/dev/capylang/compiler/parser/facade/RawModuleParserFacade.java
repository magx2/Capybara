package dev.capylang.compiler.parser.facade;

import dev.capylang.compiler.parser.facade.ParserSchema.ImportExtractionDto;
import dev.capylang.compiler.parser.facade.ParserSchema.ParseResultDto;
import dev.capylang.compiler.parser.facade.ParserSchema.ParsedModuleSetDto;
import dev.capylang.compiler.parser.facade.ParserSchema.SourceModuleDto;

import java.util.Collection;

public interface RawModuleParserFacade extends FunctionalParserFacade, ObjectOrientedParserFacade {
    ParseResultDto<ParsedModuleSetDto> parseModules(Collection<SourceModuleDto> modules);

    ImportExtractionDto extractImports(SourceModuleDto module);
}
