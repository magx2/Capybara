package dev.capylang.compiler.parser.facade;

import dev.capylang.compiler.Result;
import dev.capylang.compiler.parser.CapybaraParser;
import dev.capylang.compiler.parser.ObjectOrientedModule;
import dev.capylang.compiler.parser.ObjectOrientedParser;
import dev.capylang.compiler.parser.ParserImportPreprocessor;
import dev.capylang.compiler.parser.Program;
import dev.capylang.compiler.parser.RawModule;
import dev.capylang.compiler.parser.facade.ParserSchema.*;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public final class AntlrParserFacade implements RawModuleParserFacade {
    public static final AntlrParserFacade INSTANCE = new AntlrParserFacade();

    private AntlrParserFacade() {
    }

    @Override
    public ParseResultDto<FunctionalModuleDto> parseFunctionalModule(SourceModuleDto module) {
        var parsed = new CapybaraParser().parseModule(ParserIrAdapters.rawModule(module));
        if (parsed instanceof Result.Success<dev.capylang.compiler.parser.Module> success) {
            return ParseResultDto.success(ParserDtoMapper.functionalModule(success.value()));
        }
        return ParseResultDto.error(diagnostics(((Result.Error<?>) parsed).errors()));
    }

    @Override
    public ParseResultDto<FunctionalProgramDto> parseFunctionalModules(Collection<SourceModuleDto> modules) {
        var rawModules = modules.stream()
                .map(ParserIrAdapters::rawModule)
                .toList();
        var parsed = CapybaraParser.INSTANCE.parseModule(rawModules);
        if (parsed instanceof Result.Success<Program> success) {
            return ParseResultDto.success(ParserDtoMapper.functionalProgram(success.value()));
        }
        return ParseResultDto.error(diagnostics(((Result.Error<?>) parsed).errors()));
    }

    @Override
    public ParseResultDto<ObjectOrientedModuleDto> parseObjectOrientedModule(SourceModuleDto module) {
        var parsed = ObjectOrientedParser.INSTANCE.parseModule(ParserIrAdapters.rawModule(module));
        if (parsed instanceof Result.Success<ObjectOrientedModule> success) {
            return ParseResultDto.success(ParserDtoMapper.objectOrientedModule(success.value()));
        }
        return ParseResultDto.error(diagnostics(((Result.Error<?>) parsed).errors()));
    }

    @Override
    public ParseResultDto<List<ObjectOrientedModuleDto>> parseObjectOrientedModules(Collection<SourceModuleDto> modules) {
        var parsed = ObjectOrientedParser.INSTANCE.parseModules(modules.stream()
                .map(ParserIrAdapters::rawModule)
                .toList());
        if (parsed instanceof Result.Success<List<ObjectOrientedModule>> success) {
            return ParseResultDto.success(success.value().stream()
                    .map(ParserDtoMapper::objectOrientedModule)
                    .toList());
        }
        return ParseResultDto.error(diagnostics(((Result.Error<?>) parsed).errors()));
    }

    @Override
    public ParseResultDto<ParsedModuleSetDto> parseModules(Collection<SourceModuleDto> modules) {
        var functionalModules = modules.stream()
                .filter(module -> module.sourceKind() == SourceKindDto.FUNCTIONAL)
                .toList();
        var objectOrientedModules = modules.stream()
                .filter(module -> module.sourceKind() == SourceKindDto.OBJECT_ORIENTED)
                .toList();
        var diagnostics = new ArrayList<SyntaxDiagnosticDto>();
        var parsedFunctional = parseFunctionalModules(functionalModules);
        diagnostics.addAll(parsedFunctional.diagnostics());
        var parsedObjectOriented = parseObjectOrientedModules(objectOrientedModules);
        diagnostics.addAll(parsedObjectOriented.diagnostics());
        if (!diagnostics.isEmpty()) {
            return ParseResultDto.error(diagnostics);
        }
        return ParseResultDto.success(new ParsedModuleSetDto(
                parsedFunctional.value().orElse(new FunctionalProgramDto(List.of())),
                parsedObjectOriented.value().orElse(List.of())
        ));
    }

    @Override
    public ImportExtractionDto extractImports(SourceModuleDto module) {
        var extracted = ParserImportPreprocessor.extract(module.input());
        return new ImportExtractionDto(
                module,
                extracted.source(),
                extracted.imports().stream()
                        .map(ParserDtoMapper::importDeclaration)
                        .toList(),
                extracted.importLines().stream()
                        .map(line -> new ImportLineDto(line.line(), ParserDtoMapper.importDeclaration(line.importDeclaration())))
                        .toList()
        );
    }

    private static List<SyntaxDiagnosticDto> diagnostics(Collection<Result.Error.SingleError> errors) {
        return errors.stream()
                .map(error -> new SyntaxDiagnosticDto(error.file(), error.line(), error.column(), error.message()))
                .toList();
    }
}
