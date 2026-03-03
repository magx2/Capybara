package pl.grzeslowski.capybara.linker;

import pl.grzeslowski.capybara.compiler.Module;
import pl.grzeslowski.capybara.compiler.Program;
import pl.grzeslowski.capybara.parser.DataDeclaration;
import pl.grzeslowski.capybara.parser.Type;
import pl.grzeslowski.capybara.parser.TypeDeclaration;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static java.util.function.Function.identity;
import static java.util.stream.Collectors.toMap;

public class CapybaraLinker {
    public static final CapybaraLinker INSTANCE = new CapybaraLinker();

    public ValueOrError<LinkedProgram> link(Program program) {
        return program.modules()
                .stream()
                .map(this::linkModule)
                .reduce(
                        (ValueOrError<List<LinkedModule>>) new ValueOrError.Value<List<LinkedModule>>(List.of()),
                        ValueOrError::joinWithList,
                        ValueOrError::join)
                .map(LinkedProgram::new);
    }

    private ValueOrError<LinkedModule> linkModule(Module module) {
        return types(module).map(LinkedModule::new);
    }

    private ValueOrError<Set<GenericDataType>> types(Module module) {
        var dataDeclarationsOrError = castList(module, DataDeclaration.class)
                .stream()
                .map(this::linkDataDeclaration)
                .reduce(
                        (ValueOrError<List<LinkedDataType>>) new ValueOrError.Value<List<LinkedDataType>>(List.of()),
                        ValueOrError::joinWithList,
                        ValueOrError::join);

        if (dataDeclarationsOrError instanceof ValueOrError.Error<?> error) {
            return new ValueOrError.Error<>(error.errors());
        }
        var dataDeclarations = ((ValueOrError.Value<List<LinkedDataType>>) dataDeclarationsOrError).value();

        var typeDeclarationsOrError = castList(module, TypeDeclaration.class)
                .stream()
                .map(typeDeclaration -> linkTypeDeclaration(typeDeclaration, dataDeclarations))
                .reduce(
                        (ValueOrError<List<LinkedDataParentType>>) new ValueOrError.Value<List<LinkedDataParentType>>(List.of()),
                        ValueOrError::joinWithList,
                        ValueOrError::join);

        if (typeDeclarationsOrError instanceof ValueOrError.Error<?> error) {
            return new ValueOrError.Error<>(error.errors());
        }
        var typeDeclarations = ((ValueOrError.Value<List<LinkedDataParentType>>) typeDeclarationsOrError).value();

        var set = new HashSet<GenericDataType>();
        set.addAll(dataDeclarations);
        set.addAll(typeDeclarations);
        return new ValueOrError.Value<>(set);
    }

    private ValueOrError<LinkedDataType> linkDataDeclaration(DataDeclaration dataDeclaration) {
        var dt = new LinkedDataType(
                dataDeclaration.name(),
                dataDeclaration.fields()
                        .stream()
                        .map(CapybaraLinker::linkField)
                        .toList());
        // todo: there might be linking errors, e.g. field types not found
        return new ValueOrError.Value<>(dt);
    }

    private static LinkedDataType.LinkedField linkField(DataDeclaration.DataField type) {
        return new LinkedDataType.LinkedField(type.name(), linkType(type.type()));
    }

    private static LinkedType linkType(Type type) {
        // TODO proper mapping of types
        return new PrimitiveLinkedType(type.name());
    }

    private ValueOrError<LinkedDataParentType> linkTypeDeclaration(TypeDeclaration typeDeclaration, List<LinkedDataType> dataDeclarations) {
        return findSubtypes(typeDeclaration.subTypes(), dataDeclarations)
                .map(subTypes -> new LinkedDataParentType(typeDeclaration.name(),
                        typeDeclaration.fields()
                                .stream()
                                .map(CapybaraLinker::linkField)
                                .toList(),
                        subTypes));
    }

    private ValueOrError<List<LinkedDataType>> findSubtypes(List<String> rawSubTypes, List<LinkedDataType> dataDeclarations) {
        var dataTypesMap = dataDeclarations.stream().collect(toMap(LinkedDataType::name, identity()));
        return rawSubTypes.stream()
                .map(key -> {
                    var dataType = dataTypesMap.get(key);
                    if (dataType == null) {
                        return new ValueOrError.Error<LinkedDataType>("Type " + key + " not found");
                    }
                    return new ValueOrError.Value<>(dataType);
                })
                .reduce(
                        (ValueOrError<List<LinkedDataType>>) new ValueOrError.Value<List<LinkedDataType>>(List.of()),
                        ValueOrError::joinWithList,
                        ValueOrError::join);
    }

    private static <T> List<T> castList(Module module, Class<T> clazz) {
        return module.functional().definitions()
                .stream()
                .filter(clazz::isInstance)
                .map(clazz::cast)
                .toList();
    }
}
