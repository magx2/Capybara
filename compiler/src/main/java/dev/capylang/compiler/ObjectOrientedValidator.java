package dev.capylang.compiler;

import capy.lang.Result;
import dev.capylang.compiler.parser.ObjectOriented;
import dev.capylang.compiler.parser.ObjectOrientedModule;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.TreeSet;

public final class ObjectOrientedValidator {
    public static final ObjectOrientedValidator INSTANCE = new ObjectOrientedValidator();

    private static final String PASS_CLASS_NAME = "dev.capylang.compiler.linking.ObjectOrientedValidationPass";

    @SuppressWarnings("unchecked")
    private static <T> java.util.Optional<T> typedOptional(java.util.Optional value) {
        return (java.util.Optional<T>) value;
    }

    private static java.util.Optional<String> optionalString(java.util.Optional value) {
        return typedOptional(value);
    }

    private static java.util.Optional<ObjectOriented.MethodBody> methodBody(java.util.Optional value) {
        return typedOptional(value);
    }

    private static java.util.Optional<ObjectOriented.Statement> statementOptional(java.util.Optional value) {
        return typedOptional(value);
    }

    public Result<List<ObjectOrientedModule>> validate(List<ObjectOrientedModule> modules) {
        if (modules.isEmpty()) {
            return Results.success(modules);
        }

        var validationErrors = ValidationPassAdapter.load().validateModules(modules.stream()
                .map(ModuleDtoBuilder::new)
                .map(ModuleDtoBuilder::module)
                .toList());

        if (validationErrors.isEmpty()) {
            return Results.success(modules);
        }

        var errors = new TreeSet<CompilerError>();
        validationErrors.forEach(error -> errors.add(error(error)));
        return CompilerErrors.result(errors);
    }

    private static CompilerError error(Object error) {
        var tuple = (List<?>) error;
        return new CompilerError(
                intAt(tuple, 0),
                intAt(tuple, 1),
                stringAt(tuple, 2),
                stringAt(tuple, 3)
        );
    }

    private static int intAt(List<?> tuple, int index) {
        return ((Number) tuple.get(index)).intValue();
    }

    private static String stringAt(List<?> tuple, int index) {
        return (String) tuple.get(index);
    }

    private static final class ValidationPassAdapter {
        private final Method validateModules;

        private ValidationPassAdapter() throws ReflectiveOperationException {
            var pass = Class.forName(PASS_CLASS_NAME);
            validateModules = pass.getMethod("validateModules", List.class);
        }

        static ValidationPassAdapter load() {
            try {
                return new ValidationPassAdapter();
            } catch (ReflectiveOperationException e) {
                throw new IllegalStateException("Object-oriented validation pass is unavailable", e);
            }
        }

        List<?> validateModules(List<Object> modules) {
            try {
                return (List<?>) validateModules.invoke(null, modules);
            } catch (IllegalAccessException e) {
                throw new IllegalStateException("Object-oriented validation pass is unavailable", e);
            } catch (InvocationTargetException e) {
                throw rethrow("Object-oriented validation pass failed", e.getCause());
            }
        }

        private IllegalStateException rethrow(String message, Throwable cause) {
            if (cause instanceof RuntimeException runtimeException) {
                throw runtimeException;
            }
            if (cause instanceof Error error) {
                throw error;
            }
            throw new IllegalStateException(message, cause);
        }
    }

    private static final class ModuleDtoBuilder {
        private final ObjectOrientedModule module;
        private final List<Object> blocks = new ArrayList<>();
        private final List<Object> statements = new ArrayList<>();
        private int nextBlockId;
        private int nextStatementId;

        private ModuleDtoBuilder(ObjectOrientedModule module) {
            this.module = module;
        }

        Object module() {
            var definitions = module.objectOriented().definitions().stream()
                    .map(this::definition)
                    .toList();
            var members = module.objectOriented().definitions().stream()
                    .flatMap(definition -> definition.members().stream().map(member -> member(definition.name(), member)))
                    .toList();
            return List.of(module.moduleFile(), definitions, members, List.copyOf(blocks), List.copyOf(statements));
        }

        private Object definition(ObjectOriented.TypeDeclaration definition) {
            if (definition instanceof ObjectOriented.ClassDeclaration classDeclaration) {
                return List.of(
                        "class",
                        classDeclaration.name(),
                        parameters(classDeclaration.constructorParameters()),
                        parents(classDeclaration.parents()),
                        classDeclaration.modifiers()
                );
            }
            if (definition instanceof ObjectOriented.TraitDeclaration traitDeclaration) {
                return List.of(
                        "trait",
                        traitDeclaration.name(),
                        List.of(),
                        parents(traitDeclaration.parents()),
                        List.of()
                );
            }
            if (definition instanceof ObjectOriented.InterfaceDeclaration interfaceDeclaration) {
                return List.of(
                        "interface",
                        interfaceDeclaration.name(),
                        List.of(),
                        parents(interfaceDeclaration.parents()),
                        List.of()
                );
            }
            throw new IllegalArgumentException("Unsupported object-oriented definition: " + definition.getClass().getName());
        }

        private Object member(String owner, ObjectOriented.MemberDeclaration member) {
            if (member instanceof ObjectOriented.FieldDeclaration fieldDeclaration) {
                return List.of(
                        owner,
                        "field",
                        fieldDeclaration.name(),
                        fieldDeclaration.type(),
                        fieldDeclaration.visibility(),
                        List.of(),
                        false,
                        emptyBody(),
                        optionalString(fieldDeclaration.initializer()).isPresent(),
                        optionalString(fieldDeclaration.initializer()).orElse(""),
                        List.of()
                );
            }
            if (member instanceof ObjectOriented.MethodDeclaration methodDeclaration) {
                return List.of(
                        owner,
                        "method",
                        methodDeclaration.name(),
                        methodDeclaration.returnType(),
                        methodDeclaration.visibility(),
                        methodDeclaration.modifiers(),
                        methodBody(methodDeclaration.body()).isPresent(),
                        methodBody(methodDeclaration.body()).map(this::body).orElseGet(this::emptyBody),
                        false,
                        "",
                        parameters(methodDeclaration.parameters())
                );
            }
            if (member instanceof ObjectOriented.InitBlock initBlock) {
                return List.of(
                        owner,
                        "init",
                        "init",
                        "void",
                        "public",
                        List.of(),
                        true,
                        blockBody(initBlock.body()),
                        false,
                        "",
                        List.of()
                );
            }
            throw new IllegalArgumentException("Unsupported object-oriented member: " + member.getClass().getName());
        }

        private Object body(ObjectOriented.MethodBody body) {
            if (body instanceof ObjectOriented.ExpressionBody expressionBody) {
                return List.of("expression", expressionBody.expression(), -1);
            }
            if (body instanceof ObjectOriented.StatementBlock statementBlock) {
                return blockBody(statementBlock);
            }
            throw new IllegalArgumentException("Unsupported object-oriented method body: " + body.getClass().getName());
        }

        private Object blockBody(ObjectOriented.StatementBlock block) {
            return List.of("block", "", block(block));
        }

        private Object emptyBody() {
            return List.of("none", "", -1);
        }

        private int block(ObjectOriented.StatementBlock block) {
            var id = nextBlockId++;
            var statementIds = block.statements().stream()
                    .map(this::statement)
                    .toList();
            blocks.add(List.of(id, statementIds));
            return id;
        }

        private int statement(ObjectOriented.Statement statement) {
            var id = nextStatementId++;
            statements.add(statementRow(id, statement));
            return id;
        }

        private Object statementRow(int id, ObjectOriented.Statement statement) {
            if (statement instanceof ObjectOriented.LetStatement letStatement) {
                return statement(
                        id,
                        "let",
                        letStatement.name(),
                        optionalString(letStatement.type()).orElse(""),
                        letStatement.expression(),
                        List.of(),
                        emptyBody(),
                        -1,
                        -1,
                        List.of(),
                        ""
                );
            }
            if (statement instanceof ObjectOriented.LocalMethodStatement localMethodStatement) {
                return statement(
                        id,
                        "local_method",
                        localMethodStatement.name(),
                        "",
                        "",
                        parameters(localMethodStatement.parameters()),
                        body(localMethodStatement.body()),
                        -1,
                        -1,
                        List.of(),
                        localMethodStatement.returnType()
                );
            }
            if (statement instanceof ObjectOriented.MutableVariableStatement mutableVariableStatement) {
                return statement(
                        id,
                        "mutable",
                        mutableVariableStatement.name(),
                        optionalString(mutableVariableStatement.type()).orElse(""),
                        mutableVariableStatement.expression(),
                        List.of(),
                        emptyBody(),
                        -1,
                        -1,
                        List.of(),
                        ""
                );
            }
            if (statement instanceof ObjectOriented.AssignmentStatement assignmentStatement) {
                return statement(id, "assignment", assignmentStatement.name(), "", assignmentStatement.expression(), List.of(), emptyBody(), -1, -1, List.of(), "");
            }
            if (statement instanceof ObjectOriented.ExpressionStatement expressionStatement) {
                return statement(id, "expression", "", "", expressionStatement.expression(), List.of(), emptyBody(), -1, -1, List.of(), "");
            }
            if (statement instanceof ObjectOriented.ThrowStatement throwStatement) {
                return statement(id, "throw", "", "", throwStatement.expression(), List.of(), emptyBody(), -1, -1, List.of(), "");
            }
            if (statement instanceof ObjectOriented.ReturnStatement returnStatement) {
                return statement(id, "return", "", "", returnStatement.expression(), List.of(), emptyBody(), -1, -1, List.of(), "");
            }
            if (statement instanceof ObjectOriented.IfStatement ifStatement) {
                return statement(
                        id,
                        "if",
                        "",
                        "",
                        ifStatement.condition(),
                        List.of(),
                        emptyBody(),
                        block(ifStatement.thenBranch()),
                        statementOptional(ifStatement.elseBranch()).map(this::statement).orElse(-1),
                        List.of(),
                        ""
                );
            }
            if (statement instanceof ObjectOriented.TryCatchStatement tryCatchStatement) {
                return statement(
                        id,
                        "try",
                        "",
                        "",
                        "",
                        List.of(),
                        emptyBody(),
                        block(tryCatchStatement.tryBlock()),
                        -1,
                        tryCatchStatement.catches().stream().map(this::catchClause).toList(),
                        ""
                );
            }
            if (statement instanceof ObjectOriented.WhileStatement whileStatement) {
                return statement(id, "while", "", "", whileStatement.condition(), List.of(), emptyBody(), block(whileStatement.body()), -1, List.of(), "");
            }
            if (statement instanceof ObjectOriented.DoWhileStatement doWhileStatement) {
                return statement(id, "do_while", "", "", doWhileStatement.condition(), List.of(), emptyBody(), block(doWhileStatement.body()), -1, List.of(), "");
            }
            if (statement instanceof ObjectOriented.ForEachStatement forEachStatement) {
                return statement(
                        id,
                        "foreach",
                        forEachStatement.name(),
                        optionalString(forEachStatement.type()).orElse(""),
                        forEachStatement.iterable(),
                        List.of(),
                        emptyBody(),
                        block(forEachStatement.body()),
                        -1,
                        List.of(),
                        ""
                );
            }
            if (statement instanceof ObjectOriented.StatementBlock statementBlock) {
                return statement(id, "block", "", "", "", List.of(), emptyBody(), block(statementBlock), -1, List.of(), "");
            }
            throw new IllegalArgumentException("Unsupported object-oriented statement: " + statement.getClass().getName());
        }

        private Object statement(
                int id,
                String kind,
                String name,
                String declaredType,
                String rawExpression,
                List<Object> parameters,
                Object body,
                int bodyId,
                int elseStatementId,
                List<Object> catches,
                String returnType
        ) {
            return List.of(id, kind, name, declaredType, rawExpression, parameters, body, bodyId, elseStatementId, catches, returnType);
        }

        private Object catchClause(ObjectOriented.CatchClause catchClause) {
            return List.of(catchClause.name(), block(catchClause.body()));
        }

        private List<Object> parameters(List<ObjectOriented.Parameter> parameters) {
            return parameters.stream()
                    .map(parameter -> List.of(parameter.name(), parameter.type()))
                    .map(Object.class::cast)
                    .toList();
        }

        private List<String> parents(List<ObjectOriented.TypeReference> parents) {
            return parents.stream().map(ObjectOriented.TypeReference::name).toList();
        }
    }
}
