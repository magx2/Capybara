package dev.capylang.generator;

import dev.capylang.compiler.CompiledType;
import dev.capylang.compiler.expression.*;
import dev.capylang.generator.java.JavaConst;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.regex.Pattern;

final class ConstDependencyOrder {
    private static final Pattern CONST_NAME_PATTERN = Pattern.compile("^_?[A-Z_][A-Z0-9_]*$");

    private ConstDependencyOrder() {
    }

    static List<JavaConst> order(
            Collection<JavaConst> constants,
            BiFunction<String, List<CompiledType>, String> emittedFunctionName
    ) {
        var byName = new LinkedHashMap<String, JavaConst>();
        for (var javaConst : constants) {
            byName.put(javaConst.name(), javaConst);
        }
        var states = new HashMap<String, VisitState>();
        var ordered = new ArrayList<JavaConst>();
        for (var name : byName.keySet()) {
            visit(name, byName, emittedFunctionName, states, ordered);
        }
        return ordered;
    }

    static boolean isConstCall(CompiledFunctionCall functionCall) {
        if (!functionCall.arguments().isEmpty()) {
            return false;
        }
        var name = simpleMethodName(functionCall.name());
        return name.contains("__local_const_") || isTopLevelConstName(name);
    }

    static boolean isTopLevelConstName(String name) {
        return CONST_NAME_PATTERN.matcher(name).matches();
    }

    private static void visit(
            String name,
            Map<String, JavaConst> byName,
            BiFunction<String, List<CompiledType>, String> emittedFunctionName,
            Map<String, VisitState> states,
            List<JavaConst> ordered
    ) {
        var state = states.get(name);
        if (state == VisitState.VISITED) {
            return;
        }
        if (state == VisitState.VISITING) {
            return;
        }
        states.put(name, VisitState.VISITING);
        var javaConst = byName.get(name);
        for (var dependency : dependencies(javaConst.expression(), byName.keySet(), emittedFunctionName)) {
            if (!dependency.equals(name)) {
                visit(dependency, byName, emittedFunctionName, states, ordered);
            }
        }
        states.put(name, VisitState.VISITED);
        ordered.add(javaConst);
    }

    private static Set<String> dependencies(
            CompiledExpression expression,
            Set<String> localConstNames,
            BiFunction<String, List<CompiledType>, String> emittedFunctionName
    ) {
        var dependencies = new LinkedHashSet<String>();
        collect(expression, localConstNames, emittedFunctionName, dependencies);
        return dependencies;
    }

    private static void collect(
            CompiledExpression expression,
            Set<String> localConstNames,
            BiFunction<String, List<CompiledType>, String> emittedFunctionName,
            Set<String> dependencies
    ) {
        switch (expression) {
            case CompiledFunctionCall functionCall -> {
                if (isConstCall(functionCall)) {
                    var dependency = emittedFunctionName.apply(
                            functionCall.name(),
                            functionCall.arguments().stream().map(CompiledExpression::type).toList()
                    );
                    if (localConstNames.contains(dependency)) {
                        dependencies.add(dependency);
                    }
                }
                functionCall.arguments().forEach(argument -> collect(argument, localConstNames, emittedFunctionName, dependencies));
            }
            case CompiledNumericWidening numericWidening ->
                    collect(numericWidening.expression(), localConstNames, emittedFunctionName, dependencies);
            case CompiledNewList newList ->
                    newList.values().forEach(value -> collect(value, localConstNames, emittedFunctionName, dependencies));
            case CompiledNewSet newSet ->
                    newSet.values().forEach(value -> collect(value, localConstNames, emittedFunctionName, dependencies));
            case CompiledNewDict newDict -> newDict.entries().forEach(entry -> {
                collect(entry.key(), localConstNames, emittedFunctionName, dependencies);
                collect(entry.value(), localConstNames, emittedFunctionName, dependencies);
            });
            case CompiledTupleExpression tupleExpression ->
                    tupleExpression.values().forEach(value -> collect(value, localConstNames, emittedFunctionName, dependencies));
            case CompiledFieldAccess fieldAccess ->
                    collect(fieldAccess.source(), localConstNames, emittedFunctionName, dependencies);
            case CompiledFunctionInvoke functionInvoke -> {
                collect(functionInvoke.function(), localConstNames, emittedFunctionName, dependencies);
                functionInvoke.arguments().forEach(argument -> collect(argument, localConstNames, emittedFunctionName, dependencies));
            }
            case CompiledObjectConstruction objectConstruction ->
                    objectConstruction.arguments().forEach(argument -> collect(argument, localConstNames, emittedFunctionName, dependencies));
            case CompiledIfExpression ifExpression -> {
                collect(ifExpression.condition(), localConstNames, emittedFunctionName, dependencies);
                collect(ifExpression.thenBranch(), localConstNames, emittedFunctionName, dependencies);
                collect(ifExpression.elseBranch(), localConstNames, emittedFunctionName, dependencies);
            }
            case CompiledInfixExpression infixExpression -> {
                collect(infixExpression.left(), localConstNames, emittedFunctionName, dependencies);
                collect(infixExpression.right(), localConstNames, emittedFunctionName, dependencies);
            }
            case CompiledLetExpression letExpression -> {
                collect(letExpression.value(), localConstNames, emittedFunctionName, dependencies);
                collect(letExpression.rest(), localConstNames, emittedFunctionName, dependencies);
            }
            case CompiledLambdaExpression lambdaExpression ->
                    collect(lambdaExpression.expression(), localConstNames, emittedFunctionName, dependencies);
            case CompiledIndexExpression indexExpression -> {
                collect(indexExpression.source(), localConstNames, emittedFunctionName, dependencies);
                collect(indexExpression.index(), localConstNames, emittedFunctionName, dependencies);
            }
            case CompiledSliceExpression sliceExpression -> {
                collect(sliceExpression.source(), localConstNames, emittedFunctionName, dependencies);
                sliceExpression.start().ifPresent(start -> collect(start, localConstNames, emittedFunctionName, dependencies));
                sliceExpression.end().ifPresent(end -> collect(end, localConstNames, emittedFunctionName, dependencies));
            }
            case CompiledNewData newData -> newData.assignments()
                    .forEach(assignment -> collect(assignment.value(), localConstNames, emittedFunctionName, dependencies));
            case CompiledUnwrapExpression unwrapExpression ->
                    collect(unwrapExpression.expression(), localConstNames, emittedFunctionName, dependencies);
            case CompiledMatchExpression matchExpression -> {
                collect(matchExpression.matchWith(), localConstNames, emittedFunctionName, dependencies);
                matchExpression.cases().forEach(matchCase -> {
                    matchCase.guard().ifPresent(guard -> collect(guard, localConstNames, emittedFunctionName, dependencies));
                    collect(matchCase.expression(), localConstNames, emittedFunctionName, dependencies);
                });
            }
            case CompiledPipeExpression pipeExpression -> {
                collect(pipeExpression.source(), localConstNames, emittedFunctionName, dependencies);
                collect(pipeExpression.mapper(), localConstNames, emittedFunctionName, dependencies);
            }
            case CompiledPipeFilterOutExpression pipeFilterOutExpression -> {
                collect(pipeFilterOutExpression.source(), localConstNames, emittedFunctionName, dependencies);
                collect(pipeFilterOutExpression.predicate(), localConstNames, emittedFunctionName, dependencies);
            }
            case CompiledPipeFlatMapExpression pipeFlatMapExpression -> {
                collect(pipeFlatMapExpression.source(), localConstNames, emittedFunctionName, dependencies);
                collect(pipeFlatMapExpression.mapper(), localConstNames, emittedFunctionName, dependencies);
            }
            case CompiledPipeReduceExpression pipeReduceExpression -> {
                collect(pipeReduceExpression.source(), localConstNames, emittedFunctionName, dependencies);
                collect(pipeReduceExpression.initialValue(), localConstNames, emittedFunctionName, dependencies);
                collect(pipeReduceExpression.reducerExpression(), localConstNames, emittedFunctionName, dependencies);
            }
            case CompiledEffectExpression effectExpression ->
                    collect(effectExpression.body(), localConstNames, emittedFunctionName, dependencies);
            case CompiledEffectBindExpression effectBindExpression -> {
                collect(effectBindExpression.source(), localConstNames, emittedFunctionName, dependencies);
                collect(effectBindExpression.rest(), localConstNames, emittedFunctionName, dependencies);
            }
            case CompiledReflectionValue reflectionValue ->
                    collect(reflectionValue.target(), localConstNames, emittedFunctionName, dependencies);
            case CompiledBooleanValue ignored -> {
            }
            case CompiledByteValue ignored -> {
            }
            case CompiledDoubleValue ignored -> {
            }
            case CompiledFloatValue ignored -> {
            }
            case CompiledIntValue ignored -> {
            }
            case CompiledLongValue ignored -> {
            }
            case CompiledStringValue ignored -> {
            }
            case CompiledVariable ignored -> {
            }
            case CompiledNothingValue ignored -> {
            }
        }
    }

    private static String simpleMethodName(String name) {
        var idx = Math.max(name.lastIndexOf('.'), name.lastIndexOf('/'));
        return idx >= 0 ? name.substring(idx + 1) : name;
    }

    private enum VisitState {
        VISITING,
        VISITED
    }
}
