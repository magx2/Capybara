package dev.capylang.compiler;

import capy.lang.Result;

import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Stream;

public final class ResultOps {
    private ResultOps() {
    }

    public static <T> Result<T> error(Result.Error<?> error) {
        return Results.error(Results.errorMessage(error));
    }

    public static <T, Y> Result<Y> map(Result<T> result, Function<T, Y> mapper) {
        return Results.mapSuccess(result, mapper);
    }

    public static <T, Y> Result<Y> flatMap(Result<T> result, Function<T, Result<Y>> mapper) {
        return Results.flatMapSuccess(result, mapper);
    }

    public static <T1, T2, OutT> Result<OutT> join(BiFunction<T1, T2, OutT> function, Result<T1> a, Result<T2> b) {
        return a.reduce(
                left -> map(b, right -> function.apply(left, right)),
                leftError -> b.reduce(
                        ignored -> Results.error(leftError),
                        rightError -> Results.error(List.of(leftError, rightError))));
    }

    public static <T> Result<List<T>> join(Result<List<T>> a, Result<List<T>> b) {
        return a.reduce(
                left -> map(b, right -> Stream.concat(left.stream(), right.stream()).toList()),
                leftError -> b.reduce(
                        ignored -> Results.error(leftError),
                        rightError -> Results.error(List.of(leftError, rightError))));
    }

    public static <T> Result<List<T>> joinWithList(Result<List<T>> list, Result<T> single) {
        return flatMap(list, values -> map(single, value -> Stream.concat(values.stream(), Stream.of(value)).toList()));
    }
}
