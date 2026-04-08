package dev.capylang.compiler;

import java.util.*;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Stream;

import static java.util.Collections.unmodifiableSortedSet;

public sealed interface Result<T> {
    static <T> Result<T> error(List<String> msgs) {
        return new Error<>(msgs.stream().map(msg -> new Error.SingleError(0, 0, "", msg)).toList());
    }

    static <T> Result<T> error(String... msgs) {
        return error(Arrays.stream(msgs).toList());
    }

    static <T> Result<T> success(T value) {
        return new Success<>(value);
    }

    static <T1, T2, OutT> Result<OutT> join(BiFunction<T1, T2, OutT> function, Result<T1> a, Result<T2> b) {
        if (a instanceof Result.Error<?> errorA && b instanceof Result.Error<?> errorB) {
            return Error.join(errorA.errors, errorB.errors);
        }
        if (a instanceof Result.Error<?> error) {
            return new Error<>(error.errors);
        }
        if (b instanceof Result.Error<?> error) {
            return new Error<>(error.errors);
        }

        var valueA = ((Result.Success<T1>) a).value();
        var valueB = ((Result.Success<T2>) b).value();
        return new Success<>(function.apply(valueA, valueB));
    }

    static <T> Result<List<T>> join(Result<List<T>> a, Result<List<T>> b) {
        if (a instanceof Result.Error<?> errorA && b instanceof Result.Error<?> errorB) {
            return Error.join(errorA.errors, errorB.errors);
        }
        if (a instanceof Result.Error<?>) {
            return a;
        }
        if (b instanceof Result.Error<?>) {
            return b;
        }

        var valueA = ((Result.Success<List<T>>) a).value().stream();
        var valueB = ((Result.Success<List<T>>) b).value().stream();
        var concat = Stream.concat(valueA, valueB).toList();
        return Result.success(concat);
    }

    static <T> Result<List<T>> joinWithList(Result<List<T>> list, Result<T> single) {
        if (list instanceof Result.Error<?>) {
            return list;
        }

        if (single instanceof Result.Error<?>) {
            return new Error<>(((Result.Error<?>) single).errors());
        }

        var listValue = ((Result.Success<List<T>>) list).value();
        var singleValue = ((Result.Success<T>) single).value();
        var concat = Stream.concat(listValue.stream(), Stream.of(singleValue)).toList();
        return Result.success(concat);
    }

    <U> Result<U> map(java.util.function.Function<? super T, ? extends U> mapper);

    <U> Result<U> flatMap(java.util.function.Function<? super T, Result<? extends U>> mapper);

    record Success<T>(T value) implements Result<T> {
        @Override
        public <U> Result<U> map(Function<? super T, ? extends U> mapper) {
            return new Success<>(mapper.apply(value));
        }

        @SuppressWarnings("unchecked")
        @Override
        public <U> Result<U> flatMap(Function<? super T, Result<? extends U>> mapper) {
            return (Result<U>) mapper.apply(value);
        }
    }

    record Error<T>(SortedSet<SingleError> errors) implements Result<T> {
        public Error(SingleError error) {
            this(unmodifiableSortedSet(new TreeSet<>(Set.of(error))));
        }

        public Error(Collection<SingleError> errors) {
            this(unmodifiableSortedSet(new TreeSet<>(Set.copyOf(errors))));
        }

        @Override
        public <U> Result<U> map(Function<? super T, ? extends U> mapper) {
            return new Error<>(errors);
        }

        @Override
        public <U> Result<U> flatMap(Function<? super T, Result<? extends U>> mapper) {
            return new Error<>(errors);
        }

        public record SingleError(int line, int column, String file, String message) implements Comparable<SingleError> {
            public SingleError(String message) {
                this(0, 0, "", message);
            }

            @Override
            public int compareTo(SingleError o) {
                return Comparator
                        .comparing(SingleError::file)
                        .thenComparing(SingleError::line)
                        .thenComparing(SingleError::column)
                        .thenComparing(SingleError::message)
                        .compare(this, o);
            }

            @Override
            public String toString() {
                return "%s %d:%s: %s".formatted(file, line, column, message);
            }
        }

        public static <T> Error<T> join(Collection<SingleError> a, Collection<SingleError> b) {
            var errors = new TreeSet<SingleError>();
            errors.addAll(a);
            errors.addAll(b);
            return new Error<>(errors);
        }
    }
}
