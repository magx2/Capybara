package pl.grzeslowski.capybara.linker;

import java.util.*;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Stream;

import static java.util.Collections.unmodifiableSortedSet;

public sealed interface ValueOrError<T> {
    static <T> ValueOrError<T> error(List<String> msgs) {
        return new Error<>(msgs.stream().map(msg -> new Error.SingleError(0, 0, "", msg)).toList());
    }

    static <T> ValueOrError<T> error(String... msgs) {
        return error(Arrays.stream(msgs).toList());
    }

    static <T> ValueOrError<T> success(T value) {
        return new Value<>(value);
    }

    static <T1, T2, OutT> ValueOrError<OutT> join(BiFunction<T1, T2, OutT> function, ValueOrError<T1> a, ValueOrError<T2> b) {
        if (a instanceof ValueOrError.Error<?> errorA && b instanceof ValueOrError.Error<?> errorB) {
            return Error.join(errorA.errors, errorB.errors);
        }
        if (a instanceof ValueOrError.Error<?> error) {
            return new Error<>(error.errors);
        }
        if (b instanceof ValueOrError.Error<?> error) {
            return new Error<>(error.errors);
        }

        var valueA = ((ValueOrError.Value<T1>) a).value();
        var valueB = ((ValueOrError.Value<T2>) b).value();
        return new Value<>(function.apply(valueA, valueB));
    }

    static <T> ValueOrError<List<T>> join(ValueOrError<List<T>> a, ValueOrError<List<T>> b) {
        if (a instanceof ValueOrError.Error<?> errorA && b instanceof ValueOrError.Error<?> errorB) {
            return Error.join(errorA.errors, errorB.errors);
        }
        if (a instanceof ValueOrError.Error<?>) {
            return a;
        }
        if (b instanceof ValueOrError.Error<?>) {
            return b;
        }

        var valueA = ((ValueOrError.Value<List<T>>) a).value().stream();
        var valueB = ((ValueOrError.Value<List<T>>) b).value().stream();
        var concat = Stream.concat(valueA, valueB).toList();
        return ValueOrError.success(concat);
    }

    static <T> ValueOrError<List<T>> joinWithList(ValueOrError<List<T>> list, ValueOrError<T> single) {
        if (list instanceof ValueOrError.Error<?>) {
            return list;
        }

        if (single instanceof ValueOrError.Error<?>) {
            return new Error<>(((ValueOrError.Error<?>) single).errors());
        }

        var listValue = ((ValueOrError.Value<List<T>>) list).value();
        var singleValue = ((ValueOrError.Value<T>) single).value();
        var concat = Stream.concat(listValue.stream(), Stream.of(singleValue)).toList();
        return ValueOrError.success(concat);
    }

    <U> ValueOrError<U> map(java.util.function.Function<? super T, ? extends U> mapper);

    <U> ValueOrError<U> flatMap(java.util.function.Function<? super T, ValueOrError<? extends U>> mapper);

    public record Value<T>(T value) implements ValueOrError<T> {

        @Override
        public <U> ValueOrError<U> map(Function<? super T, ? extends U> mapper) {
            return new Value<>(mapper.apply(value));
        }

        @SuppressWarnings("unchecked")
        @Override
        public <U> ValueOrError<U> flatMap(Function<? super T, ValueOrError<? extends U>> mapper) {
            return (ValueOrError<U>) mapper.apply(value);
        }
    }

    public record Error<T>(
            SortedSet<SingleError> errors) implements ValueOrError<T> {
        public Error(SingleError error) {
            this(unmodifiableSortedSet(new TreeSet<>(Set.of(error))));
        }
        public Error(Collection<SingleError> errors) {
            this(unmodifiableSortedSet(new TreeSet<>(Set.copyOf(errors))));
        }

        @Override
        public <U> ValueOrError<U> map(Function<? super T, ? extends U> mapper) {
            return new Error<>(errors);
        }

        @Override
        public <U> ValueOrError<U> flatMap(Function<? super T, ValueOrError<? extends U>> mapper) {
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
