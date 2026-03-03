package pl.grzeslowski.capybara.linker;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Stream;

public sealed interface ValueOrError<T> {
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
        return new ValueOrError.Value<>(concat);
    }

    static <T> ValueOrError<List<T>> joinWithList(ValueOrError<List<T>> list, ValueOrError<T> single) {
        if (list instanceof ValueOrError.Error<?>) {
            return list;
        }

        if (single instanceof ValueOrError.Error<?>) {
            return new ValueOrError.Error<>(((ValueOrError.Error<?>) single).errors());
        }

        var listValue = ((ValueOrError.Value<List<T>>) list).value();
        var singleValue = ((ValueOrError.Value<T>) single).value();
        var concat = Stream.concat(listValue.stream(), Stream.of(singleValue)).toList();
        return new ValueOrError.Value<>(concat);
    }

    <U> ValueOrError<U> map(java.util.function.Function<? super T, ? extends U> mapper);

    public record Value<T>(T value) implements ValueOrError<T> {

        @Override
        public <U> ValueOrError<U> map(Function<? super T, ? extends U> mapper) {
            return new Value<>(mapper.apply(value));
        }
    }

    public record Error<T>(
            List<SingleError> errors) implements ValueOrError<T> {
        public Error(SingleError error) {
            this(List.of(error));
        }

        public Error(String error) {
            this(new SingleError(error));
        }

        @Override
        public <U> ValueOrError<U> map(Function<? super T, ? extends U> mapper) {
            return new Error<>(errors);
        }

        public record SingleError(String message) {
        }

        public static <T> Error<T> join(List<SingleError> a, List<SingleError> b) {
            var errors = new ArrayList<SingleError>(a.size() + b.size());
            errors.addAll(a);
            errors.addAll(b);
            return new Error<>(List.copyOf(errors));
        }
    }

}
