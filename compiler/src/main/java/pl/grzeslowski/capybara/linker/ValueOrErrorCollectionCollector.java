package pl.grzeslowski.capybara.linker;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collector;

public class ValueOrErrorCollectionCollector<T>
        implements Collector<ValueOrError<T>, ValueOrErrorCollectionCollector.Accumulator<T>, ValueOrError<List<T>>> {

    public static final class Accumulator<T> {
        private final List<T> values = new ArrayList<>();
        private final List<ValueOrError.Error.SingleError> errors = new ArrayList<>();

        void add(ValueOrError<T> valueOrError) {
            if (valueOrError instanceof ValueOrError.Error<T> error) {
                errors.addAll(error.errors());
                return;
            }

            values.add(((ValueOrError.Value<T>) valueOrError).value());
        }

        Accumulator<T> merge(Accumulator<T> other) {
            values.addAll(other.values);
            errors.addAll(other.errors);
            return this;
        }

        ValueOrError<List<T>> finish() {
            if (!errors.isEmpty()) {
                return new ValueOrError.Error<>(errors);
            }

            return ValueOrError.success(List.copyOf(values));
        }
    }

    @Override
    public Supplier<Accumulator<T>> supplier() {
        return Accumulator::new;
    }

    @Override
    public BiConsumer<Accumulator<T>, ValueOrError<T>> accumulator() {
        return Accumulator::add;
    }

    @Override
    public BinaryOperator<Accumulator<T>> combiner() {
        return Accumulator::merge;
    }

    @Override
    public Function<Accumulator<T>, ValueOrError<List<T>>> finisher() {
        return Accumulator::finish;
    }

    @Override
    public Set<Characteristics> characteristics() {
        return Set.of();
    }
}
