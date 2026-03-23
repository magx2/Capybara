package pl.grzeslowski.capybara.compiler;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collector;

public class ResultCollectionCollector<T>
        implements Collector<Result<T>, ResultCollectionCollector.Accumulator<T>, Result<List<T>>> {

    public static final class Accumulator<T> {
        private final List<T> values = new ArrayList<>();
        private final List<Result.Error.SingleError> errors = new ArrayList<>();

        void add(Result<T> valueOrError) {
            if (valueOrError instanceof Result.Error<T> error) {
                errors.addAll(error.errors());
                return;
            }

            values.add(((Result.Success<T>) valueOrError).value());
        }

        Accumulator<T> merge(Accumulator<T> other) {
            values.addAll(other.values);
            errors.addAll(other.errors);
            return this;
        }

        Result<List<T>> finish() {
            if (!errors.isEmpty()) {
                return new Result.Error<>(errors);
            }

            return Result.success(List.copyOf(values));
        }
    }

    @Override
    public Supplier<Accumulator<T>> supplier() {
        return Accumulator::new;
    }

    @Override
    public BiConsumer<Accumulator<T>, Result<T>> accumulator() {
        return Accumulator::add;
    }

    @Override
    public BinaryOperator<Accumulator<T>> combiner() {
        return Accumulator::merge;
    }

    @Override
    public Function<Accumulator<T>, Result<List<T>>> finisher() {
        return Accumulator::finish;
    }

    @Override
    public Set<Characteristics> characteristics() {
        return Set.of();
    }
}

