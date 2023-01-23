package info.kgeorgiy.ja.stupnikov.concurrent;

import info.kgeorgiy.java.advanced.concurrent.ScalarIP;
import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.IntStream;
import java.util.stream.Stream;

/**
 * Apply operations to values from list with multiple threads.
 */
public class IterativeParallelism implements ScalarIP {
    private final ParallelMapper parallelMapper;

    /**
     * Constructs IterativeParallelism with defined parallelMapper.
     *
     * @param parallelMapper mapper that manages threads
     */
    public IterativeParallelism(ParallelMapper parallelMapper) {
        this.parallelMapper = parallelMapper;
    }

    /**
     * Constructs IterativeParallelism with defined parallelMapper.
     * Constructed class manages threads on its own.
     */
    public IterativeParallelism() {
        this.parallelMapper = null;
    }

    @Override
    public <T> T maximum(final int threads, final List<? extends T> values, final Comparator<? super T> comparator) throws InterruptedException {
        return applyToAll(threads, values,
                stream -> stream.max(comparator).orElse(null),
                stream -> stream.max(comparator).orElse(null));
    }

    @Override
    public <T> T minimum(final int threads, final List<? extends T> values, final Comparator<? super T> comparator) throws InterruptedException {
        return maximum(threads, values, comparator.reversed());
    }

    @Override
    public <T> boolean all(final int threads, final List<? extends T> values, final Predicate<? super T> predicate) throws InterruptedException {
        return !any(threads, values, predicate.negate());
    }

    @Override
    public <T> boolean any(final int threads, final List<? extends T> values, final Predicate<? super T> predicate) throws InterruptedException {
        return applyToAll(threads, values,
                stream -> stream.anyMatch(predicate),
                stream -> stream.anyMatch(Boolean::booleanValue));
    }

    private <T, R> R applyToAll(int threads, final List<T> values, final Function<Stream<T>, R> operator, final Function<Stream<R>, R> collector) throws InterruptedException {
        threads = Math.max(Math.min(threads, values.size()), 1);
        if (parallelMapper == null && threads == 1) {
            return operator.apply(values.stream());
        }

        final List<Stream<T>> parts = genPartition(threads, values);

        final List<R> res = parallelMapper == null ? map(operator, parts) : parallelMapper.map(operator, parts);

        return collector.apply(res.stream());
    }

    private <T, R> List<R> map(
            final Function<Stream<T>, R> operator,
            final List<Stream<T>> parts
    ) throws InterruptedException {
        final int threads = parts.size();
        final List<R> res = new ArrayList<>(Collections.nCopies(threads, null));
        final List<Thread> threadList = IntStream.range(0, threads).mapToObj(
                index ->
                        new Thread(() ->
                                res.set(index, operator.apply(parts.get(index)))
                        )
        ).toList();

        threadList.forEach(Thread::start);
        for (final var thread : threadList) {
            thread.join();
        }
        return res;
    }

    private <T> List<Stream<T>> genPartition(final int num, final List<T> values) {
        final int k = values.size() / num;
        final int mod = values.size() % num;
        List<Stream<T>> partition = new ArrayList<>();
        int shift = 0;
        for (int i = 0; i < num; i++) {
            int left = shift + i * k;
            if (shift < mod) {
                shift += 1;
            }
            int right = shift + i * k + k;
            partition.add(values.subList(left, right).stream());
        }

        return partition;
    }

    public static void main(final String[] args) {
        final IterativeParallelism iterativeParallelism = new IterativeParallelism(new ParallelMapperImpl(10));
        final List<Integer> list = List.of(1, 2, 3, 4, 5, 6);
        try {
            System.out.println(iterativeParallelism.minimum(2, list, Comparator.naturalOrder()));
        } catch (final InterruptedException e) {
            System.out.println(e.getMessage());
        }
    }
}
