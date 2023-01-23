package info.kgeorgiy.ja.stupnikov.concurrent;

import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.Function;
import java.util.stream.IntStream;
import java.util.stream.Stream;

/**
 * Implementation of ParallelMapper interface which purpose is to perform parallel mapping of function on a list of arguments.
 */
public class ParallelMapperImpl implements ParallelMapper {
    private final List<Thread> threads;
    private final ParallelQueue<Runnable> queue = new ParallelQueue<>();

    /** Constructs ParallelMapperImpl with defined number of threads.
     *
     * @param threads number of threads
     */
    public ParallelMapperImpl(final int threads) {
        final Runnable r = () -> {
            while (!Thread.interrupted()) {
                try {
                    queue.take().run();
                } catch (final InterruptedException ignored) {
                    return;
                }
            }
        };
        this.threads = Stream.generate(() -> new Thread(r))
                .limit(threads)
                .peek(Thread::start)
                .toList();
    }

    /**
     * Maps function {@code f} over specified {@code args}.
     * Mapping for each element performs in parallel.
     *
     * @throws InterruptedException if calling thread was interrupted
     */
    @Override
    public <T, R> List<R> map(final Function<? super T, ? extends R> f, final List<? extends T> args) throws InterruptedException {
        final List<R> res = new ArrayList<>(Collections.nCopies(args.size(), null));
        final Counter counter = new Counter(args.size());
        IntStream.range(0, args.size()).<Runnable>mapToObj(i -> () -> {
            final R result = f.apply(args.get(i));
            synchronized (counter) {
                res.set(i, result);
                counter.countDown();
            }
        }).forEach(queue::put);
        counter.await();
        return res;
    }

    /** Stops all threads. All unfinished mappings leave in undefined state. */
    @Override
    public void close() {
        threads.forEach(Thread::interrupt);
        int i = 0;
        while (i < threads.size()) {
            try {
                threads.get(i).join();
                i++;
            } catch (final InterruptedException ignored) {
            }
        }
    }

    public static void main(final String[] args) {
        final List<Integer> list = List.of(1, 2, 3, 4, 5, 6);
        try(final ParallelMapper parallelMapper = new ParallelMapperImpl(2)) {
            try {
                System.out.println(parallelMapper.map((x) -> x + 1, list));
            } catch (final InterruptedException e) {
                System.out.println(e.getMessage());
            }
        }
    }

    private static class ParallelQueue <T> {
        private final Queue<T> queue = new ArrayDeque<>();

        public synchronized T take() throws InterruptedException {
            while (queue.isEmpty()) {
                wait();
            }
            return queue.poll();
        }

        public synchronized void put(final T e) {
            queue.offer(e);
            notify();
        }
    }

    private static class Counter {
        private int count;

        public Counter(final int count) {
            this.count = count;
        }

        public synchronized void countDown() {
            if (count <= 0) {
                throw new IllegalStateException();
            }
            if (--count == 0) {
                notify();
            }
        }

        public synchronized void await() throws InterruptedException {
            while (count != 0) {
                wait();
            }
        }
    }
}
