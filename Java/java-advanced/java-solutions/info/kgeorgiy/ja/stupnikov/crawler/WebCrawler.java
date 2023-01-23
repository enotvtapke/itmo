package info.kgeorgiy.ja.stupnikov.crawler;

import info.kgeorgiy.java.advanced.crawler.*;

import java.io.IOException;
import java.util.*;
import java.util.concurrent.*;
import java.util.stream.IntStream;

public class WebCrawler implements Crawler {
    private final Downloader downloader;

    private final ExecutorService downloaderService;
    private final ExecutorService extractorService;

    public WebCrawler(Downloader downloader, int downloaders, int extractors, int perHost) {
        if (perHost < downloaders) {
            throw new IllegalArgumentException("perHost should be not less than downloaders");
        }
        this.downloader = downloader;
        downloaderService =  Executors.newFixedThreadPool(downloaders);
        extractorService = Executors.newFixedThreadPool(extractors);
    }

    @Override
    public Result download(String url, int depth) {
        Set<String> extractedLinks = ConcurrentHashMap.newKeySet();
        extractedLinks.add(url);

        Phaser bfsIterationBarrier = new Phaser(1);
        Set<String> visited = ConcurrentHashMap.newKeySet();
        Set<String> downloaded = ConcurrentHashMap.newKeySet();
        Map<String, IOException> errors = new ConcurrentHashMap<>();
        IntStream.range(0, depth).forEach(i -> {
            for (String link : extractedLinks) {
                if (visited.add(link)) {
                    bfsIterationBarrier.register();
                    downloaderService.submit(() -> {
                        try {
                            Document document = downloader.download(link);
                            downloaded.add(link);
                            extractedLinks.remove(link);
                            if (i < depth - 1) {
                                bfsIterationBarrier.register();
                                extractorService.submit(() -> {
                                    try {
                                        extractedLinks.addAll(document.extractLinks());
                                    } catch (IOException e) {
                                        errors.put(link, e);
                                    }
                                    bfsIterationBarrier.arriveAndDeregister();
                                });
                            }
                        } catch (IOException e) {
                            errors.put(link, e);
                        }
                        bfsIterationBarrier.arriveAndDeregister();
                    });
                } else {
                    extractedLinks.remove(link);
                }
            }
            bfsIterationBarrier.arriveAndAwaitAdvance();
        });
        return new Result(downloaded.stream().toList(), errors);
    }

    @Override
    public void close() {
        downloaderService.shutdown();
        extractorService.shutdown();
        try {
            if (!downloaderService.awaitTermination(10, TimeUnit.SECONDS)) {
                downloaderService.shutdownNow();
                if (!extractorService.isTerminated()) {
                    extractorService.shutdownNow();
                }
            }
        } catch (InterruptedException e) {
            downloaderService.shutdownNow();
            if (!extractorService.isTerminated()) {
                extractorService.shutdownNow();
            }
            Thread.currentThread().interrupt();
        }
    }

    private static int parseArg(String[] args, int i) {
        if (args.length > i) {
            return Integer.parseInt(args[i]);
        } else {
            return 1;
        }
    }

    public static void main(String[] args) {
        if (args.length < 1 || args.length > 5) {
            System.out.println("Wrong number of arguments. Usage: WebCrawler url [depth [downloads [extractors [perHost]]]].");
            return;
        }
        int depth = parseArg(args, 0);
        int downloaders = parseArg(args, 1);
        int extractors = parseArg(args, 2);
        int perHost = parseArg(args, 3);
        if (args.length > 4) {
            perHost = Integer.parseInt(args[4]);
        }
        try (
                WebCrawler webCrawler = new WebCrawler(
                    new CachingDownloader(),
                    downloaders, extractors, perHost
                )
        ) {
            webCrawler.download(args[0], depth);
        } catch (IOException e) {
            System.out.println("Can not create downloader.");
        }
    }
}
