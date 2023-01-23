package info.kgeorgiy.ja.stupnikov.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.net.*;
import java.nio.ByteBuffer;
import java.nio.channels.*;
import java.util.*;
import java.util.concurrent.*;

import static java.nio.charset.StandardCharsets.UTF_8;

public class HelloUDPNonblockingServer implements HelloServer {
    private static final String PREFIX = "Hello, ";
    private Selector selector;
    private ExecutorService handler;
    private ExecutorService executor;
    private ConcurrentLinkedQueue<Map.Entry<SocketAddress, String>> sendQueue;

    private void send(DatagramChannel channel, SocketAddress address, String message) throws IOException {
        ByteBuffer buffer = ByteBuffer.wrap(message.getBytes(UTF_8));
        channel.send(buffer, address);
    }

    @Override
    public void start(int port, int threads) {
        try {
            selector = Selector.open();
        } catch (IOException e) {
            System.out.println("Can not open selector " + e.getMessage());
            return;
        }
        handler = Executors.newFixedThreadPool(threads);
        executor = Executors.newSingleThreadExecutor();
        sendQueue = new ConcurrentLinkedQueue<>();
        executor.submit(() -> {
            try {
                initializeChannel(port);
            } catch (IOException e) {
                System.out.println("Can not initialize channel " + e.getMessage());
                return;
            }
            while (selector.isOpen()) {
                try {
                    selector.select(HelloUtils.TIMEOUT);
                } catch (IOException e) {
                    System.out.println("Error during selection " + e.getMessage());
                }
                for (Iterator<SelectionKey> it = selector.selectedKeys().iterator(); it.hasNext(); ) {
                    final SelectionKey key = it.next();
                    DatagramChannel channel = (DatagramChannel) key.channel();
                    if (key.isReadable()) {
                        ByteBuffer buffer = ByteBuffer.allocate(1024);
                        try {
                            SocketAddress senderAddress = channel.receive(buffer);
                            handler.submit(() -> handlePacket(senderAddress, buffer));
                        } catch (IOException e) {
                            System.out.println("Error during receiving message " + e.getMessage());
                        }
                    }
                    if (key.isWritable() && !sendQueue.isEmpty()) {
                        var pair = sendQueue.poll();
                        try {
                            send(channel, pair.getKey(), pair.getValue());
                        } catch (IOException e) {
                            System.out.println("Error during sending message " + e.getMessage());
                        }
                    }
                    it.remove();
                }
            }
        });
    }

    @Override
    public void close() {
        selector.keys().forEach(key -> {
            try {
                key.channel().close();
            } catch (IOException e) {
                System.out.println("Error during closing channel " + e.getMessage());
            }
        });
        try {
            selector.close();
        } catch (IOException e) {
            System.out.println("Error during closing selector " + e.getMessage());
        }
        HelloUtils.shutdown(handler, 10);
        HelloUtils.shutdown(executor, 10);
    }

    private void handlePacket(SocketAddress senderAddress, ByteBuffer buffer) {
        String message = PREFIX + new String(buffer.array(), UTF_8).trim();
        sendQueue.add(Map.entry(senderAddress, message));
    }

    private void initializeChannel(int port) throws IOException {
        SocketAddress address = new InetSocketAddress(port);
        DatagramChannel channel = DatagramChannel.open();
        channel.bind(address);
        channel.configureBlocking(false);
        channel.register(selector, SelectionKey.OP_READ | SelectionKey.OP_WRITE);
    }

    public static void main(String[] args) {
        if (args.length != 2) {
            System.err.println("Invalid number of arguments.");
            return;
        }
        HelloServer helloServer = new HelloUDPNonblockingServer();
        helloServer.start(Integer.parseInt(args[0]), Integer.parseInt(args[1]));
    }
}
