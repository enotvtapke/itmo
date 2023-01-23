package info.kgeorgiy.ja.stupnikov.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.*;
import java.util.*;
import java.util.stream.IntStream;

import static java.nio.charset.StandardCharsets.UTF_8;

public class HelloUDPNonblockingClient implements HelloClient {

    private String receive(DatagramChannel channel) throws IOException {
        ByteBuffer buffer = ByteBuffer.allocate(1 << 15);
        channel.read(buffer);
        return new String(buffer.array(), UTF_8).trim();
    }

    private void send(DatagramChannel channel, String message) throws IOException {
        ByteBuffer buffer = ByteBuffer.wrap(message.getBytes(UTF_8));
        channel.write(buffer);
    }

    @Override
    public void run(String host, int port, String prefix, int threads, int requests) {
        SocketAddress remote = new InetSocketAddress(host, port);

        Selector selector;
        try {
            selector = Selector.open();
        } catch (IOException e) {
            System.out.println("Can not open selector " + e.getMessage());
            return;
        }
        IntStream.range(0, threads).forEach(threadIndex -> {
            try {
                DatagramChannel channel = DatagramChannel.open();
                channel.configureBlocking(false);
                channel.connect(remote);
                channel.register(selector, SelectionKey.OP_WRITE).attach(threadIndex);
            } catch (IOException e) {
                System.out.println("Error during establishing a connection " + e.getMessage());
            }
        });
        int[] counters = new int[threads];
        int counter = 0;
        while (counter < threads * requests) {
            try {
                selector.select(100);
                if (selector.selectedKeys().isEmpty()) {
                    selector.keys().forEach(key -> key.interestOps(SelectionKey.OP_WRITE));
                }
                for (Iterator<SelectionKey> it = selector.selectedKeys().iterator(); it.hasNext(); ) {
                    final SelectionKey key = it.next();
                    if (!key.isValid()) {
                        continue;
                    }
                    final DatagramChannel channel = (DatagramChannel) key.channel();
                    int threadIndex = (int) key.attachment();
                    if (key.isReadable() && (key.interestOps() & SelectionKey.OP_READ) == SelectionKey.OP_READ) {
                        String answer = receive(channel);
                        if (answer.matches(".*\\d\\D+\\d.*")) {
                            System.out.println(answer);
                            counters[threadIndex]++;
                            counter++;
                            key.interestOps(SelectionKey.OP_WRITE);
                        }
                    }
                    if (key.isWritable() && (key.interestOps() & SelectionKey.OP_WRITE) == SelectionKey.OP_WRITE) {
                        int packetIndex = counters[threadIndex];
                        if (packetIndex >= requests) {
                            key.interestOps(SelectionKey.OP_READ);
                            break;
                        }
                        String message = prefix + threadIndex + "_" + packetIndex;
                        send(channel, message);
                        key.interestOps(SelectionKey.OP_READ);
                    }
                    it.remove();
                }
            } catch (IOException e) {
                System.out.println("Error during sending messages " + e.getMessage());
            }
        }
        for (var key : selector.keys()) {
            try {
                key.channel().close();
            } catch (IOException e) {
                System.out.println("Error during closing channel " + e.getMessage());
            }
        }
    }

    public static void main(String[] args) {
        if (args.length != 5) {
            System.err.println("Invalid number of arguments.");
            return;
        }
        HelloClient helloClient = new HelloUDPNonblockingClient();
        helloClient.run(args[0],
                Integer.parseInt(args[1]),
                args[2],
                Integer.parseInt(args[3]),
                Integer.parseInt(args[4])
        );
    }
}
