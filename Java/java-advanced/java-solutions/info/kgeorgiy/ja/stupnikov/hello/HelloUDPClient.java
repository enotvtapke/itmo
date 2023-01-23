package info.kgeorgiy.ja.stupnikov.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.io.IOException;
import java.net.*;
import java.util.stream.IntStream;

public class HelloUDPClient implements HelloClient {
    private static final int SO_TIMEOUT = 100;

    @Override
    public void run(String host, int port, String prefix, int threads, int requests) {
        SocketAddress address = new InetSocketAddress(host, port);
        var threadList = IntStream.range(0, threads).mapToObj(threadIndex ->
            new Thread(() -> {
                try (DatagramSocket socket = new DatagramSocket()) {
                    socket.setSoTimeout(SO_TIMEOUT);
                    for (int packetIndex = 0; packetIndex < requests; packetIndex++) {
                        String message = prefix + threadIndex + "_" + packetIndex;
                        while (!socket.isClosed()) {
                            if (!HelloUtils.sendMessage(socket, address, message)) {
                                continue;
                            }

                            try {
                                String received = HelloUtils.extractMessage(HelloUtils.receivePacket(socket));
                                if (received.matches("Hello, " + prefix + threadIndex + "_" + packetIndex)) {
                                    System.out.println(received);
                                    break;
                                }
                            } catch (IOException e) {
                                System.out.println("Error during receiving packet: " + e.getMessage());
                            }
                        }
                    }
                }  catch (SocketException e) {
                    System.out.println("Can not open socket: " + e.getMessage());
                }
            })
        ).toList();


        threadList.forEach(Thread::start);

        for (final var thread : threadList) {
            try {
                thread.join();
            } catch (InterruptedException ignored) {}
        }
    }

    public static void main(String[] args) {
        if (args.length != 5) {
            System.err.println("Invalid number of arguments.");
            return;
        }
        HelloClient helloClient = new HelloUDPClient();
        helloClient.run(args[0],
                Integer.parseInt(args[1]),
                args[2],
                Integer.parseInt(args[3]),
                Integer.parseInt(args[4])
        );
    }
}
