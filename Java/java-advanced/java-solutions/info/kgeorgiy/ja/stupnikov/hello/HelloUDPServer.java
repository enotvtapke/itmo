package info.kgeorgiy.ja.stupnikov.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import static java.nio.charset.StandardCharsets.UTF_8;

public class HelloUDPServer implements HelloServer {
    private static final String PREFIX = "Hello, ";
    private ExecutorService handler;
    private DatagramSocket socket;

    private void handlePacket(DatagramPacket receivedPacket) {
        String answer = generateAnswer(receivedPacket);
        HelloUtils.sendMessage(socket, receivedPacket.getSocketAddress(), answer);
    }

    private String generateAnswer(DatagramPacket receivedPacket) {
        String message = new String(receivedPacket.getData(), receivedPacket.getOffset(), receivedPacket.getLength(), UTF_8);
        return PREFIX + message;
    }

    @Override
    public void start(int port, int threads) {
        handler = Executors.newFixedThreadPool(threads);
        try {
            socket = new DatagramSocket(port);
        } catch (SocketException e) {
            System.out.println("Can not open socket: " + e.getMessage());
            return;
        }
        Thread thread = new Thread(() -> {
            while (!socket.isClosed()) {
                try {
                    DatagramPacket receivedPacket = HelloUtils.receivePacket(socket);
                    handler.submit(() -> handlePacket(receivedPacket));
                } catch (IOException ignored) {
                }
            }
            Thread.currentThread().interrupt();
        });
        thread.start();
    }

    @Override
    public void close() {
        if (!socket.isClosed()) {
            socket.close();
        }
        handler.shutdown();
        try {
            if (!handler.awaitTermination(10, TimeUnit.SECONDS)) {
                handler.shutdownNow();
            }
        } catch (InterruptedException ignored) {
            handler.shutdownNow();
            Thread.currentThread().interrupt();
        }
    }

    public static void main(String[] args) {
        if (args.length != 2) {
            System.err.println("Invalid number of arguments.");
            return;
        }
        HelloServer helloServer = new HelloUDPServer();
        helloServer.start(Integer.parseInt(args[0]), Integer.parseInt(args[1]));
    }
}
