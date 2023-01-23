package info.kgeorgiy.ja.stupnikov.hello;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketAddress;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;

import static java.nio.charset.StandardCharsets.UTF_8;

/**
 * Contains utility methods for HelloUdp server and client.
 */
public class HelloUtils {
    /**
     * Timeout for receive a message from remote source.
     */
    public static final int TIMEOUT = 100;

    /**
     * Sends message to remote source.
     * @param socket socket to send from
     * @param receiver address of remote source
     * @param message message content
     * @return if message was sent
     */
    public static boolean sendMessage(DatagramSocket socket, SocketAddress receiver, String message) {
        byte[] buffer = message.getBytes(UTF_8);
        DatagramPacket packet = new DatagramPacket(buffer, buffer.length, receiver);
        try {
            socket.send(packet);
        } catch (IOException e) {
            System.out.println("Error during sending packet: " + e.getMessage());
            return false;
        }
        return true;
    }

    /**
     * Receives packet from remote source.
     * @param socket socket to receive on
     * @return received packet
     * @throws IOException if I/O error during receive occur
     */
    public static DatagramPacket receivePacket(DatagramSocket socket) throws IOException {
        byte[] receiveBuffer = new byte[socket.getReceiveBufferSize()];
        DatagramPacket receivedPacket = new DatagramPacket(receiveBuffer, receiveBuffer.length);
        socket.receive(receivedPacket);
        return receivedPacket;
    }

    /**
     * Extracts message from packet.
     * @param packet packet to extract message from
     * @return text of the message
     */
    public static String extractMessage(DatagramPacket packet) {
        return new String(packet.getData(), packet.getOffset(), packet.getLength(), UTF_8);
    }

    /**
     * Shutdowns executorService with timeout.
     * @param executorService service to shut down
     * @param timeout time in seconds to termination
     */
    public static void shutdown(ExecutorService executorService, int timeout) {
        executorService.shutdown();
        try {
            if (!executorService.awaitTermination(timeout, TimeUnit.SECONDS)) {
                executorService.shutdownNow();
            }
        } catch (InterruptedException ignored) {
            executorService.shutdownNow();
            Thread.currentThread().interrupt();
        }
    }
}
