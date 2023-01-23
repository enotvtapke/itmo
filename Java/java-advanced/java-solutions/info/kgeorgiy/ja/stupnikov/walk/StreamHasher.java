package info.kgeorgiy.ja.stupnikov.walk;

import java.io.IOException;
import java.io.InputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class StreamHasher {
    MessageDigest digest;

    public StreamHasher(String algorithm) throws WalkerException {
        try {
            this.digest = MessageDigest.getInstance(algorithm);
        } catch (final NoSuchAlgorithmException e) {
            throw new WalkerException("Digest error: " + e.getMessage(), e);
        }
    }

    public byte[] hash(InputStream inputStream) throws IOException {
        int n = 0;
        byte[] buffer = new byte[8192];
        while (n != -1) {
            n = inputStream.read(buffer);
            if (n > 0) {
                digest.update(buffer, 0, n);
            }
        }
        return digest.digest();
    }

    private String byteArrayToHex(byte[] byteArray) {
        StringBuilder s = new StringBuilder();
        for (byte b : byteArray) {
            s.append(String.format("%02x", b));
        }
        return s.toString();
    }

    public String hashHex(InputStream inputStream) throws IOException {
        return byteArrayToHex(hash(inputStream));
    }
}
