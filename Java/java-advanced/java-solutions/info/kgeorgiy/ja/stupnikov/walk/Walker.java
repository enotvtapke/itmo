package info.kgeorgiy.ja.stupnikov.walk;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;

public class Walker {
    static private final Charset CHARSET = StandardCharsets.UTF_8;
    public static String INVALID_HASH = "0".repeat(40);

    public static void walk(String inputFile, String outputFile) throws WalkerException {
        Path inputFilePath;
        try {
            inputFilePath = Path.of(inputFile);
        } catch (InvalidPathException e) {
            throw new WalkerException("Bad input file path", e);
        }
        Path outputFilePath;
        try {
            outputFilePath = Path.of(outputFile);
        } catch (InvalidPathException e) {
            throw new WalkerException("Bad output file path", e);
        }
        try (
                var inputFileNames = Files.lines(inputFilePath, CHARSET)
        ) {
            if (outputFilePath.getParent() != null) {
                try {
                    Files.createDirectories(outputFilePath.getParent());
                } catch (IOException e) {
                    throw new WalkerException("Unable to create directory: " + e.getMessage(), e);
                }
            }
            StreamHasher hasher = new StreamHasher("SHA-1");
            try (var writer = Files.newBufferedWriter(outputFilePath)) {
                inputFileNames.forEach((inputFileName) -> {
                    try {
                        writer.write(String.format("%s %s%n", process(inputFileName, hasher), inputFileName));
                    } catch (IOException e) {
                        System.err.println("Error during writing the file: " + e.getMessage());
                    }
                });
            } catch (IOException e) {
                throw new WalkerException("Unable to open file for writing: " + e.getMessage(), e);
            }
        } catch (IOException e) {
            throw new WalkerException("Unable to open input file: " + e.getMessage(), e);
        }
    }

    private static String process(String inputFileName, StreamHasher hasher) {
        String hash;
        Path inputFilePath;
        try {
            inputFilePath = Path.of(inputFileName);
        } catch (InvalidPathException e) {
            return INVALID_HASH;
        }
        try (var inputStream = Files.newInputStream(inputFilePath)) {
            try {
                hash = hasher.hashHex(inputStream);
            } catch (IOException e) {
                return INVALID_HASH;
            }
        } catch (IOException e) {
            return INVALID_HASH;
        }
        return hash;
    }
}
