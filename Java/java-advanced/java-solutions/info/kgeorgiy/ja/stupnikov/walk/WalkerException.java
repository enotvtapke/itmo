package info.kgeorgiy.ja.stupnikov.walk;

import java.io.IOException;

public class WalkerException extends IOException {
    public WalkerException(String errorMessage, Throwable err) {
        super(errorMessage, err);
    }
}
