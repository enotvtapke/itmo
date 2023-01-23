package info.kgeorgiy.ja.stupnikov.walk;

public class Walk {
    public static void main(String[] args) {
        if (args == null || args.length != 2 || args[0] == null || args[1] == null) {
            System.err.println("Invalid number of arguments. Usage: java Walk <input file> <output file>");
            return;
        }
        try {
            Walker.walk(args[0], args[1]);
        } catch (WalkerException e) {
            System.err.println(e.getMessage());
        }
    }
}
