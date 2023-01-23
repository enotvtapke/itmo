package info.kgeorgiy.ja.stupnikov.implementor;

import info.kgeorgiy.java.advanced.implementor.ImplerException;
import info.kgeorgiy.java.advanced.implementor.JarImpler;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Parameter;
import java.net.URISyntaxException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Arrays;
import java.util.jar.Attributes;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;

/**
 * The {@code Implementor} class allow to generate source code and <var>.jar</var> files
 * implementing given interfaces in runtime.
 */
public class Implementor implements JarImpler {
    /**
     * Line separator in current OS.
     *
     * @see System#lineSeparator()
     */
    private final static String LINE_SEP = System.lineSeparator();

    /**
     * {@link SimpleFileVisitor} that deletes files on visit.
     */
    private static final SimpleFileVisitor<Path> DELETE_VISITOR = new SimpleFileVisitor<>() {
        @Override
        public FileVisitResult visitFile(final Path file, final BasicFileAttributes attrs) throws IOException {
            Files.delete(file);
            return FileVisitResult.CONTINUE;
        }

        @Override
        public FileVisitResult postVisitDirectory(final Path dir, final IOException exc) throws IOException {
            Files.delete(dir);
            return FileVisitResult.CONTINUE;
        }
    };

    /**
     * Main method to run {@code Implementor} from console.
     * <p>
     * Pass three arguments -jar, class-name and file-name.jar to generate jar file
     * file-name.jar with realization of interface class-name.
     *
     * @param args command line arguments
     */
    public static void main(final String[] args) {
        if (args == null || args.length != 3 || args[0] == null || args[1] == null || args[2] == null) {
            System.out.println("Wrong number of arguments");
            System.out.println("Usage: java Implementor -jar class-name file-name.jar");
            return;
        }

        if (!args[0].equals("-jar")) {
            System.out.println("Invalid argument: " + args[0]);
            System.out.println("Usage: java Implementor -jar class-name file-name.jar");
            return;
        }

        final Class<?> token;
        try {
            token = Class.forName(args[1]);
        } catch (final ClassNotFoundException e) {
            System.out.println("Unknown class name: " + args[1]);
            return;
        }

        try {
            new Implementor().implementJar(token, Path.of(args[2]));
        } catch (final ImplerException e) {
            System.out.println(e.getMessage());
        }
    }

    /**
     * Produces <var>.jar</var> file implementing class or interface specified by provided <var>token</var>.
     * <p>
     * Generated class classes name should be same as classes name of the type token with <var>Impl</var> suffix
     * added.
     *
     * @param token type token to create implementation for.
     * @param jarFile target <var>.jar</var> file.
     * @throws ImplerException when implementation cannot be generated.
     */
    @Override
    public void implementJar(final Class<?> token, final Path jarFile) throws ImplerException {
        final Path root;
        try {
            root = Files.createTempDirectory(Path.of(""), "temp");
        } catch (final IOException e) {
            throw new ImplerException(e.getMessage());
        }

        implement(token, root);
        compile(root, token);
        createJar(token, jarFile, root);
        try {
            Files.walkFileTree(root, DELETE_VISITOR);
        } catch (final IOException e) {
            throw new ImplerException(e.getMessage());
        }
    }

    /**
     * Produces code implementing class or interface specified by provided {@code token}.
     * <p>
     * Generated class classes name should be same as classes name of the type token with {@code Impl} suffix
     * added. Generated source code should be placed in the correct subdirectory of the specified
     * {@code root} directory and have correct file name. For example, the implementation of the
     * interface {@link java.util.List} should go to {@code $root/java/util/ListImpl.java}
     *
     *
     * @param token type token to create implementation for.
     * @param root root directory.
     * @throws ImplerException when implementation cannot be
     * generated.
     */
    @Override
    public void implement(final Class<?> token, final Path root) throws ImplerException {
        if (!token.isInterface() || Modifier.isPrivate(token.getModifiers())) {
            throw new ImplerException("Token should describe non private interface");
        }

        final Path path = root.resolve(getImplPath(token) + ".java");

        try {
            final var parent = path.getParent();
            if (parent != null) {
                Files.createDirectories(parent);
            }
        } catch (final IOException e) {
            throw new ImplerException("Unable to create directory: " + e.getMessage(), e);
        }

        try (final var writer = Files.newBufferedWriter(path)) {
            if (token.getPackage() != null) {
                writer.write(encode("package " + token.getPackageName() + ";" + LINE_SEP + LINE_SEP));
            }
            writer.write(encode("public class " + token.getSimpleName() + "Impl implements " + token.getCanonicalName() + " {" + LINE_SEP));
            writer.write(encode(Arrays.stream(token.getMethods()).map(Implementor::getMethod).collect(Collectors.joining(LINE_SEP))));
            writer.write(encode(LINE_SEP + "}"));
        } catch (final IOException e) {
            throw new ImplerException("Error during writing the file: " + e.getMessage(), e);
        }
    }

    /**
     * Encode {@code String} s to Unicode
     * @param s {@code String} to encode
     * @return {@code String} encoded in unicode
     */
    private static String encode(final String s) {
        final StringBuilder sb = new StringBuilder();
        for (final char c : s.toCharArray()) {
            sb.append(c < 128 ? String.valueOf(c) : String.format("\\u%04x", (int) c));
        }
        return sb.toString();
    }

    /**
     * Creates <var>.jar</var> file from <var>.class</var> file described by {@code token}.
     *
     * @param token type token to create <var>.jar</var> file for
     * @param jarFile path to jar file
     * @param sourceRoot path to directory with source code
     * @throws ImplerException
     * if an I/O error during writing <var>.jar</var> file happens
     *
     * @see JarOutputStream
     */
    private static void createJar(final Class<?> token, final Path jarFile, final Path sourceRoot) throws ImplerException {
        final String implPath = getImplPath(token) + ".class";

        final Manifest manifest = new Manifest();
        manifest.getMainAttributes().put(Attributes.Name.MANIFEST_VERSION, "1.0");

        try (final var jarOutputStream = new JarOutputStream(Files.newOutputStream(jarFile))) {
            jarOutputStream.putNextEntry(new ZipEntry(implPath));
            Files.copy(sourceRoot.resolve(implPath), jarOutputStream);
        } catch (final IOException e) {
            throw new ImplerException(e.getMessage(), e);
        }
    }

    /**
     * Generates implementation for {@code method}.
     *
     * @param method method to create implementation for
     * @return {@code String} representing {@code method} source code
     *
     * @see Method
     */
    private static String getMethod(final Method method) {
        return String.format(
                """
                    %s %s %s(%s)%s {
                        %s
                    }
                """,
                getModifiers(method),
                method.getReturnType().getCanonicalName(),
                method.getName(),
                getParameters(method),
                getExceptions(method),
                getBody(method)
        );
    }

    /**
     * Generates {@code method} modifiers.
     * Ignore modifiers {@link Modifier#TRANSIENT} and {@link Modifier#ABSTRACT}
     *
     * @param method {@code Method} to get modifiers for
     * @return {@code String} representing {@code method} modifiers
     *
     * @see Method#getModifiers()
     * @see Modifier
     */
    private static String getModifiers(final Method method) {
        return Modifier.toString(method.getModifiers() & ~(128 | Modifier.ABSTRACT));
    }

    /**
     * Generates {@code method} parameters.
     * Method parameter types have canonical names. All parameters are comma separated.
     *
     * @param method {@code Method} to get modifiers for
     * @return {@code String} representing {@code method} parameters
     *
     * @see Method#getParameters()
     * @see Parameter
     */
    private static String getParameters(final Method method) {
        return Arrays.stream(method.getParameters())
                .map(parameter -> parameter.getType().getCanonicalName() + " " + parameter.getName())
                .collect(Collectors.joining(", "));
    }

    /**
     * Generates {@link String} containing {@code method} exceptions.
     * <p>
     * If {@code method} throws more than one exception they are comma separated.
     *
     * @param method {@code Method} to get exceptions for
     * @return empty {@code String} if method do not throw exceptions or " throws"
     * followed by {@code String} representing {@code method} exceptions
     *
     * @see Method#getExceptionTypes()
     */
    private static String getExceptions(final Method method) {
        final var exceptions = method.getExceptionTypes();
        if (exceptions.length != 0) {
            return " throws " + Arrays.stream(exceptions)
                    .map(Class::getCanonicalName)
                    .collect(Collectors.joining(", "));
        } else {
            return "";
        }
    }

    /**
     * Generates {@code method} body.
     * <p>
     * Body compose of one line generated by code {@code "return" + getDefaultValue(method.getReturnType()) + ";"}.
     *
     * @param method {@code Method} to generate body for
     * @return {@code String} representing {@code method} body
     * 
     * @see Implementor#getDefaultValue(Class)
     */
    private static String getBody(final Method method) {
        return "return" + getDefaultValue(method.getReturnType()) + ";";
    }

    /**
     * Generate default value for type token {@code clazz}.
     * <p>
     * Every default value except default value for {@code void} lead by " ".
     *
     * @param clazz type token to generate default value for
     * @return {@code String} representing {@code clazz} default value
     */
    private static String getDefaultValue(final Class<?> clazz) {
        if (!clazz.isPrimitive()) {
            return " null";
        }
        if (clazz.equals(boolean.class)) {
            return " false";
        } else if (clazz.equals(void.class)) {
            return "";
        } else {
            return " 0";
        }
    }

    /**
     * Compiles source code.
     *
     * @param root root directory {@code Path} to {@code token} source code
     * @param token type token to compile source code for
     * @throws ImplerException
     * if compiler is not found
     * or an error during parsing generated source code path occurs
     * or compiler exit code is not {@code 0}.
     *
     * @see JavaCompiler
     */
    private static void compile(final Path root, final Class<?> token) throws ImplerException{
        final String file = root.resolve(getImplPath(token) + ".java").toString();
        final JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        if (compiler == null) {
            throw new ImplerException("Could not find java compiler, include tools.jar to classpath");
        }
        final String classPath;
        try {
            classPath = Path.of(token.getProtectionDomain().getCodeSource().getLocation().toURI()).toString();
        } catch (final URISyntaxException e) {
            throw new ImplerException(e);
        }
        final String classpath = root + File.pathSeparator + classPath;
        final String[] args = { file, "-cp", classpath };
        final int exitCode = compiler.run(null, null, null, args);
        if (exitCode != 0) {
            throw new ImplerException("Compiler exit code: " + exitCode + " but expected " + exitCode);
        }
    }

    /**
     * Generates path to {@code token} inside its package.
     * <p>
     * Generated {@code String} has no filename extension at the end.
     *
     * @param token type token to generate path for
     * @return {@code String} representing {@code token} path
     */
    private static String getImplPath(final Class<?> token) {
        return (token.getPackageName() + "." + token.getSimpleName() + "Impl").replace(".", "/");
    }
}
