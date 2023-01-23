import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.tree.ParseTree
import translator.PythonLexer
import translator.PythonParser
import translator.PythonPreprocessorLexer
import translator.PythonPreprocessorParser
import java.nio.file.Path
import kotlin.io.path.writeText

fun preprocess() {
    val input = CharStreams.fromPath(Path.of("src/main/resources/test.py"))
    val lexer = PythonPreprocessorLexer(input)
    val tokens = CommonTokenStream(lexer)
    val parser = PythonPreprocessorParser(tokens)
    val tree: ParseTree = parser.file()
    val res = parser.translated.toString()
    println(res)
    Path.of("src/main/resources/test_preprocessed.py").writeText(res)

    println(tree.toStringTree(parser))
}

fun main() {
    preprocess()
    val input = CharStreams.fromPath(Path.of("src/main/resources/test_preprocessed.py"))
    val lexer = PythonLexer(input)
    val tokens = CommonTokenStream(lexer)
    val parser = PythonParser(tokens)
    val tree: ParseTree = parser.file()
    val res = parser.translated.toString()
    println(res)
    Path.of("src/main/resources/test.c").writeText(res)

    println(tree.toStringTree(parser))
}
