import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.tree.ParseTree
import translator.PythonLexer
import translator.PythonParser
import translator.PythonPreprocessorLexer
import translator.PythonPreprocessorParser
import java.nio.file.Path
import kotlin.io.path.writeText

val g: () -> Unit = {}

fun main() {

    invoke {
        g bb listOf(g, { })
    }
}

class NT {

}

object Context {
    infix fun (() -> Unit).bb(b: List<() -> Unit>): String {
        return "s"
    }
}

fun invoke(x: Context.() -> Unit) {

}


