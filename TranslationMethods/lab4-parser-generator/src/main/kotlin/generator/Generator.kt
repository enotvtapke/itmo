package generator

import com.squareup.kotlinpoet.ClassName
import com.squareup.kotlinpoet.FileSpec
import com.squareup.kotlinpoet.MemberName
import com.squareup.kotlinpoet.asClassName
import generator.lexer.LexerGenerator
import generator.lexer.LexerGrammarGenerator
import generator.parser.*
import grammarParser.ParserGeneratorGrammarLexer
import grammarParser.ParserGeneratorGrammarParser
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import java.nio.file.Path
import kotlin.reflect.KClass

class Generator(
    private val grammarFile: Path,
    private val outputDir: Path,
    private val basePackage: String,
    private val imports: List<KClass<*>> = listOf(),
    private val contextClass: KClass<*>? = null,
    private val contextName: String? = null,
) {
    fun generate() {
        val grammar = parseGrammar(grammarFile)
        generateLexer(grammar)
        generateParser(grammar)
    }

    private fun parseGrammar(grammar: Path): Grammar {
        val input = CharStreams.fromPath(grammar)
        val lexer = ParserGeneratorGrammarLexer(input)
        val tokens = CommonTokenStream(lexer)
        val parser = ParserGeneratorGrammarParser(tokens)
        val tree = parser.rules()
        val visitor = ParserGeneratorGrammarVisitorImpl()
        visitor.visit(tree)
        return visitor.getGrammar()
    }

    private fun generateLexer(grammar: Grammar) {
        val lexerGrammar = LexerGrammarGenerator(grammar.lexerRules).generate()
        FileSpec.builder(basePackage, "LexerGrammar").addType(lexerGrammar)
            .build().writeTo(outputDir)

        val lexer = LexerGenerator().generate(
            MemberName("$basePackage.LexerGrammar", "LEXER_RULES")
        )
        FileSpec.builder(basePackage, "Lexer").addType(lexer)
            .build().writeTo(outputDir)
    }

    private fun generateParser(grammar: Grammar) {
        val nonTerminalsGenerator = NonTerminalsGenerator(grammar.parserRules)
        val nonTerminals = nonTerminalsGenerator.generate()
        FileSpec.builder(basePackage, "NonTerminals")
            .apply { nonTerminals.forEach { addType(it) } }
            .addImports()
            .build().writeTo(outputDir)
        trimImports("NonTerminals")

        val firstFollowCalculator = FirstFollowCalculator(grammar.parserRules)
        val parser = ParserGenerator(
            grammar.parserRules, firstFollowCalculator, "lexer", contextClass, contextName
        ).generate(
            ClassName(basePackage, "Lexer"),
            basePackage
        )
        FileSpec.builder(basePackage, "Parser")
            .addType(parser)
            .addImports()
            .build().writeTo(outputDir)
        trimImports("Parser")
        grammar.parserPrefix?.let {
            addParserPrefix(it)
        }
    }

    private fun addParserPrefix(prefix: String) {
        val file = outputDir.resolve(basePackage.replace('.', '/')).resolve("Parser.kt").toFile()
        val imports = file.readLines().takeWhile {
            it.startsWith("package") || it.startsWith("import ") || it.isBlank()
        }
        val body = file.readLines().drop(imports.size)
        file.writeText(
            buildString {
                append(imports.joinToString("\n"))
                append("\n$prefix\n\n")
                append(body.joinToString("\n"))
            }
        )
    }

    private fun FileSpec.Builder.addImports() = apply {
        this@Generator.imports.forEach {
            addImport(
                it.asClassName().packageName,
                it.asClassName().simpleName
            )
        }
    }

    private fun trimImports(fileName: String) {
        val file = outputDir.resolve(basePackage.replace('.', '/')).resolve("$fileName.kt").toFile()
        file.writeText(
            file.readLines().filterNot { it.startsWith("import ") && !it.contains('.') }.joinToString("\n")
        )
    }
}