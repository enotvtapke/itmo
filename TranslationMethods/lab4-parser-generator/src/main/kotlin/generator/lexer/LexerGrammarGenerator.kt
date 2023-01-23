package generator.lexer

import com.squareup.kotlinpoet.CodeBlock
import com.squareup.kotlinpoet.ParameterizedTypeName.Companion.parameterizedBy
import com.squareup.kotlinpoet.PropertySpec
import com.squareup.kotlinpoet.TypeSpec
import com.squareup.kotlinpoet.asClassName
import generator.runtime.LexerRule

class LexerGrammarGenerator(private val lexerRules: List<LexerRule>) {
    fun generate() = TypeSpec.objectBuilder("LexerGrammar")
        .addProperty(
            PropertySpec
                .builder(
                    "LEXER_RULES",
                    List::class.asClassName().parameterizedBy(LexerRule::class.asClassName())
                )
                .initializer(
                    CodeBlock.builder()
                        .addStatement("listOf(")
                        .indent()
                        .apply { lexerRules.forEach { addStatement("%L, ", it.initializerCode()) } }
                        .unindent()
                        .add(")")
                        .build()
                )
                .build()
        ).build()

    private fun LexerRule.initializerCode() =
        CodeBlock.of("LexerRule(%S, %S.toRegex(), %L)", id, pattern, ignored)
}