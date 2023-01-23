package generator.lexer

import com.squareup.kotlinpoet.CodeBlock
import com.squareup.kotlinpoet.FunSpec
import com.squareup.kotlinpoet.MemberName
import com.squareup.kotlinpoet.TypeSpec
import generator.runtime.BaseLexer

class LexerGenerator {
    fun generate(lexerRules: MemberName) = TypeSpec.classBuilder("Lexer")
        .primaryConstructor(
            FunSpec.constructorBuilder()
                .addParameter("input", CharSequence::class)
                .build()
        )
        .superclass(BaseLexer::class)
        .addSuperclassConstructorParameter(CodeBlock.of("input"))
        .addSuperclassConstructorParameter(CodeBlock.of("%M", lexerRules))
        .build()
}