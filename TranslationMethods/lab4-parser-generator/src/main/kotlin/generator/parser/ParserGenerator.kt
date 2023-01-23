package generator.parser

import com.squareup.kotlinpoet.*
import com.squareup.kotlinpoet.KModifier.PRIVATE
import com.squareup.kotlinpoet.ParameterizedTypeName.Companion.parameterizedBy
import generator.runtime.*
import kotlin.reflect.KClass

class ParserGenerator(
    private val parserRules: List<ParserRule>,
    private val firstFollowCalculator: FirstFollowCalculator,
    private val lexerPropertyName: String = "lexer",
    private val contextClass: KClass<*>? = null,
    private val contextName: String? = null,
) {
    fun generate(lexerClassName: ClassName, nonTerminalsPackage: String): TypeSpec {
        return TypeSpec.classBuilder("Parser")
            .addProperty(
                PropertySpec.builder(lexerPropertyName, lexerClassName)
                    .initializer("%T(input)", lexerClassName).addModifiers(PRIVATE).build()
            )
            .apply {
                if (contextClass != null) {
                    addProperty(
                        PropertySpec.builder(contextName ?: "ctx", contextClass)
                            .initializer("%T()", contextClass).build()
                    )
                }
            }
            .primaryConstructor(
                FunSpec.constructorBuilder()
                    .addParameter("input", String::class)
                    .addStatement("%L.nextToken()", lexerPropertyName)
                    .build()
            )
            .addFunctions(parserRules.map { ruleFunction(it, nonTerminalsPackage) })
            .build()
    }

    private fun ruleFunction(rule: ParserRule, nonTerminalsPackage: String): FunSpec {
        val nonTerminal = ClassName(nonTerminalsPackage, rule.className)
        return FunSpec.builder(rule.id)
            .addParameters(
                rule.argumentsTypes.map { it.asParameter() }
            )
            .returns(Node::class.asClassName().parameterizedBy(nonTerminal))
            .addStatement("val %L = %T()", rule.id, nonTerminal)
            .addStatement("val res = %T(%L)", Node::class, rule.id)
            .beginControlFlow("when (%L.curTerminal.id)", lexerPropertyName)
            .apply {
                rule.alternatives.forEach {
                    addCode(whenBranch(rule.id, it))
                }
            }
            .addStatement(
                "else -> throw %T(\"Invalid terminal \${%L.curTerminal}\", %L.curPos)",
                ParserException::class, lexerPropertyName, lexerPropertyName
            )
            .endControlFlow()
            .addStatement("return res")
            .build()
    }

    private fun whenBranch(ruleId: String, alternative: List<ParserRuleToken>) =
        CodeBlock.builder()
            .beginControlFlow("in listOf(%L) ->",
                firstFollowCalculator.first1(ruleId, alternative).joinToString(", ") { "\"" + it + "\"" })
            .apply {
                if (alternative.count { it !is CodeToken } == 0) addStatement(
                    "res.addChild(%T(%S, %S))",
                    Terminal::class, EPS.id, EPS.text
                )

                alternative.forEach { token ->
                    when (token) {
                        is CodeToken -> addStatement(token.block)
                        is TermToken -> {
                            if (token.label != null) {
                                addStatement("val %L = %L.curTerminal", token.label, lexerPropertyName)
                            }

                            addStatement("%L.expect(%S)", lexerPropertyName, token.id)
                            addStatement("res.addChild(%L.curTerminal)", lexerPropertyName)
                            addStatement("%L.nextToken()", lexerPropertyName)
                            addStatement("")
                        }

                        is NonTermToken -> {
                            if (token.label != null) {
                                addStatement(
                                    "val %L = %L(%L).also { res.addChild(it) }.label",
                                    token.label,
                                    token.id,
                                    token.args.joinToString(", ")
                                )
                            } else {
                                addStatement("res.addChild(%L(%L))", token.id, token.args.joinToString(", "))
                            }
                        }
                    }
                }
            }
            .endControlFlow()
            .build()
}