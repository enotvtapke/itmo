package generator.parser

import com.squareup.kotlinpoet.*
import generator.runtime.NonTerminal

class NonTerminalsGenerator(private val parserRules: List<ParserRule>) {
    fun generate(): List<TypeSpec> {
        return parserRules.map {
            nonTerminal(it)
        }
    }

    private fun nonTerminal(parserRule: ParserRule) =
        TypeSpec.classBuilder(parserRule.className)
            .apply {
                if (parserRule.returnValuesTypes.isNotEmpty()) {
                    addModifiers(KModifier.DATA)
                    primaryConstructor(
                        FunSpec.constructorBuilder()
                            .addParameters(parserRule.returnValuesTypes
                                .map {
                                    ParameterSpec.builder(it.label, it.typeClassName.copy(true)).defaultValue("null")
                                        .build()
                                }
                            )
                            .build()
                    )
                }
            }
            .addProperties(
                parserRule.returnValuesTypes
                    .map {
                        PropertySpec.builder(it.label, it.typeClassName.copy(true))
                            .mutable()
                            .initializer(it.label).build()
                    }
            )
            .superclass(NonTerminal::class.asClassName())
            .build()
}