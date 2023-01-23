package generator.parser

import com.squareup.kotlinpoet.ClassName
import com.squareup.kotlinpoet.ParameterSpec

data class ParserRule(
    val id: String,
    val alternatives: List<List<ParserRuleToken>>,
    val argumentsTypes: List<Argument> = listOf(),
    val returnValuesTypes: List<Argument> = listOf(),
) {
    val className: String
        get() = id.replaceFirstChar { it.uppercaseChar() }
}

data class Argument(val label: String, val type: String) {
    fun asParameter(): ParameterSpec = ParameterSpec.builder(label, typeClassName).build()

    val typeClassName: ClassName
        get() {
            val `package` =
                if (type.contains('.')) {
                    type.substring(0, type.lastIndexOf('.'))
                } else {
                    ""
                }
            return ClassName(`package`, type)
        }
}

sealed class ParserRuleToken

data class TermToken(val id: String, val label: String? = null) : ParserRuleToken()
data class CodeToken(val block: String) : ParserRuleToken()
data class NonTermToken(val id: String, val args: List<String> = listOf(), val label: String? = null) :
    ParserRuleToken()
