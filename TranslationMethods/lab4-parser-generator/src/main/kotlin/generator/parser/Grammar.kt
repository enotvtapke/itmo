package generator.parser

import generator.runtime.LexerRule

data class Grammar(
    val lexerRules: List<LexerRule>,
    val parserRules: List<ParserRule>,
    val parserPrefix: String? = null
)