package generator.parser

import generator.runtime.LexerRule
import grammarParser.ParserGeneratorGrammarBaseVisitor
import grammarParser.ParserGeneratorGrammarParser

class ParserGeneratorGrammarVisitorImpl : ParserGeneratorGrammarBaseVisitor<Unit>() {
    private val lexerRules = mutableListOf<LexerRule>()
    private val parserRules = mutableListOf<ParserRule>()
    private var prefix: String? = null

    fun getGrammar() = Grammar(lexerRules, parserRules, prefix)

    override fun visitPrefix(ctx: ParserGeneratorGrammarParser.PrefixContext?) {
        prefix = ctx!!.text.removePrefix("@prefix").trim().trimCodeBlock()
    }

    override fun visitMyLexerRule(ctx: ParserGeneratorGrammarParser.MyLexerRuleContext?) {
        lexerRules.add(
            LexerRule(
                ctx!!.TYPE_ID().text,
                Regex(ctx.STRING().text.trim('\'').replace("\\\\", "\\")),
                ctx.SK() != null
            )
        )
    }

    override fun visitMyParserRule(ctx: ParserGeneratorGrammarParser.MyParserRuleContext?) {
        parserRules.add(
            ParserRule(
                id = ctx!!.VAR_ID().text,
                argumentsTypes = ctx.types()?.arguments() ?: listOf(),
                returnValuesTypes = ctx.return_()?.types()?.arguments() ?: listOf(),
                alternatives = ctx.alternatives().tokens().map { tokensContext ->
                    tokensContext.token().map { token ->
                        when {
                            token.CODE() != null -> CodeToken(token.text.trimCodeBlock())
                            token.terminal() != null -> TermToken(
                                token.terminal().TYPE_ID().text,
                                token.terminal()?.label()?.VAR_ID()?.text
                            )

                            token.nonterm() != null -> NonTermToken(
                                token.nonterm().VAR_ID().text,
                                token.nonterm().args()?.VAR_ID()?.map { it.text } ?: listOf(),
                                token.nonterm()?.label()?.VAR_ID()?.text
                            )

                            else -> {
                                throw IllegalStateException("Invalid token node type $token")
                            }
                        }
                    }
                }
            )
        )
    }

    private fun ParserGeneratorGrammarParser.TypesContext.arguments() =
        let { it.label().zip(it.TYPE_ID()) }.map { Argument(it.first.VAR_ID().text, it.second.text) }

    private fun String.trimCodeBlock() = removePrefix("{").removeSuffix("}").trim()
}