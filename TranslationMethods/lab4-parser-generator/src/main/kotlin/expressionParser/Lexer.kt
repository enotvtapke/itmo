package expressionParser

import expressionParser.LexerGrammar.LEXER_RULES
import generator.runtime.BaseLexer
import kotlin.CharSequence

public class Lexer(
  input: CharSequence,
) : BaseLexer(input, LEXER_RULES)
