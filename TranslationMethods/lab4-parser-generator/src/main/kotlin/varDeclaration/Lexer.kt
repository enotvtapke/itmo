package varDeclaration

import generator.runtime.BaseLexer
import kotlin.CharSequence
import varDeclaration.LexerGrammar.LEXER_RULES

public class Lexer(
  input: CharSequence,
) : BaseLexer(input, LEXER_RULES)
