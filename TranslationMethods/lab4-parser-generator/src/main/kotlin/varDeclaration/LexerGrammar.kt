package varDeclaration

import generator.runtime.LexerRule
import kotlin.collections.List

public object LexerGrammar {
  public val LEXER_RULES: List<LexerRule> = listOf(
    LexerRule("VAR", "var".toRegex(), false), 
    LexerRule("COLON", ":".toRegex(), false), 
    LexerRule("LANGLE", "<".toRegex(), false), 
    LexerRule("RANGLE", ">".toRegex(), false), 
    LexerRule("COMMA", ",".toRegex(), false), 
    LexerRule("SEMICOLON", ";".toRegex(), false), 
    LexerRule("TYPE_ID", "[A-Z][A-Za-z_0-9]*".toRegex(), false), 
    LexerRule("VAR_ID", "[a-z][A-Za-z_0-9]*".toRegex(), false), 
    LexerRule("WS", "[ \\t\\r\\n]+".toRegex(), true), 
  )
}
