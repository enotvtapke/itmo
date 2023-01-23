// Generated from java-escape by ANTLR 4.11.1
package grammarParser;
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link ParserGeneratorGrammarParser}.
 */
public interface ParserGeneratorGrammarListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link ParserGeneratorGrammarParser#rules}.
	 * @param ctx the parse tree
	 */
	void enterRules(ParserGeneratorGrammarParser.RulesContext ctx);
	/**
	 * Exit a parse tree produced by {@link ParserGeneratorGrammarParser#rules}.
	 * @param ctx the parse tree
	 */
	void exitRules(ParserGeneratorGrammarParser.RulesContext ctx);
	/**
	 * Enter a parse tree produced by {@link ParserGeneratorGrammarParser#prefix}.
	 * @param ctx the parse tree
	 */
	void enterPrefix(ParserGeneratorGrammarParser.PrefixContext ctx);
	/**
	 * Exit a parse tree produced by {@link ParserGeneratorGrammarParser#prefix}.
	 * @param ctx the parse tree
	 */
	void exitPrefix(ParserGeneratorGrammarParser.PrefixContext ctx);
	/**
	 * Enter a parse tree produced by {@link ParserGeneratorGrammarParser#myParserRule}.
	 * @param ctx the parse tree
	 */
	void enterMyParserRule(ParserGeneratorGrammarParser.MyParserRuleContext ctx);
	/**
	 * Exit a parse tree produced by {@link ParserGeneratorGrammarParser#myParserRule}.
	 * @param ctx the parse tree
	 */
	void exitMyParserRule(ParserGeneratorGrammarParser.MyParserRuleContext ctx);
	/**
	 * Enter a parse tree produced by {@link ParserGeneratorGrammarParser#return}.
	 * @param ctx the parse tree
	 */
	void enterReturn(ParserGeneratorGrammarParser.ReturnContext ctx);
	/**
	 * Exit a parse tree produced by {@link ParserGeneratorGrammarParser#return}.
	 * @param ctx the parse tree
	 */
	void exitReturn(ParserGeneratorGrammarParser.ReturnContext ctx);
	/**
	 * Enter a parse tree produced by {@link ParserGeneratorGrammarParser#alternatives}.
	 * @param ctx the parse tree
	 */
	void enterAlternatives(ParserGeneratorGrammarParser.AlternativesContext ctx);
	/**
	 * Exit a parse tree produced by {@link ParserGeneratorGrammarParser#alternatives}.
	 * @param ctx the parse tree
	 */
	void exitAlternatives(ParserGeneratorGrammarParser.AlternativesContext ctx);
	/**
	 * Enter a parse tree produced by {@link ParserGeneratorGrammarParser#nonterm}.
	 * @param ctx the parse tree
	 */
	void enterNonterm(ParserGeneratorGrammarParser.NontermContext ctx);
	/**
	 * Exit a parse tree produced by {@link ParserGeneratorGrammarParser#nonterm}.
	 * @param ctx the parse tree
	 */
	void exitNonterm(ParserGeneratorGrammarParser.NontermContext ctx);
	/**
	 * Enter a parse tree produced by {@link ParserGeneratorGrammarParser#args}.
	 * @param ctx the parse tree
	 */
	void enterArgs(ParserGeneratorGrammarParser.ArgsContext ctx);
	/**
	 * Exit a parse tree produced by {@link ParserGeneratorGrammarParser#args}.
	 * @param ctx the parse tree
	 */
	void exitArgs(ParserGeneratorGrammarParser.ArgsContext ctx);
	/**
	 * Enter a parse tree produced by {@link ParserGeneratorGrammarParser#types}.
	 * @param ctx the parse tree
	 */
	void enterTypes(ParserGeneratorGrammarParser.TypesContext ctx);
	/**
	 * Exit a parse tree produced by {@link ParserGeneratorGrammarParser#types}.
	 * @param ctx the parse tree
	 */
	void exitTypes(ParserGeneratorGrammarParser.TypesContext ctx);
	/**
	 * Enter a parse tree produced by {@link ParserGeneratorGrammarParser#type}.
	 * @param ctx the parse tree
	 */
	void enterType(ParserGeneratorGrammarParser.TypeContext ctx);
	/**
	 * Exit a parse tree produced by {@link ParserGeneratorGrammarParser#type}.
	 * @param ctx the parse tree
	 */
	void exitType(ParserGeneratorGrammarParser.TypeContext ctx);
	/**
	 * Enter a parse tree produced by {@link ParserGeneratorGrammarParser#token}.
	 * @param ctx the parse tree
	 */
	void enterToken(ParserGeneratorGrammarParser.TokenContext ctx);
	/**
	 * Exit a parse tree produced by {@link ParserGeneratorGrammarParser#token}.
	 * @param ctx the parse tree
	 */
	void exitToken(ParserGeneratorGrammarParser.TokenContext ctx);
	/**
	 * Enter a parse tree produced by {@link ParserGeneratorGrammarParser#label}.
	 * @param ctx the parse tree
	 */
	void enterLabel(ParserGeneratorGrammarParser.LabelContext ctx);
	/**
	 * Exit a parse tree produced by {@link ParserGeneratorGrammarParser#label}.
	 * @param ctx the parse tree
	 */
	void exitLabel(ParserGeneratorGrammarParser.LabelContext ctx);
	/**
	 * Enter a parse tree produced by {@link ParserGeneratorGrammarParser#terminal}.
	 * @param ctx the parse tree
	 */
	void enterTerminal(ParserGeneratorGrammarParser.TerminalContext ctx);
	/**
	 * Exit a parse tree produced by {@link ParserGeneratorGrammarParser#terminal}.
	 * @param ctx the parse tree
	 */
	void exitTerminal(ParserGeneratorGrammarParser.TerminalContext ctx);
	/**
	 * Enter a parse tree produced by {@link ParserGeneratorGrammarParser#tokens}.
	 * @param ctx the parse tree
	 */
	void enterTokens(ParserGeneratorGrammarParser.TokensContext ctx);
	/**
	 * Exit a parse tree produced by {@link ParserGeneratorGrammarParser#tokens}.
	 * @param ctx the parse tree
	 */
	void exitTokens(ParserGeneratorGrammarParser.TokensContext ctx);
	/**
	 * Enter a parse tree produced by {@link ParserGeneratorGrammarParser#myLexerRule}.
	 * @param ctx the parse tree
	 */
	void enterMyLexerRule(ParserGeneratorGrammarParser.MyLexerRuleContext ctx);
	/**
	 * Exit a parse tree produced by {@link ParserGeneratorGrammarParser#myLexerRule}.
	 * @param ctx the parse tree
	 */
	void exitMyLexerRule(ParserGeneratorGrammarParser.MyLexerRuleContext ctx);
}