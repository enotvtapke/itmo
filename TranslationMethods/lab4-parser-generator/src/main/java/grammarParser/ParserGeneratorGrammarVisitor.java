// Generated from java-escape by ANTLR 4.11.1
package grammarParser;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link ParserGeneratorGrammarParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface ParserGeneratorGrammarVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link ParserGeneratorGrammarParser#rules}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRules(ParserGeneratorGrammarParser.RulesContext ctx);
	/**
	 * Visit a parse tree produced by {@link ParserGeneratorGrammarParser#prefix}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrefix(ParserGeneratorGrammarParser.PrefixContext ctx);
	/**
	 * Visit a parse tree produced by {@link ParserGeneratorGrammarParser#myParserRule}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMyParserRule(ParserGeneratorGrammarParser.MyParserRuleContext ctx);
	/**
	 * Visit a parse tree produced by {@link ParserGeneratorGrammarParser#return}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitReturn(ParserGeneratorGrammarParser.ReturnContext ctx);
	/**
	 * Visit a parse tree produced by {@link ParserGeneratorGrammarParser#alternatives}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAlternatives(ParserGeneratorGrammarParser.AlternativesContext ctx);
	/**
	 * Visit a parse tree produced by {@link ParserGeneratorGrammarParser#nonterm}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNonterm(ParserGeneratorGrammarParser.NontermContext ctx);
	/**
	 * Visit a parse tree produced by {@link ParserGeneratorGrammarParser#args}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArgs(ParserGeneratorGrammarParser.ArgsContext ctx);
	/**
	 * Visit a parse tree produced by {@link ParserGeneratorGrammarParser#types}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTypes(ParserGeneratorGrammarParser.TypesContext ctx);
	/**
	 * Visit a parse tree produced by {@link ParserGeneratorGrammarParser#type}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitType(ParserGeneratorGrammarParser.TypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link ParserGeneratorGrammarParser#token}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitToken(ParserGeneratorGrammarParser.TokenContext ctx);
	/**
	 * Visit a parse tree produced by {@link ParserGeneratorGrammarParser#label}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLabel(ParserGeneratorGrammarParser.LabelContext ctx);
	/**
	 * Visit a parse tree produced by {@link ParserGeneratorGrammarParser#terminal}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTerminal(ParserGeneratorGrammarParser.TerminalContext ctx);
	/**
	 * Visit a parse tree produced by {@link ParserGeneratorGrammarParser#tokens}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTokens(ParserGeneratorGrammarParser.TokensContext ctx);
	/**
	 * Visit a parse tree produced by {@link ParserGeneratorGrammarParser#myLexerRule}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMyLexerRule(ParserGeneratorGrammarParser.MyLexerRuleContext ctx);
}