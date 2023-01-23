// Generated from java-escape by ANTLR 4.11.1
package translator;

    import java.util.*;
    import java.lang.*;
    import java.util.stream.Collectors;

import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link PythonPreprocessorParser}.
 */
public interface PythonPreprocessorListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link PythonPreprocessorParser#file}.
	 * @param ctx the parse tree
	 */
	void enterFile(PythonPreprocessorParser.FileContext ctx);
	/**
	 * Exit a parse tree produced by {@link PythonPreprocessorParser#file}.
	 * @param ctx the parse tree
	 */
	void exitFile(PythonPreprocessorParser.FileContext ctx);
	/**
	 * Enter a parse tree produced by {@link PythonPreprocessorParser#stat}.
	 * @param ctx the parse tree
	 */
	void enterStat(PythonPreprocessorParser.StatContext ctx);
	/**
	 * Exit a parse tree produced by {@link PythonPreprocessorParser#stat}.
	 * @param ctx the parse tree
	 */
	void exitStat(PythonPreprocessorParser.StatContext ctx);
	/**
	 * Enter a parse tree produced by {@link PythonPreprocessorParser#idents}.
	 * @param ctx the parse tree
	 */
	void enterIdents(PythonPreprocessorParser.IdentsContext ctx);
	/**
	 * Exit a parse tree produced by {@link PythonPreprocessorParser#idents}.
	 * @param ctx the parse tree
	 */
	void exitIdents(PythonPreprocessorParser.IdentsContext ctx);
}