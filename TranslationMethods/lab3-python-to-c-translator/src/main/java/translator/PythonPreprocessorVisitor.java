// Generated from java-escape by ANTLR 4.11.1
package translator;

    import java.util.*;
    import java.lang.*;
    import java.util.stream.Collectors;

import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link PythonPreprocessorParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface PythonPreprocessorVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link PythonPreprocessorParser#file}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFile(PythonPreprocessorParser.FileContext ctx);
	/**
	 * Visit a parse tree produced by {@link PythonPreprocessorParser#stat}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStat(PythonPreprocessorParser.StatContext ctx);
	/**
	 * Visit a parse tree produced by {@link PythonPreprocessorParser#idents}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIdents(PythonPreprocessorParser.IdentsContext ctx);
}