package expressionParser

import app.Context
import generator.runtime.Node
import generator.runtime.ParserException
import generator.runtime.Terminal
import java.math.BigDecimal
import kotlin.String

public class Parser(
  input: String,
) {
  private val lexer: Lexer = Lexer(input)

  public val context: Context = Context()
  init {
    lexer.nextToken()
  }

  public fun expr(acc: BigDecimal): Node<Expr> {
    val expr = Expr()
    val res = Node(expr)
    when (lexer.curTerminal.id) {
      in listOf("NUMBER", "LBRACKET") -> {
        val t = term(acc).also { res.addChild(it) }.label
        val tVal = t.res!!
        val e = expr_(tVal).also { res.addChild(it) }.label
        expr.res = e.res
      }
      else -> throw ParserException("Invalid terminal ${lexer.curTerminal}", lexer.curPos)
    }
    return res
  }

  public fun expr_(acc: BigDecimal): Node<Expr_> {
    val expr_ = Expr_()
    val res = Node(expr_)
    when (lexer.curTerminal.id) {
      in listOf("PLUS") -> {
        lexer.expect("PLUS")
        res.addChild(lexer.curTerminal)
        lexer.nextToken()

        val t = term(acc).also { res.addChild(it) }.label
        val tmp = context.add(acc, t.res!!)
        val e_ = expr_(tmp).also { res.addChild(it) }.label
        expr_.res = e_.res
      }
      in listOf("END", "RBRACKET") -> {
        res.addChild(Terminal("EPS", ""))
        expr_.res = acc
      }
      else -> throw ParserException("Invalid terminal ${lexer.curTerminal}", lexer.curPos)
    }
    return res
  }

  public fun term(acc: BigDecimal): Node<Term> {
    val term = Term()
    val res = Node(term)
    when (lexer.curTerminal.id) {
      in listOf("NUMBER") -> {
        val number = lexer.curTerminal
        lexer.expect("NUMBER")
        res.addChild(lexer.curTerminal)
        lexer.nextToken()

        term.res = number.text.toBigDecimal()
      }
      in listOf("LBRACKET") -> {
        lexer.expect("LBRACKET")
        res.addChild(lexer.curTerminal)
        lexer.nextToken()

        val e = expr(acc).also { res.addChild(it) }.label
        lexer.expect("RBRACKET")
        res.addChild(lexer.curTerminal)
        lexer.nextToken()

        term.res = e.res
      }
      else -> throw ParserException("Invalid terminal ${lexer.curTerminal}", lexer.curPos)
    }
    return res
  }
}