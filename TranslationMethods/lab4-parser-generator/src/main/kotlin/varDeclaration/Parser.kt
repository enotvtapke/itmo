package varDeclaration

import generator.runtime.Node
import generator.runtime.ParserException
import generator.runtime.Terminal
import kotlin.String

data class Example(val `data`: String)

public class Parser(
  input: String,
) {
  private val lexer: Lexer = Lexer(input)
  init {
    lexer.nextToken()
  }

  public fun varDeclaration(): Node<VarDeclaration> {
    val varDeclaration = VarDeclaration()
    val res = Node(varDeclaration)
    when (lexer.curTerminal.id) {
      in listOf("VAR") -> {
        lexer.expect("VAR")
        res.addChild(lexer.curTerminal)
        lexer.nextToken()

        lexer.expect("VAR_ID")
        res.addChild(lexer.curTerminal)
        lexer.nextToken()

        lexer.expect("COLON")
        res.addChild(lexer.curTerminal)
        lexer.nextToken()

        res.addChild(genericType())
        res.addChild(endline())
      }
      else -> throw ParserException("Invalid terminal ${lexer.curTerminal}", lexer.curPos)
    }
    return res
  }

  public fun endline(): Node<Endline> {
    val endline = Endline()
    val res = Node(endline)
    when (lexer.curTerminal.id) {
      in listOf("SEMICOLON") -> {
        lexer.expect("SEMICOLON")
        res.addChild(lexer.curTerminal)
        lexer.nextToken()

      }
      in listOf("END") -> {
        res.addChild(Terminal("EPS", ""))
      }
      else -> throw ParserException("Invalid terminal ${lexer.curTerminal}", lexer.curPos)
    }
    return res
  }

  public fun genericType(): Node<GenericType> {
    val genericType = GenericType()
    val res = Node(genericType)
    when (lexer.curTerminal.id) {
      in listOf("TYPE_ID") -> {
        lexer.expect("TYPE_ID")
        res.addChild(lexer.curTerminal)
        lexer.nextToken()

        res.addChild(genericParametersList())
      }
      else -> throw ParserException("Invalid terminal ${lexer.curTerminal}", lexer.curPos)
    }
    return res
  }

  public fun genericParametersList(): Node<GenericParametersList> {
    val genericParametersList = GenericParametersList()
    val res = Node(genericParametersList)
    when (lexer.curTerminal.id) {
      in listOf("LANGLE") -> {
        lexer.expect("LANGLE")
        res.addChild(lexer.curTerminal)
        lexer.nextToken()

        res.addChild(genericParameters())
        lexer.expect("RANGLE")
        res.addChild(lexer.curTerminal)
        lexer.nextToken()

      }
      in listOf("SEMICOLON", "END", "COMMA", "RANGLE") -> {
        res.addChild(Terminal("EPS", ""))
      }
      else -> throw ParserException("Invalid terminal ${lexer.curTerminal}", lexer.curPos)
    }
    return res
  }

  public fun genericParameters(): Node<GenericParameters> {
    val genericParameters = GenericParameters()
    val res = Node(genericParameters)
    when (lexer.curTerminal.id) {
      in listOf("TYPE_ID") -> {
        res.addChild(genericType())
        res.addChild(genericParametersNext())
      }
      else -> throw ParserException("Invalid terminal ${lexer.curTerminal}", lexer.curPos)
    }
    return res
  }

  public fun genericParametersNext(): Node<GenericParametersNext> {
    val genericParametersNext = GenericParametersNext()
    val res = Node(genericParametersNext)
    when (lexer.curTerminal.id) {
      in listOf("COMMA") -> {
        lexer.expect("COMMA")
        res.addChild(lexer.curTerminal)
        lexer.nextToken()

        res.addChild(genericParameters())
      }
      in listOf("RANGLE") -> {
        res.addChild(Terminal("EPS", ""))
      }
      else -> throw ParserException("Invalid terminal ${lexer.curTerminal}", lexer.curPos)
    }
    return res
  }
}