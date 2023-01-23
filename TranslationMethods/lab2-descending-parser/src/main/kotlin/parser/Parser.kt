package parser

import parser.NonTerminal.*
import parser.Terminal.*
import java.io.InputStream

class Parser(private val lex: LexicalAnalyzer) {

    constructor(inputStream: InputStream) : this(LexicalAnalyzer(inputStream))

    constructor(string: String) : this(string.byteInputStream())

    init {
        lex.nextToken()
    }

    fun parse(): Node {
        return s()
    }

    private fun s(): Node {
        val res = Node(S)
        when (lex.curTerminal) {
            VAR -> {
                lex.nextToken()
                res.addChild(VAR)

                lex.expect(IDENTIFIER)
                lex.nextToken()
                res.addChild(IDENTIFIER)

                lex.expect(COLON)
                lex.nextToken()
                res.addChild(COLON)

                lex.expect(IDENTIFIER)
                lex.nextToken()
                res.addChild(IDENTIFIER)

                lex.expect(LANGLE)
                lex.nextToken()
                res.addChild(LANGLE)

                res.addChild(f())

                lex.expect(RANGLE)
                lex.nextToken()
                res.addChild(RANGLE)

                res.addChild(t())
            }
            else -> lex.error("Unexpected token ${lex.curTerminal}")
        }
        return res
    }

    private fun t(): Node {
        val res = Node(T)
        when (lex.curTerminal) {
            SEMICOLON -> {
                lex.nextToken()
                res.addChild(SEMICOLON)
            }
            END -> {
                res.addChild(EMPTY)
            }
            else -> lex.error("Unexpected token ${lex.curTerminal}")
        }
        return res
    }

    private fun e(): Node {
        val res = Node(E)
        when (lex.curTerminal) {
            IDENTIFIER -> {
                lex.nextToken()
                res.addChild(IDENTIFIER)

                res.addChild(e_())
            }
            else -> lex.error("Unexpected token ${lex.curTerminal}")
        }
        return res
    }

    private fun e_(): Node {
        val res = Node(E_)
        when (lex.curTerminal) {
            LANGLE -> {
                lex.nextToken()
                res.addChild(LANGLE)

                res.addChild(f())

                lex.expect(RANGLE)
                lex.nextToken()
                res.addChild(RANGLE)
            }
            COMMA, RANGLE -> {
                res.addChild(EMPTY)
            }
            else -> lex.error("Unexpected token ${lex.curTerminal}")
        }
        return res
    }

    private fun f(): Node {
        val res = Node(F)
        when (lex.curTerminal) {
            IDENTIFIER -> {
                res.addChild(e())
                res.addChild(f_())
            }
            else -> lex.error("Unexpected token ${lex.curTerminal}")
        }
        return res
    }

    private fun f_(): Node {
        val res = Node(F_)
        when (lex.curTerminal) {
            COMMA -> {
                lex.nextToken()
                res.addChild(COMMA)

                res.addChild(f())
            }
            RANGLE -> {
                res.addChild(EMPTY)
            }
            else -> lex.error("Unexpected token ${lex.curTerminal}")
        }
        return res
    }
}
