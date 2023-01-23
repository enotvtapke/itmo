package parser

import parser.Terminal.*
import java.io.InputStream
import java.text.ParseException

class LexicalAnalyzer(private val inputStream: InputStream) {
    private var curChar: Int = 0
    private var curPos: Int = 0
    var curTerminal: Terminal = END
        private set

    init {
        nextChar()
    }

    private fun nextChar() {
        curPos++
        curChar = inputStream.read()
    }

    private fun Int.isBlank(): Boolean {
        return this != -1 && (this.toChar()).isWhitespace()
    }

    fun nextToken() {
        while (curChar.isBlank()) {
            nextChar()
        }
        curTerminal = when (curChar) {
            -1 -> END
            ':'.code -> {
                nextChar()
                COLON
            }

            '<'.code -> {
                nextChar()
                LANGLE
            }

            '>'.code -> {
                nextChar()
                RANGLE
            }

            ';'.code -> {
                nextChar()
                SEMICOLON
            }

            ','.code -> {
                nextChar()
                COMMA
            }

            else -> {
                if (!curChar.toChar().isJavaIdentifierStart()) {
                    error("Invalid character `${String(CharArray(curChar))}`")
                }
                val str = StringBuilder(curChar.toChar().toString())
                nextChar()
                while (curChar.toChar().isJavaIdentifierPart()) {
                    str.append(curChar.toChar().toString())
                    nextChar()
                }
                when (str.toString()) {
                    "var" -> VAR
                    else -> {
                        IDENTIFIER
                    }
                }
            }
        }
    }

    fun error(message: String) {
        throw ParseException("Error at pos ${curPos}: $message", curPos)
    }

    fun expect(terminal: Terminal) {
        if (curTerminal != terminal) {
            error("Invalid token. Expected `$terminal`, actual `$curTerminal`")
        }
    }
}