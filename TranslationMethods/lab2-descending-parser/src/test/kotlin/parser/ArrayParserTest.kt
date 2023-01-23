package parser

import org.assertj.core.api.Assertions.assertThat
import org.assertj.core.api.Assertions.assertThatThrownBy
import org.junit.jupiter.api.Test
import parser.NonTerminal.*
import parser.Terminal.*
import parser.ParserTestUtils.nonGenericIdentifier
import java.text.ParseException

internal class ArrayParserTest {

    @Test
    fun `should parse in case of no semicolon`() {
        val actual = Parser("var ar:Array<Int>").parse()

        val expected = Node(S) {
            nodes(VAR, IDENTIFIER, COLON, IDENTIFIER, LANGLE)
            nonGenericIdentifier()
            node(RANGLE)
            node(T) {
                node(EMPTY)
            }
        }

        assertThat(actual).isEqualTo(expected)
    }

    @Test
    fun `should parse in case of semicolon`() {
        val actual = Parser("var ar:Array<Int>;").parse()

        val expected = Node(S) {
            nodes(VAR, IDENTIFIER, COLON, IDENTIFIER, LANGLE)
            nonGenericIdentifier()
            node(RANGLE)
            node(T) {
                node(SEMICOLON)
            }
        }

        assertThat(actual).isEqualTo(expected)
    }

    @Test
    fun `should parse in case of random whitespaces`() {
        val actual = Parser("  var     ar    :    Array <   Int   >  ;  ").parse()

        val expected = Node(S) {
            nodes(VAR, IDENTIFIER, COLON, IDENTIFIER, LANGLE)
            nonGenericIdentifier()
            node(RANGLE)
            node(T) {
                node(SEMICOLON)
            }
        }

        assertThat(actual).isEqualTo(expected)
    }

    @Test
    fun `should parse in case of strange identifiers`() {
        val actual = Parser("  var     _\$f0    :    Array <   _94\$5as   >  ;  ").parse()

        val expected = Node(S) {
            nodes(VAR, IDENTIFIER, COLON, IDENTIFIER, LANGLE)
            nonGenericIdentifier()
            node(RANGLE)
            node(T) {
                node(SEMICOLON)
            }
        }

        assertThat(actual).isEqualTo(expected)
    }

    @Test
    fun `should throw parse exception in case of invalid VAR token`() {
        assertThatThrownBy {
            Parser("val a: Array<Int>").parse()
        }.isInstanceOf(ParseException::class.java)
    }

    @Test
    fun `should throw parse exception in case of no IDENTIFIER token after var`() {
        assertThatThrownBy {
            Parser("var: Array<Int>").parse()
        }.isInstanceOf(ParseException::class.java)
    }

    @Test
    fun `should throw parse exception in case of invalid IDENTIFIER token after var`() {
        assertThatThrownBy {
            Parser("var 12: Array<Int>").parse()
        }.isInstanceOf(ParseException::class.java)
    }

    @Test
    fun `should throw parse exception in case of no COLON`() {
        assertThatThrownBy {
            Parser("var a Array<Int>").parse()
        }.isInstanceOf(ParseException::class.java)
    }

    @Test
    fun `should throw parse exception in case of invalid angle brackets`() {
        assertThatThrownBy {
            Parser("var a: Array[Int]").parse()
        }.isInstanceOf(ParseException::class.java)
    }

    @Test
    fun `should throw parse exception in case of no IDENTIFIER for array type`() {
        assertThatThrownBy {
            Parser("var a: Array<>").parse()
        }.isInstanceOf(ParseException::class.java)
    }

    @Test
    fun `should throw parse exception in case of invalid IDENTIFIER for array type`() {
        assertThatThrownBy {
            Parser("var a: Array<a@>").parse()
        }.isInstanceOf(ParseException::class.java)
    }

    @Test
    fun `should throw parse exception in case of no RANGLE`() {
        assertThatThrownBy {
            Parser("var a: Array<Int;").parse()
        }.isInstanceOf(ParseException::class.java)
    }
}