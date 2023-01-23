package parser

import org.assertj.core.api.Assertions.assertThat
import org.junit.jupiter.api.Test
import parser.NonTerminal.*
import parser.ParserTestUtils.nonGenericIdentifier
import parser.Terminal.*

internal class GenericsParserTest {

    @Test
    fun `should parse in case of array of generics`() {
        val actual = Parser("var ar:Array<Mock<Int>>").parse()

        val expected = Node(S) {
            nodes(VAR, IDENTIFIER, COLON, IDENTIFIER, LANGLE)
            node(F) {
                node(E) {
                    node(IDENTIFIER)
                    node(E_) {
                        node(LANGLE)
                        nonGenericIdentifier()
                        node(RANGLE)
                    }
                }
                node(F_) {
                    node(EMPTY)
                }
            }
            node(RANGLE)
            node(T) {
                node(EMPTY)
            }
        }

        assertThat(actual).isEqualTo(expected)
    }

    @Test
    fun `should parse in case of array of generics of multiple parameters`() {
        val actual = Parser("var ar:Map<Int,String>").parse()

        val expected = Node(S) {
            nodes(VAR, IDENTIFIER, COLON, IDENTIFIER, LANGLE)
            node(F) {
                node(E) {
                    node(IDENTIFIER)
                    node(E_) {
                        node(EMPTY)
                    }
                }
                node(F_) {
                    node(COMMA)
                    nonGenericIdentifier()
                }
            }
            node(RANGLE)
            node(T) {
                node(EMPTY)
            }
        }

        assertThat(actual).isEqualTo(expected)
    }
}