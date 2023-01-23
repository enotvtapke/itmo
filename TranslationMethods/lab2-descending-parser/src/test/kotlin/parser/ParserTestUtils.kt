package parser

import parser.NonTerminal.*
import parser.Terminal.*

object ParserTestUtils {
    fun Node.nonGenericIdentifier() =
        node(F) {
            node(E) {
                node(IDENTIFIER)
                node(E_) {
                    node(EMPTY)
                }
            }
            node (F_) {
                node(EMPTY)
            }
        }
}