package generator.runtime

abstract class NonTerminal: Token {
    override fun toString(): String {
        return this::class.simpleName!!
    }
}
