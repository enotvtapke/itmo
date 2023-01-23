package generator.runtime

data class Terminal(val id: String, val text: String): Token {
    override fun show(): String {
        return "$id($text)"
    }
}

val END = Terminal("END", "")
val EPS = Terminal("EPS", "")
