package generator.runtime

open class BaseLexer constructor(private val input: CharSequence, private val rules: List<LexerRule>) {
    var curPos = 0
        private set

    var curTerminal: Terminal = END
        private set

    fun nextToken() {
        fun IntRange.length() = last - first + 1

        if (curPos >= input.length) {
            curTerminal = END
            return
        }
        rules.forEach { rule ->
            val match = rule.pattern.matchAt(input, curPos) ?: return@forEach
            curPos += match.range.length()
            if (rule.ignored) {
                nextToken()
            } else {
                Terminal(rule.id, match.value).apply {
                    curTerminal = this
                }
            }
            return
        }
        throw LexerException(
            "Unknown token starting from `${input.subSequence(curPos, minOf(curPos + 10, input.length))}...`", curPos
        )
    }

    fun expect(terminalId: String) {
        if (curTerminal.id != terminalId) {
            throw LexerException(
                "Invalid terminal. Expected terminal with id `$terminalId`, actual `$curTerminal`", curPos
            )
        }
    }
}
