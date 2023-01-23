package parser

data class Node(val label: Token, val children: MutableList<Node>, private val id: Long) {
    constructor(node: Token) : this(node, mutableListOf(), nextId())
    constructor(node: Token, block: Node.() -> Unit) : this(node) {
        this.block()
    }

    fun node(nodeLabel: Token, block: Node.() -> Unit) {
        Node(nodeLabel).also {
            addChild(it)
            it.block()
        }
    }

    fun node(nodeLabel: Token) {
        node(nodeLabel) {}
    }

    fun nodes(vararg nodeLabels: Token) {
        nodeLabels.forEach {
            node(it)
        }
    }

    fun addChild(child: Node) {
        children.add(child)
    }

    fun addChild(child: Token) {
        children.add(Node(child))
    }

    private fun toStringInternal(): String =
        buildString {
            append("$id [label=\"$label\"]")
            appendLine()
            val childrenIds = children.map(Node::id).joinToString(separator = " ")
            append("$id -- {$childrenIds}")
            appendLine()
            children.forEach { append(it.toStringInternal()) }
        }

    override fun toString(): String = buildString {
        append("graph ParseTree {")
        appendLine()
        append(toStringInternal().trimEnd().prependIndent("  "))
        appendLine()
        append("}")
        appendLine()
    }

    override fun equals(other: Any?): Boolean {
        if (other !is Node) return false
        return this.label == other.label && this.children == other.children
    }

    override fun hashCode(): Int {
        return id.hashCode()
    }

    companion object {
        private var globalId: Long = 0
        private fun nextId() = globalId++
    }
}