import java.util.concurrent.atomic.*

class Solution(private val env: Environment) : Lock<Solution.Node> {
    private val tail: AtomicReference<Node?> = AtomicReference(null)

    override fun lock(): Node {
        val my = Node()
        val prev = tail.getAndSet(my)
        return if (prev == null) {
            my
        } else {
            prev.next.value = my
            while (my.locked.value) env.park()
            my
        }
    }

    override fun unlock(node: Node) {
        if (tail.compareAndSet(node, null)) return

        var next: Node?
        do {
            next = node.next.value!!
        } while (next == null)
        next.locked.value = false
        env.unpark(next.thread)
    }

    class Node {
        val thread: Thread = Thread.currentThread()

        val next: AtomicReference<Node?> = AtomicReference(null)
        val locked: AtomicReference<Boolean> = AtomicReference(true)
    }
}