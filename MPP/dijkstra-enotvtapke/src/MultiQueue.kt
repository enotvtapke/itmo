package dijkstra

import java.util.PriorityQueue
import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.locks.Lock
import java.util.concurrent.locks.ReentrantLock

fun main() {
    println("asd")
}

class MultiQueue<E>(threads: Int, private val comparator: Comparator<E>) {
    companion object {
        private const val C = 4
    }

    private val queues: List<PriorityQueue<E>>
    private val locks: List<Lock>

    init {
        queues = (0 until C * threads).map { PriorityQueue(comparator) }
        locks = (0 until C * threads).map { ReentrantLock() }
    }

    fun add(e: E) {
        while (true) {
            val index = ThreadLocalRandom.current().nextInt(queues.size)
            val queue = queues[index]
            val lock = locks[index]

            if (!lock.tryLock()) continue
            try {
                queue.add(e)
            } finally {
                lock.unlock()
            }
            return
        }
    }

    fun poll(): E? {
        while (true) {
            val (index1, index2) = distinctRandom(queues.size)
            val (q1, q2) = listOf(queues[index1], queues[index2])
            val (p1, p2) = listOf(q1.peek(), q2.peek())
            if (p1 == null && p2 == null) {
                return null
            }
            val index =
                if (p1 == null) {
                    index2
                } else if (p2 == null) {
                    index1
                } else {
                    if (comparator.compare(p1, p2) == -1) {
                        index1
                    } else {
                        index2
                    }
                }
            val q = queues[index]
            val l = locks[index]
            if (!l.tryLock()) continue
            val task: E?
            try {
                task = q.poll()
            } finally {
                l.unlock()
            }
            return task
        }
    }

    private fun distinctRandom(until: Int): Pair<Int, Int> {
        if (until <= 1) {
            throw IllegalArgumentException("Upper bound for distinct random is too small")
        }
        val random = ThreadLocalRandom.current()
        val i = random.nextInt(until)
        var j = random.nextInt(until)
        while (j == i) {
            j = random.nextInt(until)
        }
        return Pair(i, j)
    }
}