package mpp.stackWithElimination

import kotlinx.atomicfu.atomicArrayOfNulls
import kotlin.random.Random

class TreiberStackWithElimination<E> {
    private val stack = TreiberStack<E>()
    private val eliminationArray = atomicArrayOfNulls<Record<E>?>(ELIMINATION_ARRAY_SIZE)

    private class Record<E>(val value: E)

    /**
     * Adds the specified element [x] to the stack.
     */
    fun push(x: E) {
        var index = Random.nextInt(0, ELIMINATION_ARRAY_SIZE)
        val record = Record(x)

        for (i in 0..3) {
            if (eliminationArray[(index + i) % ELIMINATION_ARRAY_SIZE].compareAndSet(null, record)) {
                index = (index + i) % ELIMINATION_ARRAY_SIZE
                break
            }
            if (i == 3) {
                stack.push(x)
                return
            }
        }

        for (i in 0..19) {
            if (eliminationArray[index].value != record) {
                return
            }
        }

        if (eliminationArray[index].compareAndSet(record, null)) {
            stack.push(x)
        }
    }

    /**
     * Retrieves the first element from the stack
     * and returns it; returns `null` if the stack
     * is empty.
     */
    fun pop(): E? {
        val index = Random.nextInt(0, ELIMINATION_ARRAY_SIZE)

        for (i in 0..3) {
            val him = eliminationArray[(index + i) % ELIMINATION_ARRAY_SIZE].value
            if (him != null) {
                if (eliminationArray[(index + i) % ELIMINATION_ARRAY_SIZE].compareAndSet(him, null)) {
                    return him.value
                }
            }
        }

        return stack.pop()
    }
}

private const val ELIMINATION_ARRAY_SIZE = 2 // DO NOT CHANGE IT