package mpp.dynamicarray

import kotlinx.atomicfu.*

interface DynamicArray<E> {
    /**
     * Returns the element located in the cell [index],
     * or throws [IllegalArgumentException] if [index]
     * exceeds the [size] of this array.
     */
    fun get(index: Int): E

    /**
     * Puts the specified [element] into the cell [index],
     * or throws [IllegalArgumentException] if [index]
     * exceeds the [size] of this array.
     */
    fun put(index: Int, element: E)

    /**
     * Adds the specified [element] to this array
     * increasing its [size].
     */
    fun pushBack(element: E)

    /**
     * Returns the current size of this array,
     * it increases with [pushBack] invocations.
     */
    val size: Int
}

sealed interface Movable<T> {
    val value: T
}
class Moved<T>(override val value: T): Movable<T>
class NotMoved<T>(override val value: T): Movable<T>

class DynamicArrayImpl<E> : DynamicArray<E> {
    private val curCore = atomic(Core<E>(INITIAL_CAPACITY))
    override val size: Int get() = curCore.value.size.value

    override fun get(index: Int): E {
        require(index < size)
        return curCore.value.array[index].value!!.value
    }

    override fun put(index: Int, element: E) {
        require(index < size)
        while (true) {
            val core = curCore.value
            val e = core.array[index].value
            if (e is NotMoved) {
                if (core.array[index].compareAndSet(e, NotMoved(element))) {
                    return
                }
            } else {
                move(core)
            }
        }
    }

    private fun move(curCore: Core<E>) {
        var nextCore = Core<E>(curCore.capacity * 2)
        nextCore.size.value = curCore.size.value
        if (!curCore.next.compareAndSet(null, nextCore)) {
            nextCore = curCore.next.value ?: return
        }
        for (i in 0 until curCore.size.value) {
            var v: Movable<E>?
            do {
                v = curCore.array[i].value
            } while (v is NotMoved<*> && !curCore.array[i].compareAndSet(v, Moved(v.value)))
            v!!
            nextCore.array[i].compareAndSet(null, NotMoved(v.value))
        }
        this.curCore.compareAndSet(curCore, nextCore)
    }

    override fun pushBack(element: E) {
        while(true) {
            val core = curCore.value
            val s = core.size.value
            if (s >= core.capacity) {
                move(core)
            } else {
                if (core.array[s].compareAndSet(null, NotMoved(element))) {
                    core.size.compareAndSet(s, s + 1)
                    return
                } else {
                    core.size.compareAndSet(s, s + 1)
                }
            }
        }
    }
}

private class Core<T>(
    val capacity: Int,
    val next: AtomicRef<Core<T>?> = atomic(null),
) {
    val array = atomicArrayOfNulls<Movable<T>?>(capacity)
    val size = atomic(0)
}

private const val INITIAL_CAPACITY = 1 // DO NOT CHANGE ME