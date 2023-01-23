package mpp.dynamicarray

import org.junit.Test

class MyTest {
    @Test
    fun myTest() {
        val dynamicArray = DynamicArrayImpl<Int>()
        dynamicArray.pushBack(12)
        dynamicArray.pushBack(3)
        println(dynamicArray.get(1))
        dynamicArray.put(0, 5)
        dynamicArray.put(1, 6)
        println(dynamicArray.get(1))
        println(dynamicArray.get(0))

    }
}