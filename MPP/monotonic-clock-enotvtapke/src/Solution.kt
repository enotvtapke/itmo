import kotlin.math.max

/**
 * В теле класса решения разрешено использовать только переменные делегированные в класс RegularInt.
 * Нельзя volatile, нельзя другие типы, нельзя блокировки, нельзя лазить в глобальные переменные.
 *
 * @author Stupnikov Aleksander
 */
class Solution : MonotonicClock {
    private var c11 by RegularInt(0)
    private var c12 by RegularInt(0)
    private var c13 by RegularInt(0)

    private var c21 by RegularInt(0)
    private var c22 by RegularInt(0)
    private var c23 by RegularInt(0)

    override fun write(time: Time) {
        c21 = time.d1
        c22 = time.d2
        c23 = time.d3

        c13 = c23
        c12 = c22
        c11 = c21
    }

    override fun read(): Time {
        var d1 = c11
        var d2 = c12
        var d3 = c13
        val r1 = Time(d1, d2, d3)
        d3 = c23
        d2 = c22
        d1 = c21
        val r2 = Time(d1, d2, d3)

        return if (r1 == r2) {
            r1
        } else {
            if (r1.d1 != r2.d1) {
                Time(max(r2.d1, r1.d1), 0, 0)
            } else {
                if (r2.d2 != r1.d2) {
                    Time(r1.d1, max(r2.d2, r1.d2), 0)
                } else {
                    Time(r1.d1, r1.d2, r1.d3)
                }
            }
        }
    }
}