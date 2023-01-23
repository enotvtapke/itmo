#include "pi.h"

#include "random_gen.h"

double calculate_pi(unsigned long runs)
{
    if (runs == 0) {
        return -1;
    }
    unsigned long k = 0;
    for (unsigned long i = 0; i < runs; i++) {
        double x = get_random_number();
        double y = get_random_number();
        if (x * x + y * y <= 1) {
            k++;
        }
    }
    return static_cast<double>(k) / runs * 4;
}
