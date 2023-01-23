import numpy as np


def grad(fun, x, h=1e-5):
    if not callable(fun):
        raise ValueError("fun should be callable")
    dim = len(x)
    g = np.zeros(dim)
    step = np.zeros(dim)
    for i in range(dim):
        step[i] = h
        g[i] = (fun(x + step) - fun(x - step)) / (2 * h)
        step[i] = 0
    return g


def gen_quadratic_function(n, limits):
    a = np.random.randint(-limits, limits + 1, size=(n, n))
    a0 = np.random.randint(-limits, limits + 1)

    def f(x):
        res = a0
        for i in range(n):
            for j in range(n):
                res += a[i, j] * x[i] * x[j]
        return res

    return f


def condition_number(fun, x):
    return np.abs(grad(fun, x) * x / fun(x))
