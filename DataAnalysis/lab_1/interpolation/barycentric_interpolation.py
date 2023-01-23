import numpy as np
from numpy import ndarray
from functools import partial


def barycentric_interpolation(xi: ndarray, yi: ndarray):
    n = len(xi)
    if n != len(yi):
        raise ValueError("Sizes of xi and yi should be equal")
    w = np.ones(n, dtype=float)

    for j in range(n):
        for k in range(n):
            if k == j:
                continue
            w[j] *= xi[j] - xi[k]
        w[j] = 1 / w[j]
    return partial(_barycentric_formula, w, xi, yi)


def _barycentric_formula(wi, xi, yi, x: ndarray):
    n = len(x)
    res = np.empty(n)

    for i in range(n):
        f = False
        coefs = np.empty(len(xi))
        for j in range(len(xi)):
            if x[i] == xi[j]:
                res[i] = yi[j]
                f = True
                break
            coefs[j] = wi[j] / (x[i] - xi[j])
        # if np.searchsorted(x[i], xi) != -1:
        #     res[i] = yi[np.searchsorted(x[i], xi)]
        # coefs = wi / np.subtract(x[i], xi)
        if not f:
            res[i] = np.sum(coefs * yi) / np.sum(coefs)
    return res
