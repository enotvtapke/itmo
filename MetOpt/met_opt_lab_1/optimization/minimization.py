import numpy as np

import optimization as opt


def to_scalar(fun, init_point, direction):
    return lambda x: fun(init_point + x[0] * direction)


def min_in_direction(fun, init_point, direction,
                     method=lambda g, bounds: opt.dichotomy(g, bounds, lambda iter, point: iter > 4)):
    g = to_scalar(fun, init_point, direction)
    bounds = opt.find_unimodal_interval(g, np.array([0]), initial_lr=0.06)
    g_min = method(g, bounds)
    return init_point + direction * g_min


def gradient_descent(init_point, next_point, stopping_criterion):
    x = init_point
    points = [np.array(x)]

    iteration = 0
    while True:
        if stopping_criterion(iteration, x):
            break
        x = next_point(x)
        points.append(x)
        iteration += 1

    return np.array(points)
