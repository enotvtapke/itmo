import numpy as np

from optimization import utils


def exp_decay(lr, k=0.01):
    return lr * np.exp(-k)


def exp_increase(lr, k=0.01):
    return lr * np.exp(k)


def _wolfe_2(fun, x, p, alpha, grad, grad_x, c2):
    return np.matmul(grad(fun, x + alpha * p), p) >= c2 * np.matmul(grad_x, p)


def _wolfe_1(fun, x, p, alpha, grad_x, c1):
    return fun(x + alpha * p) <= fun(x) + c1 * alpha * np.matmul(grad_x, p)


def wolfe(alpha, fun, x, direction, c1=1e-4, c2=0.9):
    grad_x = utils.grad(fun, x)
    return _wolfe_1(fun, x, direction, alpha, grad_x, c1) and _wolfe_2(fun, x, direction, alpha, utils.grad, grad_x, c2)


def _wolfe_conditions(fun, x, direction, grad=None, c1=1e-4, c2=0.8):
    if grad is None:
        grad = utils.grad
    lr = 1e-2
    alpha = lr
    grad_x = grad(fun, x)
    while not (_wolfe_1(fun, x, direction, alpha, grad_x, c1) and _wolfe_2(fun, x, direction, alpha, grad, grad_x, c2)):
        alpha = exp_increase(alpha)
    return alpha
