import numpy as np
import matplotlib.pyplot as plt
import time
plt.style.use('seaborn-whitegrid')


def f(x):
    return x[0] * x[0] * x[1] * x[1] * np.log(x[0] * x[0] + x[1] * x[1])


def dy(x, y):
    return 2 * x * x * y * np.log(y * y + x * x) + 2 * y ** 3 * x * x / (y * y + x * x)


def dx(x, y):
    return 2 * y * y * x * np.log(x * x + y * y) + 2 * x ** 3 * y * y / (x * x + y * y)


def stop_eps(prev, cur, eps):
    if np.linalg.norm(cur - prev) < eps:
        return True
    else:
        return False


def stop_delta(prev, cur, eps):
    if np.abs(f(cur) - f(prev)) < eps:
        return True
    else:
        return False


def k(a):
    return a * 1000/1005


def calc(start=np.array([10, 10], dtype=float), eps=0.001):
    start_time = time.time()
    cur = start
    res = [cur]
    a = 0.1
    i = 0
    while True:
        i += 1
        prev = cur
        grad = np.array([dx(*cur), dy(*cur)], dtype=float)
        a = k(a)
        cur = cur - 1 / np.linalg.norm(grad) * a * grad
        res.append(cur)
        if stop_delta(prev, cur, eps):
            break
    return [res, i, time.time() - start_time]


start = np.array([10, 10], dtype=float)
eps = 0.0000000001
res, number_iterations, calc_time = calc(start, eps)
x = [e[0] for e in res]
y = [e[1] for e in res]
fig, ax = plt.subplots()
ax.scatter(x, y, s=0.5)
ax.scatter(x, y, s=0.5)
plt.savefig(f'./1.png', dpi=300)
print(f'Eps (||(∆x_k,∆y_k )|| < eps): {eps}')
print(f'Number of iterations: {number_iterations}')
print(f'Calculated extremum point: ({res[len(res) - 1][0]}, {res[len(res) - 1][1]})')
print(f'f({res[len(res) - 1][0]}, {res[len(res) - 1][1]}): {f(res[len(res) - 1])}')
print(f'Time: {calc_time} seconds')
if start[0] == 10 and start[1] == 10:
    print(f'Precise extremum point: ({np.e**(-1/4)/2**(1/2)}, {np.e**(-1/4)/2**(1/2)})')
print(f'Starting point: ({start[0]}, {start[1]})')
