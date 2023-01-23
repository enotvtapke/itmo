import numpy as np
import matplotlib.pyplot as plt
import math

N = 20
K = 5

def func(x, C = 1):
    return 2 * math.exp(2 * x) / (math.exp(2 * x) - C)

def der(y): return y * (2 - y)

X = [(i) / K for i in range(N)] * (N - N // K)
Y = []
for i in range(N - N // K):
    for j in range(N):
        Y.append((i) / K)

length = 100

U = list(map(lambda y: math.sqrt(length / (der(y)**2 + 1)), Y))
V = list(map(lambda y: der(y) / abs(y + 0.01) * math.sqrt(length - length / (der(y)**2 + 1)), Y))

fig, ax = plt.subplots()

ax.quiver(X, Y, U, V)

fig.set_figwidth(12)    #  ширина и
fig.set_figheight(12)    #  высота "Figure"
Xs = [i / 100 for i in range(0, 400)]

for j in range(-12, 0, 4):
    plt.plot(Xs, list(map(lambda x: func(x, j), Xs)))

plt.plot(Xs, list(map(lambda x: func(x, 0), Xs)))
Xs = [i / 100 for i in range(125, 400)]
for j in range(4, 5, 2):
    plt.plot(Xs, list(map(lambda x: func(x, j), Xs)))
plt.savefig('3.png')
plt.show()