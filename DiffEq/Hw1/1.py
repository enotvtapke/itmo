import numpy as np
import matplotlib.pyplot as plt
import math

N = 20
K = 5

def der(y): return y * (2 - y)

X = [(i) / K for i in range(N)] * (N - N // K)
Y = []
for i in range(N - N // K):
    for j in range(N):
        Y.append((i) / K)

length = 100

U = list(map(lambda y: math.sqrt(length / (der(y)**2 + 1)), Y))
V = list(map(lambda y: math.sqrt(length - length / (der(y)**2 + 1)), Y))

fig, ax = plt.subplots()

ax.quiver(X, Y, U, V)

fig.set_figwidth(12)    #  ширина и
fig.set_figheight(12)    #  высота "Figure"

plt.savefig('1.png')
plt.show()