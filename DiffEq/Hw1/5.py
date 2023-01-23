import numpy as np
import matplotlib.pyplot as plt
import math

def func(x):
    return 2 * np.exp(2 * x) / (np.exp(2 * x) + 3)

def func1(x, C = 1):
    return 2 * math.exp(2 * x) / (math.exp(2 * x) - C)

def der(y): 
    return y * (2 - y)

fig, ax = plt.subplots()


fig.set_figwidth(12)    #  ширина и
fig.set_figheight(12)    #  высота "Figure"


Xs = [i / 100 for i in range(0, 301)]
Ys = list(map(lambda x: func1(x, -3), Xs))
plt.plot(Xs, Ys)


H = 0.5
X = np.array([0])
while X[len(X) - 1] < 3:
    X = np.append(X, X[len(X) - 1] + H)

Y = np.array([0.5])

for i in range(len(X) - 1):
    Y = np.append(Y, Y[-1] + der(Y[-1]) * H)

print("H:", H)
print(Y[-1] - Ys[-1])
plt.plot(X, Y)

H = 0.1
X = np.array([0])
while X[len(X) - 1] < 3:
    X = np.append(X, X[len(X) - 1] + H)

Y = np.array([0.5])

for i in range(len(X) - 1):
    Y = np.append(Y, Y[-1] + der(Y[-1]) * H)

print("H:", H)
print(Y[-1] - Ys[-1])
plt.plot(X, Y)

plt.savefig('5.png')
plt.show()