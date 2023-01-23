a = 1
b = 2
c = int(input())
print(a + c * b)
b = 10
d = a * b
print(d // c - b)
if a == b:
    print(b)
elif b == c:
    print(c)
else:
    print(a)

j = 5
while j < 10:
    print(j)
    print(j * 2)
    c = int(input())
    if j > 5:
        print(j)
    j = j + 2
print(22)

a = True
b = False
d = a and b or a
print(d)
