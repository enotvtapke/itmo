import re

import matplotlib.pyplot as plt
plt.style.use('seaborn-whitegrid')

index = "2"
filename = f"mem{index}.bash.log"
with open(filename, 'r') as fp:
    data = fp.read()
processed = list(map(lambda t: t.split('\n'), data.split('\n\n')))
print(*processed, sep='\n')
name = processed[0][0].split()[0]
pid = processed[0][0].split()[1]

body = processed[1:len(processed) - 2]
tail = processed[len(processed) - 1]
crash_size = tail[len(tail) - 2]
err_mes_1 = re.split(r" \[\d+\.\d+] ", tail[0])[0]
err_mes_2 = re.split(r" \[\d+\.\d+] ", tail[0])[1]
# print(err_mes_2)
# print(err_mes_1)
free_mem = list(map(
    lambda m: float(re.search(r'\d+.\d+', re.search(r', .+ free,', m[0]).group(0)).group(0)),
    body))
free_swap = list(map(
    lambda m: float(re.search(r'\d+.\d+', re.search(r', .+ free,', m[1]).group(0)).group(0)),
    body))
vim = list(map(
    lambda m: int(m[3].split(' ')[4]) / 2**10,
    body))
res = list(map(
    lambda m: m[3].split(' ')[5],
    body))
for i in range(len(res)):
    if res[i][-1] == 'g':
        res[i] = float(res[i][0:-1]) * 1024
    elif res[i][-1] == 'm':
        res[i] = float(res[i][0:-1])
    else:
        res[i] = float(res[i]) / 1024
crash_mem = re.search(r'total-vm:\d+kB', err_mes_1).group(0)[9:-2]
fig, ax = plt.subplots()
ax.plot([i for i in range(len(free_mem))], free_mem)
ax.set_title("Free memory")
ax.set_xlabel('time (s)')
ax.set_ylabel('free memory (MB)')
plt.savefig(f'./free_mem{index}.svg')
ax.clear()

ax.plot([i for i in range(len(free_swap))], free_swap)
ax.set_title("Free swap")
ax.set_xlabel('time (s)')
ax.set_ylabel('free swap (MB)')
plt.savefig(f'./free_swap{index}.svg')
ax.clear()

ax.plot([i for i in range(len(vim))], vim)
ax.set_title("Virtual memory usage")
ax.set_xlabel('time (s)')
ax.set_ylabel('virtual memory (MB)')
plt.savefig(f'./vim_usage{index}.svg')
ax.clear()

ax.plot([i for i in range(len(res))], res)
ax.set_title("Res memory usage")
ax.set_xlabel('time (s)')
ax.set_ylabel('res memory (MB)')
plt.savefig(f'./res{index}.svg')
ax.clear()

top = list(map(
    lambda m: list(map(int, m[2].split(' '))),
    body))

print(top)
# print(free_mem)
# print(free_swap)
# print(vim)
# print(res)