import sys

def max_with_idx(li: str) -> tuple[int, int]:
    m = -1
    idx = 0
    for i in range(0, len(li)):
        v = int(li[i])
        if v > m:
            m = v
            idx = i
    return m, idx

def joltage(batteries: str, need: int) -> int:
    result = 0
    last_idx = 0
    for n in range(need):
        rem = batteries[last_idx:len(batteries) - (need - n - 1)]
        v, i = max_with_idx(rem)
        result = result * 10 + v
        last_idx += i + 1

    return result

def task01(path: str) -> int:
    with open(path, "r") as file:
        return sum(map(lambda l: joltage(l.strip(), 2), file.readlines()))
    

def task02(path: str) -> int:
    with open(path, "r") as file:
        return sum(map(lambda l: joltage(l.strip(), 12), file.readlines()))
    


print(task01(sys.argv[1]))
print(task02(sys.argv[1]))