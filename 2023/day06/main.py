import sys
from math import prod, sqrt, floor, ceil
from typing import List, Tuple


def beat(time, distance) -> int:
    low = int(floor((time - sqrt(time ** 2 - 4 * distance)) / 2))
    high = int(ceil((time + sqrt(time ** 2 - 4 * distance)) / 2))
    return high - low - 1


def parse(lines: List[str]) -> List[Tuple[int, int]]:
    time = list(map(int, lines[0].removesuffix('\n').split(':')[1].strip().split()))
    distance = list(map(int, lines[1].removesuffix('\n').split(':')[1].strip().split()))
    return list(zip(time, distance))


def parse_kerning(lines: List[str]) -> Tuple[int, int]:
    time = int(lines[0].removesuffix('\n').split(':')[1].replace(' ', ''))
    distance = int(lines[1].removesuffix('\n').split(':')[1].replace(' ', ''))
    return time, distance


def task01(path: str) -> int:
    with open(path, 'r') as file:
        return prod([beat(t, d) for t, d in parse(file.readlines())])


def task02(path: str) -> int:
    with open(path, 'r') as file:
        t, d = parse_kerning(file.readlines())
        return beat(t, d)


print(f'Task01: {task01(sys.argv[1])}')
print(f'Task02: {task02(sys.argv[1])}')
