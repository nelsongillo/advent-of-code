import functools
import operator
import sys
from typing import Dict, List

red = 12
green = 13
blue = 14


def cubes(line: str) -> (int, [(Dict[str, int])]):
    rgb = []

    data = line.split(':')
    gid = int(data[0].removeprefix('Game '))

    for subset in data[1].split(';'):
        for pull in subset.split(','):
            colors = {'red': 0, 'green': 0, 'blue': 0}
            color = pull.strip().split(' ')
            colors[color[1]] += int(color[0])
            rgb.append(colors)

    return gid, rgb


def possible(r: int, g: int, b: int) -> bool:
    return r <= red and g <= green and b <= blue


def game(line: str) -> int:
    gid, colors = cubes(line)
    return gid if all(map(lambda color: possible(color['red'], color['green'], color['blue']), colors)) else 0


def fewest(key: str, values: List[Dict[str, int]]) -> int:
    return max(map(lambda v: v[key], values))


def power(line: str) -> int:
    gid, colors = cubes(line)
    return fewest('red', colors) * fewest('green', colors) * fewest('blue', colors)


def task01(path: str) -> int:
    with open(path, "r") as file:
        return functools.reduce(operator.add, map(game, file.readlines()))


def task02(path: str) -> int:
    with open(path, "r") as file:
        return functools.reduce(operator.add, map(power, file.readlines()))


print(f'Task01: {task01(sys.argv[1])}')
print(f'Task02: {task02(sys.argv[1])}')
