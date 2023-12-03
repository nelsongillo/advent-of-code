from typing import List, Set, Tuple
import sys

period = '.'
gear = '*'


def all_neighbours(x, y) -> List[Tuple[int, int]]:
    return [
        (x - 1, y),  # W
        (x, y - 1),  # N
        (x + 1, y),  # E
        (x, y + 1),  # S

        (x - 1, y - 1),  # NW
        (x + 1, y - 1),  # NE
        (x + 1, y + 1),  # SE
        (x - 1, y + 1),  # SW
    ]


def extract_number(line: str, start: int) -> Tuple[int, int]:
    buf = ''
    length = 0
    for i, c in enumerate(line[start:]):
        if c.isdigit():
            buf += c
            length += 1
        else:
            break

    return int(buf), length


def has_adjacent_symbol(engine: List[str], x: int, y: int, length: int) -> bool:
    potential = []
    for idx in range(x, x + length):
        potential += all_neighbours(idx, y)

    for (xx, yy) in potential:
        if 0 <= yy < len(engine) and 0 <= xx < len(engine[yy]) and \
                not engine[yy][xx].isdigit() and engine[yy][xx] != period:
            return True

    return False


def adjacent_gears(engine: List[str], x: int, y: int, length: int) -> Set[Tuple[int, int]]:
    potential = []
    gears = set()
    for idx in range(x, x + length):
        potential += all_neighbours(idx, y)

    for (xx, yy) in potential:
        if 0 <= yy < len(engine) and 0 <= xx < len(engine[yy]) and \
                not engine[yy][xx].isdigit() and engine[yy][xx] == gear:
            gears.add((xx, yy))

    return gears


def find_numbers(engine: List[str]) -> [int]:
    numbers = []
    for y, line in enumerate(engine):
        x = 0
        while x < len(line):
            if line[x].isdigit():
                num, length = extract_number(line, x)
                if has_adjacent_symbol(engine, x, y, length):
                    numbers.append(num)
                x += length

            x += 1

    return numbers


def find_gears(engine: List[str]) -> int:
    gears = dict()
    for y, line in enumerate(engine):
        x = 0
        while x < len(line):
            if line[x].isdigit():
                num, length = extract_number(line, x)
                gs = adjacent_gears(engine, x, y, length)
                for g in gs:
                    if gears.get(g) is None:
                        gears[g] = [num]
                    else:
                        gears[g].append(num)
                x += length

            x += 1

    total_ration = 0
    for rations in gears.values():
        if len(rations) == 2:
            total_ration += rations[0] * rations[1]

    return total_ration


def task01(path: str) -> int:
    with open(path, "r") as file:
        return sum(find_numbers(list(map(lambda s: s.removesuffix('\n'), file.readlines()))))


def task02(path: str) -> int:
    with open(path, "r") as file:
        return find_gears(list(map(lambda s: s.removesuffix('\n'), file.readlines())))


print(f'Task01: {task01(sys.argv[1])}')
print(f'Task02: {task02(sys.argv[1])}')
