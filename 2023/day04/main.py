import sys
from collections import defaultdict
from typing import List


def points(match: int) -> int:
    return pow(2, match - 1) if match > 0 else 0


def matches(line: str) -> int:
    line = line.split(':')[1]
    numbers = line.split('|')
    winning = set(map(lambda n: int(n.strip()), numbers[0].split()))
    having = set(map(lambda n: int(n.strip()), numbers[1].split()))

    return len(winning & having)


def counting(card_matches: List[int]) -> int:
    count = 0
    cards = defaultdict(lambda: 1)
    for i, match in enumerate(card_matches):
        count += cards[i]
        for n in range(match):
            cards[i + 1 + n] += cards[i]

    return count


def task01(path: str) -> int:
    with open(path, 'r') as file:
        return sum(map(lambda s: points(matches(s.removesuffix('\n'))), file.readlines()))


def task02(path: str) -> int:
    with open(path, 'r') as file:
        return counting(list(map(lambda s: matches(s.removesuffix('\n')), file.readlines())))


print(f'Task01: {task01(sys.argv[1])}')
print(f'Task02: {task02(sys.argv[1])}')
