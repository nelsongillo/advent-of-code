import sys
from collections import Counter
from copy import copy
from functools import cmp_to_key
from typing import Tuple, List

picture = {
    'A': 14,
    'K': 13,
    'Q': 12,
    'J': 11,
    'T': 10
}

picture_joker = copy(picture)
picture_joker['J'] = 1

is_joker = False


def parse(line: str) -> Tuple[str, int]:
    parts = line.split()
    return parts[0], int(parts[1])


def strength(hand: str) -> int:
    occ = Counter(hand)

    if len(occ) == 1:
        return 7  # 5 of a kind
    if len(occ) == 2:
        if occ[hand[0]] == 1 or occ[hand[0]] == 4:
            return 6  # 4 of a kind
        else:
            return 5  # Full House
    if len(occ) == 3:
        if (occ[hand[0]] == 1 or occ[hand[0]] == 3) and occ[hand[1]] != 2:
            return 4  # 3 of a kind
        else:
            return 3  # Two Pairs
    if len(occ) == 4:
        return 2  # One Pair
    else:
        return 1  # Highest Card


def card_value(c: str) -> int:
    global is_joker
    if is_joker:
        return int(c) if c.isdigit() else picture_joker[c]
    return int(c) if c.isdigit() else picture[c]


def sort_hands(a: Tuple[str, int, int], b: Tuple[str, int, int]) -> int:
    if a[0] == b[0]:
        return 0

    if a[2] != b[2]:
        return a[2] - b[2]

    for i in range(len(a[0])):
        if a[0][i] == b[0][i]:
            continue
        else:
            return card_value(a[0][i]) - card_value(b[0][i])


def transform(hand: str) -> str:
    if 'J' not in hand.upper():
        return hand

    if hand == 'JJJJJ':
        return 'AAAAA'

    occ = Counter(hand.replace('J', ''))
    return hand.replace('J', occ.most_common(1)[0][0])


def solve(lines: List[str], joker: bool = False) -> int:
    global is_joker
    is_joker = joker
    hands = []
    for line in lines:
        h, b = parse(line.strip('\n'))
        s = strength(h)
        if is_joker:
            s = strength(transform(h))
        hands.append((h, b, s))
    hands = sorted(hands, key=cmp_to_key(sort_hands))

    result = 0
    for rank, hand in enumerate(hands):
        result += hand[1] * (rank + 1)

    return result


def task01(path: str) -> int:
    with open(path, 'r') as file:
        return solve(file.readlines())


def task02(path: str) -> int:
    with open(path, 'r') as file:
        return solve(file.readlines(), joker=True)


print(f'Task01: {task01(sys.argv[1])}')
print(f'Task02: {task02(sys.argv[1])}')
