from sys import argv
from math import lcm
from re import match
from typing import Tuple, Dict, Callable


def parse_nodes(lines: str) -> Dict[str, Tuple[str, str]]:
    nodes = {}
    for line in lines.split('\n'):
        node = match('(\w{3}) = \((\w{3}), (\w{3})\)', line)
        nodes[node.group(1)] = (node.group(2), node.group(3))

    return nodes


def navigate(instructions: str, nodes: Dict[str, Tuple[str, str]], start: str, stop: Callable[[str], bool], ) -> int:
    steps = 0
    idx = 0
    current = start
    while not stop(current):
        if instructions[idx] == 'R':
            current = nodes[current][1]
        else:
            current = nodes[current][0]

        idx = (idx + 1) % len(instructions)
        steps += 1

    return steps


def navigate_ghost(instructions: str, nodes: Dict[str, Tuple[str, str]]) -> int:
    current = set(filter(lambda n: n[2] == 'A', nodes.keys()))
    return lcm(*map(lambda c: navigate(instructions, nodes, c, lambda s: s[2] == 'Z'), current))


def task01(path: str) -> int:
    with open(path, 'r') as file:
        parts = file.read().split('\n\n')
        return navigate(parts[0], parse_nodes(parts[1]), 'AAA', lambda s: s == 'ZZZ')


def task02(path: str) -> int:
    with open(path, 'r') as file:
        parts = file.read().split('\n\n')
        return navigate_ghost(parts[0], parse_nodes(parts[1]))


print(f'Task01: {task01(argv[1])}')
print(f'Task02: {task02(argv[1])}')
