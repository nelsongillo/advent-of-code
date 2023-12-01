import functools
import operator
import sys

numbers = {
    'one': 1,
    'two': 2,
    'three': 3,
    'four': 4,
    'five': 5,
    'six': 6,
    'seven': 7,
    'eight': 8,
    'nine': 9
}


def calibrate01(line: str) -> int:
    digits = []
    for i, c in enumerate(line):
        if c.isdigit():
            digits.append(int(c))

    return 10 * digits[0] + digits[len(digits) - 1]


def calibrate02(line: str) -> int:
    digits = []
    for i, c in enumerate(line):
        if c.isdigit():
            digits.append(int(c))
        else:
            for k, v in numbers.items():
                if line[i:].startswith(k):
                    digits.append(v)

    return 10 * digits[0] + digits[len(digits) - 1]


def task01(path: str) -> int:
    with open(path, "r") as file:
        return functools.reduce(operator.add, map(calibrate01, file.readlines()))


def task02(path: str) -> int:
    with open(path, "r") as file:
        return functools.reduce(operator.add, map(calibrate02, file.readlines()))


print(f'Task01: {task01(sys.argv[1])}')
print(f'Task02: {task02(sys.argv[1])}')
