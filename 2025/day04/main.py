import sys
from typing import List, Tuple

def neighbour_idx(grid: List[List], x: int, y: int) -> List[Tuple[int, int]]:
    lenx = len(grid)
    leny = len(grid[0])

    potentials = [
        (x-1, y-1),
        (x-1, y),
        (x-1, y+1),
        (x, y-1),
        (x, y+1),
        (x+1, y-1),
        (x+1, y),
        (x+1, y+1),
    ]

    return list(filter(lambda xy: xy[0] >= 0 and xy[0] < lenx and xy[1] >= 0 and xy[1] < leny, potentials))

def has_paper(grid: List[List[str]], x: int, y: int) -> bool:
    return grid[y][x] == "@"

def accessible_rolls(grid: List[List[str]]) -> List[Tuple[int, int]]:
    rolls = []
    for x in range(len(grid)):
        for y in range(len(grid[x])):
            if not has_paper(grid, x, y):
                continue

            li = list(map(lambda xy: has_paper(grid, xy[0], xy[1]), neighbour_idx(grid, x, y)))
            if li.count(True) < 4:
                rolls.append([x, y])

    return rolls

def remove_rolls(grid: List[List[str]], at: List[Tuple[int, int]]) -> List[List[str]]:
    modified = grid
    for (x, y) in at:
        modified[y] = modified[y][:x] + '.' + modified[y][x + 1:]
    
    return modified

def task01(path: str) -> int:
    with open(path, "r") as file:
        grid = list(map(lambda l: l.strip(), file.readlines()))
        return len(accessible_rolls(grid))

def task02(path: str) -> int:
    with open(path, "r") as file:
        total = 0
        grid = list(map(lambda l: l.strip(), file.readlines()))
        while True:
            to_remove = accessible_rolls(grid)
            total += len(to_remove)
            if len(to_remove) == 0:
                break

            grid = remove_rolls(grid, to_remove)

        return total

print(task01(sys.argv[1]))
print(task02(sys.argv[1]))