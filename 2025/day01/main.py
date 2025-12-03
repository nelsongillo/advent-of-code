import sys

def dial(current: int, rotation: int, direction: str) -> tuple[int, int]:
    if direction == 'L':
        intermediate = current - rotation
        clicks = ((current - 1) // 100) - ((intermediate - 1) // 100)
        result = intermediate % 100
    elif direction == 'R':
        intermediate = current + rotation
        clicks = (intermediate // 100) - (current // 100)
        result = intermediate % 100
    else:
        raise Exception(f"Unknown direction: {direction}")

    return result, clicks

def task01(path: str, start: int) -> int:
    zeros = 0
    current = start
    with open(path, "r") as file:
        for line in file:
            current, _ = dial(current, int(line[1:]), line[0])
            if current == 0:
                zeros += 1
        return zeros

                

def task02(path: str, start: int) -> int:
    zeros = 0
    current = start
    with open(path, "r") as file:
        for line in file:
            current, clicks = dial(current, int(line[1:]), line[0])
            zeros += clicks
        return zeros


print(task01(sys.argv[1], 50))
print(task02(sys.argv[1], 50))