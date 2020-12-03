OPEN = False
TREE = True
ARRIVED = (-1, -1, OPEN)
input_file = "data.txt"

def wrap_x(x, step, max):
    if x + step >= max:
        return x + step - max
    else:
        return x + step

def step(x, y, lines, step_x, step_y):
    if len(lines) <= y + step_y:
        return ARRIVED

    y = y + step_y
    x = wrap_x(x, step_x, len(lines[y]))

    if lines[y][x] == ".":
        return (x, y, OPEN)
    else:
        return (x, y, TREE)


def route(lines, step_x, step_y):
    acc = 0
    ret = step(0, 0, lines, step_x, step_y)
    while ret != ARRIVED:
        (x, y, space) = ret
        if space:
            acc += 1

        ret = step(x, y, lines, step_x, step_y)
    
    return acc

def multiply(array):
    acc = 1
    for element in array:
        acc = acc * element

    return acc




file = open(input_file, "r")
lines = []
for line in file.readlines():
    lines.append(line.strip())



results = []
results.append(route(lines, 1, 1))
results.append(route(lines, 3, 1))
results.append(route(lines, 5, 1))
results.append(route(lines, 7, 1))
results.append(route(lines, 1, 2))

print(results)

puzzle01 = results[1]
puzzle02 = multiply(results)

print("Puzzle 01: {}".format(puzzle01))
print("Puzzle 02: {}".format(puzzle02))