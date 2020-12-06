def anyone(lines):
    pool = set()
    lines = ''.join(lines.split())
    for x in lines:
        pool.add(x)

    return len(pool)

def everyone(lines):
    lines = lines.strip().split("\n")
    pool = set(lines[0]).intersection(*lines)
    return len(pool)



input = "data.txt"

file = open(input, "r")
lines = file.read().split("\n\n")

puzzle01 = map(anyone, lines)
puzzle02 = map(everyone, lines)

print("Puzzle 01: {}".format(sum(puzzle01)))
print("Puzzle 02: {}".format(sum(puzzle02)))
    
