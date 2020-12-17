import operator

def get_diffs(values):
    one = 0
    three = 0
    chain = sorted(values)
    chain.insert(0, 0)
    chain.append(max(values) + 3)
    
    diffs = list(map(operator.sub, chain[1:], chain[:-1]))

    for d in diffs:
        if d == 1:
            one += 1
        elif d == 3:
            three += 1

    return (one, three)


def puzzle01_calc(values):
    (one, three) = get_diffs(values)
    return one * three


file = "data.txt"
values = [int(i) for i in open(file, "r").readlines()]

puzzle01 = puzzle01_calc(values)
print("Puzzle 01: {}".format(puzzle01))