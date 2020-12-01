# calculate solution
def solution_puzzle01(values):
    for x in values:
        for y in values:
            if x + y == 2020:
                print("Puzzle 01: ", x * y)
                return


def solution_puzzle02(values):
    for x in values:
        for y in values:
            if x + y >= 2020:
                continue

            for z in values:
                if x + y + z == 2020:
                    print("Puzzle 02: ", x * y * z)
                    return


input_file = "data.txt"

file = open(input_file, "r")
lines = file.readlines()

values = [int(i) for i in lines]

solution_puzzle01(values)
solution_puzzle02(values)