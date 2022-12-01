# calculate solution
def two_values_sum_to(values, sum):
    for x in values:
        y = sum - x
        if y in values:
            return (x, y)

    return None


def three_values_sum_to(values, sum):
    for x in values:
        remainder = sum - x
        ret = two_values_sum_to(values, remainder)
        if ret != None:
            (y, z) = ret
            return (x, y, z)
    
    return None



def solution_puzzle01(values):
    ret = two_values_sum_to(values, 2020)
    if ret != None:
        (x, y) = ret
        print("Puzzle 01: {} * {} = {}".format(x, y, x*y))
    else:
        print("Puzzle 01: No values found")


def solution_puzzle02(values):
    ret = three_values_sum_to(values, 2020)
    if ret != None:
        (x, y, z) = ret
        print("Puzzle 01: {} * {} * {} = {}".format(x, y, z, x*y*z))
    else:
        print("Puzzle: No values found")


input_file = "data.txt"

file = open(input_file, "r")
lines = file.readlines()

values = sorted([int(i) for i in lines])

solution_puzzle01(values)
solution_puzzle02(values)