def two_values_sum_to(values, sum):
    for x in values:
        y = sum - x
        if y in values and y != x:
            return True

    return False

def first_non_valid(preamble_size, values):
    index = preamble_size

    while(index < len(values)):
        arr = values[index - preamble_size : index]
        if not two_values_sum_to(arr, values[index]):
            return values[index]
        
        index += 1

    return None

def continues_set_sum_to(values, goal):
    for end in range(2, len(values)):
        arr = values[:end]
        index = 0
        while index < len(values):
            if sum(arr) == goal:
                return arr
            index += 1
            arr = values[index : end + index]

    return None

def calc_weakness(set):
    return min(set) + max(set)


file = "data.txt"
values = [int(i) for i in open(file, "r").readlines()]

puzzle01 = first_non_valid(25, values)
puzzle02 = calc_weakness(
    continues_set_sum_to(
        values[0 : values.index(puzzle01)],
        puzzle01
    )
)

print("Puzzle 01: {}".format(puzzle01))
print("Puzzle 02: {}".format(puzzle02))