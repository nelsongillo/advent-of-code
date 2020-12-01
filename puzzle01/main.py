# calculate solution
def solution(values):
    for x in values:
        for y in values:
            if x + y == 2020:
                print(x * y)
                return



input_file = "data.txt"

file = open(input_file, "r")
lines = file.readlines()

values = [int(i) for i in lines]

solution(values)