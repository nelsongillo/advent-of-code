import math

def binary_search(min, max, seat):
    if len(seat) == 1:
        if (seat == "F") or (seat == "L"):
            return min
        else:
            return max

    else:
        mid = math.ceil((max + min) / 2)

        if (seat[0] == "F") or (seat[0] == "L"):
            return binary_search(min, mid - 1, seat[1:])
        else:
            return binary_search(mid, max, seat[1:])

def calc_seat(row, col):
    return row * 8 + col

        
input = "data.txt"

file = open(input, "r")

seats = []
for line in file.readlines():
    line = line.strip()

    fb = line[0:7]
    rl = line[7:]

    row = binary_search(0, 127, fb)
    col = binary_search(0, 7, rl)

    seats.append(calc_seat(row, col))

missing = -1
for x in range(min(seats), max(seats)):
    if x not in seats:
        missing = x
        break

print("Puzzle 01: {}".format(max(seats)))
print("Puzzle 02: {}".format(missing))