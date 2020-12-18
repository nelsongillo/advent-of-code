import copy

FLOOR = '.'
EMPTY = 'L'
OCCUPIED = '#'

# update seating depending on max occupied and neighbour rule
def update_seating(seats, needed_occupied, neighbours):
    updated = copy.deepcopy(seats)

    for row in range(0, len(seats)):
        for col in range(0, len(seats[row])):
            # floor does not change
            if seats[row][col] == FLOOR:
                continue

            empty_neighnours = neighbours(seats, row, col)

            if seats[row][col] == EMPTY:
                if empty_neighnours == 8:
                    updated[row][col] = OCCUPIED

            if seats[row][col] == OCCUPIED:
                if empty_neighnours <= 8 - needed_occupied:
                    updated[row][col] = EMPTY

    return updated

            
directions =    [ (-1, -1), (0, -1), (1, -1)
                , (-1,  0),          (1,  0)
                , (-1,  1), (0,  1), (1,  1)]
# Count empty neighbours next to main seat at (row, col)
def get_empty_direct_neighbours(seats,  row, col):
    empty = 8
    
    row_max = len(seats)
    col_max = len(seats[0])


    for (r, c) in directions:
        x = row + r
        y = col + c

        if x in range(0, row_max) and y in range(0, col_max):
            if seats[x][y] == OCCUPIED:
                empty -= 1

    return empty

# Count empty next possible neighbours
def get_empty_visible_neighbours(seats, row, col):
    empty = 8
    
    row_max = len(seats)
    col_max = len(seats[0])


    for (r, c) in directions:
        x = row + r
        y = col + c

        while x in range(0, row_max) and y in range(0, col_max):
            if seats[x][y] == FLOOR:
                x = x + r
                y = y + c
            
            elif seats[x][y] == OCCUPIED:
                empty -= 1
                break
            else:
                break

    return empty

# update seating until nothing changes
def final_seating(seats, needed_occupied, neighbours):
    x = seats
    y = update_seating(x, needed_occupied, neighbours)

    while x != y:
        x = y
        y = update_seating(x, needed_occupied, neighbours)

    return y
           
# count occupied seats
def count_occupied(seats):
    count = 0
    for row in range(0, len(seats)):
        for col in range(0, len(seats[row])):
            if seats[row][col] == OCCUPIED:
                count += 1

    return count


file = "data.txt"
seats = [list(line.strip())  for line in open(file, "r").readlines()]

puzzle01 = count_occupied(final_seating(seats, 4, get_empty_direct_neighbours))
puzzle02 = count_occupied(final_seating(seats, 5, get_empty_visible_neighbours))
print("Puzzle 01: {}".format(puzzle01))
print("Puzzle 02: {}".format(puzzle02))