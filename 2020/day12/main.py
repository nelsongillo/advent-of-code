FORWARD = 'F'
EAST = 'E'
SOUTH = 'S'
WEST = 'W'
NORTH = 'N'
RIGHT = 'R'
LEFT = 'L'

# Ship start facing East
# Directions: East = 0, South = 90, West = 180, North = 270
def navigate_naive(instructions):
    east = 0
    north = 0
    direction = 0
    for (a, v) in instructions:
        if a == FORWARD:

            if direction == 0:
                east += v
            elif direction == 90:
                north -= v
            elif direction == 180:
                east -= v
            elif direction == 270:
                north += v
        
        elif a == EAST:
            east += v
        elif a == SOUTH:
            north -= v
        elif a == WEST:
            east -= v
        elif a == NORTH:
            north += v
        elif a == LEFT:
            direction = (direction - v) % 360
        elif a == RIGHT:
            direction = (direction + v) % 360

    return (east, north)





file = "data.txt"
# (Action, Value)
instructions = []
for line in open(file, "r").readlines():
    action = line[0]
    value = int(line[1:])
    instructions.append((action, value))

(east01, north01) = navigate_naive(instructions)

puzzle01 = abs(east01) + abs(north01)

print("Puzzle01: {}".format(puzzle01))
