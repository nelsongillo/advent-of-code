def acc_value_pre_loop(instructions):
    acc = 0
    index = 0

    (visited, ins, value) = instructions[index]
    while(not visited):
        # mark visited
        instructions[index] = (True, ins, value)

        # Acc Instruction
        if ins == "acc":
            acc += value
            index += 1
        
        # Jump Instruction
        elif ins == "jmp":
            index += value
        
        # No Instruction
        else:
            index += 1

        # get next instruction
        (visited, ins, value) = instructions[index]
    
    return acc





file = "data.txt"
lines = open(file, "r").readlines()

# (Visited, Instruction, Value)
instructions = []
for line in lines:
    values = line.split()
    val = int(values[1])
    ins = values[0]

    instructions.append((False, ins, val))


puzzle01 = acc_value_pre_loop(instructions)

print("Puzzle 01: {}".format(puzzle01))