class DataEntry:
    
    def __init__(self, min, max, character, password):
        self.min = min
        self.max = max
        self.character = character
        self.password = password

    def check_valid_puzzle01(self):
        amout = 0
        for c in self.password:
            if c == self.character:
                amout = amout + 1

        return self.min <= amout <= self.max

    def check_valid_puzzle02(self):
        one = self.password[self.min - 1] == self.character
        two = self.password[self.max - 1] == self.character

        return one != two




input_file = "data.txt"

file = open(input_file, "r")
lines = file.readlines()

entries = []
for line in lines:
    spaces = line.split(" ")
    nums = [int(i) for i in spaces[0].split("-")]
    c = spaces[1][:1]
    entries.append(DataEntry(nums[0], nums[1], c, spaces[2]))



p01_valid = 0
p02_valid = 0
for e in entries:
    if e.check_valid_puzzle01():
        p01_valid = p01_valid + 1
    
    if e.check_valid_puzzle02():
        p02_valid = p02_valid + 1

print("Puzzle 01: {}".format(p01_valid))
print("Puzzle 02: {}".format(p02_valid))