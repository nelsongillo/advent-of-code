import re

input_file = "data.txt"


req_fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
def all_req_fields(passport):
    for key in req_fields:
        if passport.get(key) == None:
            return False

    return True


color_regex = "^#(?:[0-9a-fA-F]{2}){3}$"
pid_regex = "^\d{9}$"
hgt_regex = "^\d{2,3}(cm|in)$"
ecl_vals = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
def valid_fields(passport):
    # birth year
    byr = int(passport.get("byr"))
    if byr in range(1920, 2002) == False:
        return False

    # issue year
    iyr = int(passport.get("iyr"))
    if iyr in range(2010, 2020) == False:
        return False

    # expiration year
    eyr = int(passport.get("eyr"))
    if eyr in range(2020, 2030) == False:
        return False

    # height
    if re.match(hgt_regex, passport.get("hgt")) == None:
       return False

    if passport.get("hgt")[-2:] == "in":
        inch = True
    else:
        inch = False

    hgt = int(passport.get("hgt")[:-2])
    if inch:
        if hgt in range(59, 76) == False:
            return False
    else:
        if hgt in range(150, 193) == False:
            return False

    # hair color
    if re.match(color_regex, passport.get("hcl")) == None:
       return False

    # eye color
    if not (passport.get("ecl") in ecl_vals):
        return False

    # passport id
    if re.match(pid_regex, passport.get("pid")) == None:
       return False

    
    # all values correct
    return True

    


file = open(input_file, "r")
lines = file.read().split("\n\n")

puzzle01 = 0
puzzle02 = 0
for entry in lines:
    passport = dict([])
    fields = entry.split()
    for field in fields:
        (key, val) = field.split(":")

        passport[key] = val

    if all_req_fields(passport):
        puzzle01 += 1

        if valid_fields(passport):
            puzzle02 += 1
    


print("Puzzle 01: {}".format(puzzle01))
print("Puzzle 02: {}".format(puzzle02))

