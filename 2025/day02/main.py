import sys
import re

twice = re.compile("^(.+?)\\1$")
twice_or_more = re.compile("^(.+?)\\1+$")

def task01(path: str) -> int:
    invalids = 0
    with open(path, "r") as file:
        for pair in file.readline().split(","):
            start = int(pair.split("-")[0])
            stop = int(pair.split("-")[1])
            for id in range(start, stop+1):
                if twice.match(str(id)):
                    invalids += id

    return invalids

def task02(path: str) -> int:
    invalids = 0
    with open(path, "r") as file:
        for pair in file.readline().split(","):
            start = int(pair.split("-")[0])
            stop = int(pair.split("-")[1])
            for id in range(start, stop+1):
                if twice_or_more.match(str(id)):
                    invalids += id

    return invalids

print(task01(sys.argv[1]))
print(task02(sys.argv[1]))