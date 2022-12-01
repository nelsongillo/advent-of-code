import re


def find_bag(graph, target):
    bags = set()
    for main in graph:
        # print(main)
        for sub in graph[main]:
            if sub == target:
                bags.add(main)
                bags.update(find_bag(graph, main))

    return bags


# Bags containing other
main_bag = re.compile(r'[a-z]+ [a-z]+')
# bags conatined by main
content_bags = re.compile(r'(\d+) ([a-z ]+) bags?')

input = "data.txt"

file = open(input, "r")
lines = file.readlines()

bags = dict()
for line in lines:
    main = main_bag.match(line).group()
    bags[main] = dict()
    if "no other" not in line:
        for n, color in content_bags.findall(line):
            bags[main][color] = int(n)


target = "shiny gold"
puzzle01 = len(find_bag(bags, target))

print("Puzzle 01: {}".format(puzzle01))
